;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.rpc
  (:require
   [app.auth.ldap :as-alias ldap]
   [app.common.data :as d]
   [app.common.exceptions :as ex]
   [app.common.logging :as l]
   [app.common.spec :as us]
   [app.config :as cf]
   [app.db :as db]
   [app.http :as-alias http]
   [app.http.access-token :as actoken]
   [app.http.client :as-alias http.client]
   [app.http.session :as session]
   [app.loggers.audit :as audit]
   [app.main :as-alias main]
   [app.metrics :as mtx]
   [app.msgbus :as-alias mbus]
   [app.rpc.climit :as climit]
   [app.rpc.cond :as cond]
   [app.rpc.helpers :as rph]
   [app.rpc.retry :as retry]
   [app.rpc.rlimit :as rlimit]
   [app.storage :as-alias sto]
   [app.util.services :as sv]
   [app.util.time :as dt]
   [app.worker :as-alias wrk]
   [clojure.spec.alpha :as s]
   [integrant.core :as ig]
   [promesa.core :as p]
   [yetti.request :as yrq]
   [yetti.response :as yrs]))

(s/def ::profile-id ::us/uuid)

(defn- default-handler
  [_]
  (p/rejected (ex/error :type :not-found)))

(defn- handle-response-transformation
  [response request mdata]
  (reduce (fn [response transform-fn]
            (transform-fn request response))
          response
          (::response-transform-fns mdata)))

(defn- handle-before-comple-hook
  [response mdata]
  (doseq [hook-fn (::before-complete-fns mdata)]
    (ex/ignoring (hook-fn)))
  response)

(defn- handle-response
  [request result]
  (if (fn? result)
    (result request)
    (let [mdata (meta result)]
      (-> {::yrs/status  (::http/status mdata 200)
           ::yrs/headers (::http/headers mdata {})
           ::yrs/body    (rph/unwrap result)}
          (handle-response-transformation request mdata)
          (handle-before-comple-hook mdata)))))

(defn- rpc-query-handler
  "Ring handler that dispatches query requests and convert between
  internal async flow into ring async flow."
  [methods {:keys [params path-params] :as request}]
  (let [type       (keyword (:type path-params))
        profile-id (or (::session/profile-id request)
                       (::actoken/profile-id request))

        data       (-> params
                       (assoc ::request-at (dt/now))
                       (assoc ::http/request request))
        data       (if profile-id
                     (-> data
                         (assoc :profile-id profile-id)
                         (assoc ::profile-id profile-id))
                     (dissoc data :profile-id ::profile-id))
        method     (get methods type default-handler)
        response   (method data)]
    (handle-response request response)))

(defn- rpc-mutation-handler
  "Ring handler that dispatches mutation requests and convert between
  internal async flow into ring async flow."
  [methods {:keys [params path-params] :as request}]
  (let [type       (keyword (:type path-params))
        profile-id (or (::session/profile-id request)
                       (::actoken/profile-id request))
        data       (-> params
                       (assoc ::request-at (dt/now))
                       (assoc ::http/request request))
        data       (if profile-id
                     (-> data
                         (assoc :profile-id profile-id)
                         (assoc ::profile-id profile-id))
                     (dissoc data :profile-id))
        method     (get methods type default-handler)
        response   (method data)]
    (handle-response request response)))

(defn- rpc-command-handler
  "Ring handler that dispatches cmd requests and convert between
  internal async flow into ring async flow."
  [methods {:keys [params path-params] :as request}]
  (let [type       (keyword (:type path-params))
        etag       (yrq/get-header request "if-none-match")
        profile-id (or (::session/profile-id request)
                       (::actoken/profile-id request))
        data       (-> params
                       (assoc ::request-at (dt/now))
                       (assoc ::session/id (::session/id request))
                       (assoc ::http/request request)
                       (assoc ::cond/key etag)
                       (cond-> (uuid? profile-id)
                         (assoc ::profile-id profile-id)))

        method    (get methods type default-handler)]

    (binding [cond/*enabled* true]
      (let [response (method data)]
        (handle-response request response)))))

(defn- wrap-metrics
  "Wrap service method with metrics measurement."
  [{:keys [metrics ::metrics-id]} f mdata]
  (let [labels (into-array String [(::sv/name mdata)])]
    (fn [cfg params]
      (let [tp (dt/tpoint)]
        (try
          (f cfg params)
          (finally
            (mtx/run! metrics
                      :id metrics-id
                      :val (inst-ms (tp))
                      :labels labels)))))))

(defn- wrap-authentication
  [_ f mdata]
  (fn [cfg params]
    (let [profile-id (::profile-id params)]
      (if (and (::auth mdata true) (not (uuid? profile-id)))
        (ex/raise :type :authentication
                  :code :authentication-required
                  :hint "authentication required for this endpoint")
        (f cfg params)))))

(defn- wrap-access-token
  "Wraps service method with access token validation."
  [_ f {:keys [::sv/name] :as mdata}]
  (if (contains? cf/flags :access-tokens)
    (fn [cfg params]
      (let [request (::http/request params)]
        (if (contains? request ::actoken/id)
          (let [perms (::actoken/perms request #{})]
            (if (contains? perms name)
              (f cfg params)
              (ex/raise :type :authorization
                        :code :operation-not-allowed
                        :allowed perms)))
          (f cfg params))))
    f))

(defn- wrap-audit
  [_ f mdata]
  (if (or (contains? cf/flags :webhooks)
          (contains? cf/flags :audit-log))
    (if-not (::audit/skip mdata)
      (fn [cfg params]
        (let [result (f cfg params)]
          (->> (audit/prepare-event cfg mdata params result)
               (audit/submit! cfg))
          result))
      f)
    f))

(defn- wrap-spec-conform
  [_ f mdata]
  (let [spec (or (::sv/spec mdata) (s/spec any?))]
    (fn [cfg params]
      (f cfg (us/conform spec params)))))

(defn- wrap-all
  [cfg f mdata]
  (as-> f $
    (wrap-metrics cfg $ mdata)
    (cond/wrap cfg $ mdata)
    (retry/wrap-retry cfg $ mdata)
    (climit/wrap cfg $ mdata)
    (rlimit/wrap cfg $ mdata)
    (wrap-audit cfg $ mdata)
    (wrap-spec-conform cfg $ mdata)
    (wrap-authentication cfg $ mdata)
    (wrap-access-token cfg $ mdata)))

(defn- wrap
  [cfg f mdata]
  (l/debug :hint "register method" :name (::sv/name mdata))
  (let [f (wrap-all cfg f mdata)]
    (partial f cfg)))

(defn- process-method
  [cfg [vfn mdata]]
  [(keyword (::sv/name mdata)) [mdata (wrap cfg vfn mdata)]])

(defn- resolve-query-methods
  [cfg]
  (let [cfg (assoc cfg ::type "query" ::metrics-id :rpc-query-timing)]
    (->> (sv/scan-ns
          'app.rpc.queries.projects
          'app.rpc.queries.profile
          'app.rpc.queries.viewer
          'app.rpc.queries.fonts)
         (map (partial process-method cfg))
         (into {}))))

(defn- resolve-mutation-methods
  [cfg]
  (let [cfg (assoc cfg ::type "mutation" ::metrics-id :rpc-mutation-timing)]
    (->> (sv/scan-ns
          'app.rpc.mutations.media
          'app.rpc.mutations.profile
          'app.rpc.mutations.projects
          'app.rpc.mutations.fonts
          'app.rpc.mutations.share-link)
         (map (partial process-method cfg))
         (into {}))))

(defn- resolve-command-methods
  [cfg]
  (let [cfg (assoc cfg ::type "command" ::metrics-id :rpc-command-timing)]
    (->> (sv/scan-ns
          'app.rpc.commands.access-token
          'app.rpc.commands.audit
          'app.rpc.commands.auth
          'app.rpc.commands.feedback
          'app.rpc.commands.fonts
          'app.rpc.commands.binfile
          'app.rpc.commands.comments
          'app.rpc.commands.demo
          'app.rpc.commands.files
          'app.rpc.commands.files-create
          'app.rpc.commands.files-share
          'app.rpc.commands.files-temp
          'app.rpc.commands.files-update
          'app.rpc.commands.ldap
          'app.rpc.commands.management
          'app.rpc.commands.media
          'app.rpc.commands.profile
          'app.rpc.commands.projects
          'app.rpc.commands.search
          'app.rpc.commands.teams
          'app.rpc.commands.verify-token
          'app.rpc.commands.viewer
          'app.rpc.commands.webhooks)
         (map (partial process-method cfg))
         (into {}))))

(defmethod ig/pre-init-spec ::methods [_]
  (s/keys :req [::session/manager
                ::http.client/client
                ::db/pool
                ::mbus/msgbus
                ::ldap/provider
                ::sto/storage
                ::mtx/metrics
                ::main/props
                ::wrk/executor]
          :opt [::climit
                ::rlimit]
          :req-un [::db/pool]))

(defmethod ig/init-key ::methods
  [_ cfg]
  (let [cfg (d/without-nils cfg)]
    {:mutations (resolve-mutation-methods cfg)
     :queries   (resolve-query-methods cfg)
     :commands  (resolve-command-methods cfg)}))

(s/def ::mutations
  (s/map-of keyword? (s/tuple map? fn?)))

(s/def ::queries
  (s/map-of keyword? (s/tuple map? fn?)))

(s/def ::commands
  (s/map-of keyword? (s/tuple map? fn?)))

(s/def ::methods
  (s/keys :req-un [::mutations
                   ::queries
                   ::commands]))

(s/def ::routes vector?)

(defmethod ig/pre-init-spec ::routes [_]
  (s/keys :req [::methods
                ::db/pool
                ::main/props
                ::wrk/executor
                ::session/manager]))

(defmethod ig/init-key ::routes
  [_ {:keys [::methods] :as cfg}]
  (let [methods (-> methods
                    (update :commands update-vals peek)
                    (update :queries update-vals peek)
                    (update :mutations update-vals peek))]
    [["/rpc" {:middleware [[session/authz cfg]
                           [actoken/authz cfg]]}
      ["/command/:type" {:handler (partial rpc-command-handler (:commands methods))}]
      ["/query/:type" {:handler (partial rpc-query-handler (:queries methods))}]
      ["/mutation/:type" {:handler (partial rpc-mutation-handler (:mutations methods))
                          :allowed-methods #{:post}}]]]))

