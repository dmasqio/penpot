;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.schema
  (:refer-clojure :exclude [deref merge])
  #?(:cljs (:require-macros [app.common.schema :refer [ignoring]]))
  (:require
   [app.common.data :as d]
   [app.common.schema.openapi :as-alias oapi]
   [app.common.uuid :as uuid]
   [clojure.test.check.generators :as tgen]
   [cuerdas.core :as str]
   [malli.core :as m]
   [malli.error :as me]
   [malli.generator :as mg]
   [malli.registry :as mr]
   [malli.transform :as mt]
   [malli.util :as mu]))

(def registry (atom {}))

(def default-registry
  (mr/composite-registry
   m/default-registry
   (mu/schemas)
   (mr/mutable-registry registry)))

(defn validate
  [s value]
  (m/validate s value {:registry default-registry}))

(defn explain
  [s value]
  (m/explain s value {:registry default-registry}))

(defn explain-data
  [s value]
  (mu/explain-data s value {:registry default-registry}))

(defn schema?
  [o]
  (m/schema? o))

(defn schema
  [s]
  (m/schema s {:registry default-registry}))

(defn humanize
  [exp]
  (me/humanize exp))

(defn form
  [s]
  (m/form s))

(defn merge
  [& items]
  (apply mu/merge (map schema items)))

(defn ref?
  [s]
  (m/-ref-schema? s))

(defn deref
  [s]
  (m/deref s))

(defn error-values
  [exp]
  (malli.error/error-value exp {:malli.error/mask-valid-values '...}))

(def input-transformer
  (let [default-decoder
        {:compile (fn [s _registry]
                    (let [props (m/type-properties s)]
                      (::decode props)))}
        default-encoder
        {:compile (fn [s _]
                    (let [props (m/type-properties s)]
                      (::encode props)))}

        coders {:vector mt/-sequential-or-set->vector
                :sequential mt/-sequential-or-set->seq
                :set mt/-sequential->set
                :tuple mt/-sequential->vector}]

    (mt/transformer
     {:name :penpot
      :default-decoder default-decoder
      :default-encoder default-encoder}
     {:name :string
      :decoders (mt/-string-decoders)
      :encoders (mt/-string-encoders)}
     {:name :collections
      :decoders coders
      :encoders coders}

     )))

(def output-transformer
  (mt/key-transformer {:encode str/camel
                       :decode (comp keyword str/kebab)}))

(defn validator
  [s]
  (-> s schema m/validator))

(defn explainer
  [s]
  (-> s schema m/explainer))

(defn encode
  ([s val transformer]
   (m/encode s val {:registry default-registry} transformer))
  ([s val options transformer]
   (m/encode s val options transformer)))

(defn decode
  ([s val transformer]
   (m/decode s val {:registry default-registry} transformer))
  ([s val options transformer]
   (m/decode s val options transformer)))

(defn decoder
  ([s transformer]
   (m/decoder s  {:registry default-registry} transformer))
  ([s options transformer]
   (m/decoder s options transformer)))

(defn humanize-data
  [explain-data]
  (let [errors  (humanize explain-data)
        schema  (-> explain-data :schema m/form)
        value   (-> explain-data error-values d/without-qualified)]
    (array-map
     :schema schema
     :value value
     :errors errors)))

(defmacro ignoring
  [expr]
  (if (:ns &env)
    `(try ~expr (catch :default e# nil))
    `(try ~expr (catch Throwable e# nil))))

(defn simple-schema
  [& {:keys [pred] :as options}]
  (cond-> options
    (contains? options :type-properties)
    (update :type-properties (fn [props]
                               (cond-> props
                                 (contains? props :decode/string)
                                 (update :decode/string (fn [decode-fn]
                                                          (fn [s]
                                                            (if (pred s)
                                                              s
                                                              (or (ignoring (decode-fn s)) s)))))
                                 (contains? props ::decode)
                                 (update ::decode (fn [decode-fn]
                                                    (fn [s]
                                                      (if (pred s)
                                                        s
                                                        (or (ignoring (decode-fn s)) s))))))))
    :always
    (m/-simple-schema)))

(defn lookup
  "Lookups schema from registry."
  ([s] (lookup default-registry s))
  ([registry s] (schema (mr/schema registry s))))

(defn generator
  [s]
  (mg/generator s {:registry default-registry}))

(defn generate
  [s]
  (mg/generate (schema s)))

(defmacro assert-schema!
  [& [s value hint]]
  (let [sname   (pr-str s)
        context (if-let [nsdata (:ns &env)]
                  {:ns (str (:name nsdata))
                   :schema sname
                   :line (:line &env)
                   :file (:file (:meta nsdata))}
                  {:ns   (str (ns-name *ns*))
                   :schema sname
                   :line (:line (meta &form))})
        hint    (or hint (str "schema assert: " sname))]

    `(let [v# ~value
           s# (schema ~s)
           ;; s# (if (m/-ref-schema? s#) (m/deref s#) s#)
           ]
       (if (validate s# v#)
         v#
         (throw (ex-info ~hint
                         (into {:type :assertion
                                :code :data-validation
                                :hint ~hint
                                ::explain (explain s# v#)}
                               ~context)))))))


(defmacro assert-expr!
  [& [expr hint]]
  (let [hint (or hint (str "expr assert: " (pr-str expr)))]
    `(when-not ~expr
       (throw (rx-info ~hint
                       {:type :assertion
                        :code :expr-validation
                        :hint ~hint})))))

(defmacro assert!
  [& [expr :as params]]
  (cond
    (or (keyword? expr)
        (vector? expr)
        (symbol? expr))
    (when *assert*
      `(assert-schema! ~@params))

    (list? expr)
    (when *assert*
      `(assert-expr! ~@params))

    :else
    (throw (ex-info "invalid arguments" {}))))

(defmacro verify!
  "A variant of `assert!` macro that evaluates always, independently
  of the *assert* value."
  [& params]
  (binding [*assert* true]
    `(assert! ~@params)))

(defn register! [type s]
  (let [s (if (map? s) (simple-schema s) s)]
    (swap! registry assoc type s)))

(defn def! [type s]
  (register! type s))

(def! :merge (mu/-merge))
(def! :union (mu/-union))

(def uuid-rx
  #"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$")

(def! ::uuid
  {:type ::uuid
   :pred uuid?
   :type-properties
   {:title "uuid"
    :description "UUID formatted string"
    :error/message "should be an uuid"
    :gen/gen (tgen/fmap (fn [_] (uuid/next)) tgen/any)
    ::oapi/type "string"
    ::oapi/format "uuid"
    ::decode #(uuid/uuid (re-matches uuid-rx %))}})

(def non-empty-strings-xf
  (comp
   (filter string?)
   (remove str/empty?)
   (remove str/blank?)))

(def! ::set-of-strings
  (m/-simple-schema
   {:type ::set-of-strings
    :pred #(and (set? %) (every? string? %))
    :type-properties
    {:title "set[type=string]"
     :description "Set of Strings"
     :error/message "should be an set of strings"
     :gen/gen (tgen/set (generator :string))
     ::oapi/type "array"
     ::oapi/format "set"
     ::oapi/items {:type "string"}
     ::oapi/unique-items true
     ::decode (fn [v]
                (into #{} non-empty-strings-xf (str/split v #"[\s,]+")))}}))

(def max-safe-int (int 1e6))
(def min-safe-int (int -1e6))

(def! ::safe-int
  {:type ::safe-int
   :pred #(and (int? %) (>= max-safe-int %) (>= % min-safe-int))
   :type-properties
   {:title "int"
    :description "Safe Integer"
    :error/message "expected to be int in safe range"
    :gen/gen (generator :int)
    ::oapi/type "integer"
    ::oapi/format "int64"
    ::decode parse-long}})

(def! ::safe-number
  {:type ::safe-number
   :pred #(and (number? %) (>= max-safe-int %) (>= % min-safe-int))
   :type-properties
   {:title "number"
    :description "Safe Number"
    :error/message "expected to be number in safe range"
    :gen/gen (generator :int)
    ::oapi/type "number"
    ::oapi/format "double"
    ::decode parse-double}})

;; --- GENERATORS

(defn gen-set-from-choices
  [choices]
  (->> tgen/nat
       (tgen/fmap (fn [i]
                    (into #{}
                          (map (fn [_] (rand-nth choices)))
                          (range i))))))
