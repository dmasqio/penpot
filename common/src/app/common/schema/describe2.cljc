;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.schema.describe2
  (:require
   [app.common.data :as d]
   [app.common.exceptions :as ex]
   [app.common.schema :as sm]
   [cuerdas.core :as str]
   [malli.core :as m]
   [malli.util :as mu]))

(declare describe*)

(defmulti visit (fn [name _schema _children _options] name) :default ::default)

(defmethod visit ::default [name schema children options]
  (m/form schema options))

(defmethod visit :vector [name schema children options]
  (apply vector :vector children))

(defmethod visit :map [name schema children options]
  (let [childs (map (fn [[k p c :as entry]]
                      (if (nil? p)
                        [k c]
                        entry))
                    children)
        props  (m/properties schema)
        params (cond->> childs
                 (some? props)
                 (cons props))]

    (apply vector :map params)))

(defmethod visit :merge [_ schema children options]
  (apply vector :merge children))

(defmethod visit :schema [_ schema children options]
  (visit ::m/schema schema children options))

(defmethod visit ::m/val [_ schema children options]
  (last children))

(defmethod visit ::m/schema [_ schema children options]

  (let [schema' (m/deref schema)
        level   (::level options)]

    (if (< level 1000)
      (let [res (describe* schema' (update options ::level inc))]
        res)
      (last children))))

    ;; #_(describe* schema' options)))

(defn describe* [s options]
  (letfn [(walk-fn [schema path children options]
            ;; (prn ">" (m/type schema) children)
            (let [result (visit (m/type schema) schema  children options)]
              ;; (prn "<" (m/type schema) result)
              result))]
    (m/walk s walk-fn options)))

(defn describe
  "Given a schema, returns a string explaiaing the required shape in English"
  ([s]
   (describe s nil))
  ([s options]
   (let [
         defs    (atom (d/ordered-set))
         s       (sm/schema s)
         s       (cond-> s
                   (= (m/type s) ::m/schema)
                   (m/deref))
         options (assoc options ::m/walk-entry-vals true ::level 0)]
     (describe* s options))))
