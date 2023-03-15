;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.debug.components-preview
  (:require-macros [app.main.style :refer [css]])
  (:require [app.main.data.users :as du]
            [app.main.refs :as refs]
            [app.main.store :as st]
            [app.main.ui.components.tests.test-component :as tc]
            [app.util.dom :as dom]
            [rumext.v2 :as mf]))

(mf/defc components-preview
  {::mf/wrap-props false}
  []
  (let [profile (mf/deref refs/profile)
        initial (mf/with-memo [profile]
                  (update profile :lang #(or % "")))
        initial-theme (:theme initial)
        on-change (fn [event]
                    (let [theme (dom/event->value event)
                          data (assoc initial :theme theme)]
                      (st/emit! (du/update-profile data))))

        colors [:bg-primary 
                :bg-secondary 
                :bg-tertiary 
                :bg-cuaternary 
                :fg-primary 
                :fg-secondary 
                :acc 
                :acc-muted 
                :acc-secondary 
                :acc-tertiary]]

    [:section.debug-components-preview
     [:div {:class (css :themes-row)}
      [:h2 "Themes"]
      [:select {:label "Select theme color"
                :name :theme
                :default "default"
                :value initial-theme
                :on-change on-change}
       [:option {:label "Penpot Dark (default)" :value "default"}]
       [:option  {:label "Penpot Light" :value "light"}]]
      [:div {:class (css :wrapper)}
      ;;  (for [color colors]
      ;;    [:div {:class (dom/classnames (css color) true
      ;;                                  (css :rect) true)}
      ;;     (d/name color)])
       [:div {:class (dom/classnames (css :bg-primary) true
                                     (css :rect) true)}
        "background primary"]
       [:div {:class (dom/classnames (css :bg-secondary) true
                                     (css :rect) true)}
        "background-secondary"]
       [:div {:class (dom/classnames (css :bg-tertiary) true
                                     (css :rect) true)}
        "background-tertiary"]
       [:div {:class (dom/classnames (css :bg-cuaternary) true
                                     (css :rect) true)}
        "background-cuaternary"]
       [:div {:class (dom/classnames (css :fg-primary) true
                                     (css :rect) true)}
        "foreground-primary"]
       [:div {:class (dom/classnames (css :fg-secondary) true
                                     (css :rect) true)}
        "foreground-secondary"]
       [:div {:class (dom/classnames (css :acc) true
                                     (css :rect) true)}
        "accent-primary"]
       [:div {:class (dom/classnames (css :acc-muted) true
                                     (css :rect) true)}
        "accent-primary-muted"]
       [:div {:class (dom/classnames (css :acc-secondary) true
                                     (css :rect) true)}
        "accent-secondary"]
       [:div {:class (dom/classnames (css :acc-tertiary) true
                                     (css :rect) true)}
        "accent-tertiary"]]]
     [:div {:class (css :components-row)}
      
      [:& tc/test-component
       {:action #(prn "ey soy un botón") :name "púlsame"}]
      
      ]]))