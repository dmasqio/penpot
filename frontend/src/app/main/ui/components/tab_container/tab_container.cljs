;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.components.tab-container.tab-container
  (:require-macros [app.main.style :refer [css styles]])
  (:require
   [app.common.data :as d]
   [app.main.ui.icons :as i]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(mf/defc tab-element
  {::mf/wrap-props false}
  [props]
  (let [children (unchecked-get props "children")]
    [:div.tab-element
     [:div.tab-element-content children]]))

(mf/defc tab-container
  {::mf/wrap-props false}
  [props]
  (let [children  (->>
                   (unchecked-get props "children")
                   (filter some?))
        selected  (unchecked-get props "selected")
        on-change (unchecked-get props "on-change-tab")
        collapsable? (unchecked-get props "collapsable?")
        handle-collapse (unchecked-get props "handle-collapse")

        state     (mf/use-state #(or selected (-> children first .-props .-id)))
        selected  (or selected @state)

        select-fn
        (mf/use-fn
         (mf/deps on-change)
         (fn [event]
           (let [id (d/read-string (.. event -target -dataset -id))]
             (reset! state id)
             (when (fn? on-change) (on-change id)))))]

    [:div {:class (dom/classnames (css :tab-container) true)}
     [:div {:class (dom/classnames (css :tab-container-tabs) true)}
      (when collapsable?
        [:button
         {:on-click handle-collapse
          :class (dom/classnames (css :collapse-sidebar) true)
          :aria-label (tr "workspace.sidebar.collapse")}
         i/arrow-refactor])
      (for [tab children]
        (let [props (.-props tab)
              id    (.-id props)
              title (.-title props)]
          [:div
           {:key (str/concat "tab-" (d/name id))
            :data-id (pr-str id)
            :on-click select-fn
            :class (dom/classnames (css :tab-container-tab-title) true
                                   (css :current) (= selected id))}
           title]))]
     [:div.tab-container-content
      (d/seek #(= selected (-> % .-props .-id)) children)]]))
