;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.atoms.button
  (:require-macros [app.main.style :refer [css]])
  (:require
   [rumext.v2 :as mf]
   [app.util.dom :as dom]))

(mf/defc button
  [{:keys [text disabled]}]
  [:button {:className (dom/classnames
                        (css :kk)      true
                        (css :button)  true
                        (css :disabled) disabled)} text])

