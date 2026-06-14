;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.workspace.left-header
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.main.data.workspace :as dw]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.icons :as deprecated-icon]
   [app.main.ui.workspace.main-menu :as main-menu]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

;; --- Header Component

(mf/defc left-header*
  [{:keys [file layout project class]}]
  (let [file-id     (:id file)
        file-name   (:name file)
        shared?     (:is-shared file)
        persistence
        (mf/deref refs/persistence)

        persistence-status
        (get persistence :status)

        editing*    (mf/use-state false)
        editing?    (deref editing*)
        input-ref   (mf/use-ref nil)

        handle-blur
        (mf/use-fn
         (mf/deps file-id)
         (fn [_]
           (let [value (str/trim (-> input-ref mf/ref-val dom/get-value))]
             (when (not= value "")
               (st/emit! (dw/rename-file file-id value)))
             (reset! editing* false))))

        handle-name-keydown
        (mf/use-fn
         (mf/deps handle-blur)
         (fn [event]
           (when (kbd/enter? event)
             (handle-blur event))))

        start-editing-name
        (mf/use-fn
         (fn [event]
           (dom/prevent-default event)
           (reset! editing* true)))]

    (mf/with-effect [editing?]
      (when ^boolean editing?
        (dom/select-text! (mf/ref-val input-ref))))

    [:header {:class (dm/str class " " (stl/css :workspace-header-left))}
     [:div {:alt (tr "workspace.sitemap")
            :class (stl/css :project-tree)}
      [:div
       {:class (stl/css :project-name)}
       (:name project)]
      (if ^boolean editing?
        [:input
         {:class (stl/css :file-name-input)
          :type "text"
          :ref input-ref
          :on-blur handle-blur
          :on-key-down handle-name-keydown
          :auto-focus true
          :default-value (:name file "")}]
        [:div
         {:class (stl/css :file-name)
          :title file-name
          :on-double-click start-editing-name}
         ;;-- Persistende state widget
         [:div {:class (case persistence-status
                         :pending (stl/css :status-notification :pending-status)
                         :saving (stl/css :status-notification :saving-status)
                         :saved (stl/css :status-notification :saved-status)
                         :error (stl/css :status-notification :error-status)
                         (stl/css :status-notification))
                :title (case persistence-status
                         :pending (tr "workspace.header.saving")
                         :saving (tr "workspace.header.saving")
                         :saved (tr "workspace.header.saved")
                         :error (tr "workspace.header.save-error")
                         nil)}
          (case persistence-status
            :pending deprecated-icon/status-alert
            :saving deprecated-icon/status-alert
            :saved deprecated-icon/status-tick
            :error deprecated-icon/status-wrong
            nil)]
         [:div {:class (stl/css :file-name-label)} file-name]])]
     (when ^boolean shared?
       [:span {:class (stl/css :shared-badge)} deprecated-icon/library])
     [:div {:class (stl/css :menu-section)}
      [:> main-menu/menu* {:layout layout
                           :file file}]]]))
