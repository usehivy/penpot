;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui
  (:require
   [app.common.data :as d]
   [app.common.uuid :as uuid]
   [app.config :as cf]
   [app.main.data.common :as dcm]
   [app.main.data.nitrate :as dnt]
   [app.main.data.team :as dtm]
   [app.main.errors :as errors]
   [app.main.refs :as refs]
   [app.main.repo :as rp]
   [app.main.router :as rt]
   [app.main.store :as st]
   [app.main.ui.context :as ctx]
   [app.main.ui.debug.icons-preview :refer [icons-preview*]]
   [app.main.ui.debug.playground :refer [playground*]]
   [app.main.ui.ds.product.loader :refer [loader*]]
   [app.main.ui.error-boundary :refer [error-boundary*]]
   [app.main.ui.exports.files]
   [app.main.ui.frame-preview :as frame-preview]
   [app.main.ui.nitrate.entry :as nitrate-entry]
   [app.main.ui.notifications :as notifications]
   [app.main.ui.onboarding.questions :refer [questions-modal]]
   [app.main.ui.onboarding.team-choice :refer [onboarding-team-modal*]]
   [app.main.ui.releases :refer [release-notes-modal]]
   [app.main.ui.static :as static]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.modules :as mod]
   [app.util.theme :as theme]
   [beicon.v2.core :as rx]
   [rumext.v2 :as mf]))

(def viewer-page*
  (mf/lazy #(mod/load 'app.main.ui.viewer/viewer-page*)))

(def workspace-page*
  (mf/lazy #(mod/load 'app.main.ui.workspace/workspace-page*)))

(mf/defc workspace-legacy-redirect*
  {::mf/props :obj
   ::mf/private true}
  [{:keys [project-id file-id page-id layout]}]
  (mf/with-effect []
    (->> (rp/cmd! :get-project {:id project-id})
         (rx/subs! (fn [{:keys [team-id]}]
                     (st/emit! (dcm/go-to-workspace :team-id team-id
                                                    :file-id file-id
                                                    :page-id page-id
                                                    :layout layout)))
                   errors/on-error)))
  [:> loader*
   {:title (tr "labels.loading")
    :overlay true}])

(mf/defc viewer-legacy-redirect*
  {::mf/props :obj
   ::mf/private true}
  [{:keys [page-id file-id section index share-id interactions-mode frame-id share]}]
  (mf/with-effect []
    (let [params {:page-id page-id
                  :file-id file-id
                  :section section
                  :index index
                  :share-id share-id
                  :interactions-mode interactions-mode
                  :frame-id frame-id
                  :share share}]
      (st/emit! (rt/nav :viewer (d/without-nils params)))))

  [:> loader*
   {:title (tr "labels.loading")
    :overlay true}])

(mf/defc team-container*
  {::mf/props :obj
   ::mf/private true}
  [{:keys [team-id children]}]
  (mf/with-effect [team-id]
    (st/emit! (dtm/initialize-team team-id))
    (fn []
      (st/emit! (dtm/finalize-team team-id))))

  (let [{:keys [permissions] :as team} (mf/deref refs/team)]
    (when (= team-id (:id team))
      [:> (mf/provider ctx/current-team-id) {:value team-id}
       [:> (mf/provider ctx/permissions) {:value permissions}
        [:> (mf/provider ctx/can-edit?) {:value (:can-edit permissions)}
         ;; The `:key` is mandatory here because we want to reinitialize
         ;; all dom tree instead of simple rerender.
         [:* {:key (str team-id)} children]]]])))

(mf/defc page*
  {::mf/props :obj
   ::mf/private true}
  [{:keys [route profile]}]
  (let [{:keys [data params]} route
        props   (get profile :props)
        section (get data :name)
        team    (mf/deref refs/team)
        nitrate-entry-active? (dnt/nitrate-entry-active?)


        show-question-modal?
        (and (contains? cf/flags :onboarding)
             (not nitrate-entry-active?)
             (not (:onboarding-viewed props))
             (not (contains? props :onboarding-questions)))

        show-team-modal?
        (and (contains? cf/flags :onboarding)
             (not nitrate-entry-active?)
             (not (:onboarding-viewed props))
             (not (contains? props :onboarding-team-id))
             (:is-default team))

        show-release-modal?
        (and (contains? cf/flags :onboarding)
             (not nitrate-entry-active?)
             (not (contains? cf/flags :hide-release-modal))
             (:onboarding-viewed props)
             (not= (:release-notes-viewed props) (:main cf/version))
             (not= "0.0" (:main cf/version)))]

    [:& (mf/provider ctx/current-route) {:value route}
     (case section
       :nitrate-entry
       [:> nitrate-entry/nitrate-entry-page* {:profile profile}]

       :debug-icons-preview
       (when *assert*
         [:> icons-preview*])

       :debug-playground
       (when *assert*
         [:> playground*])

       :workspace
       (let [params     (get params :query)
             team-id    (some-> params :team-id uuid/parse*)
             file-id    (some-> params :file-id uuid/parse*)
             page-id    (some-> params :page-id uuid/parse*)
             layout     (some-> params :layout keyword)]
         [:? {}
          (when (cf/external-feature-flag "onboarding-03" "test")
            (cond
              show-question-modal?
              [:& questions-modal]

              show-team-modal?
              [:> onboarding-team-modal* {:go-to-team false}]

              show-release-modal?
              [:& release-notes-modal {:version (:main cf/version)}]))

          [:> team-container* {:team-id team-id}
           [:> workspace-page* {:team-id team-id
                                :file-id file-id
                                :page-id page-id
                                :layout-name layout
                                :key file-id}]]])

       :viewer
       (let [params   (get params :query)
             index    (some-> (rt/get-query-param params :index) parse-long)
             share-id (some-> (:share-id params) uuid/parse*)
             section  (or (some-> (:section params) keyword)
                          :interactions)

             file-id  (some-> (:file-id params) uuid/parse*)
             page-id  (some-> (:page-id params) uuid/parse*)
             imode    (or (some-> (:interactions-mode params) keyword)
                          :show-on-click)
             frame-id (some-> (:frame-id params) uuid/parse*)
             share    (:share params)]

         [:? {}
          [:> viewer-page*
           {:page-id page-id
            :file-id file-id
            :frame-id frame-id
            :section section
            :index index
            :share-id share-id
            :interactions-mode imode
            :share share}]])


       :workspace-legacy
       (let [project-id (some-> params :path :project-id uuid/parse*)
             file-id    (some-> params :path :file-id uuid/parse*)
             page-id    (some-> params :query :page-id uuid/parse*)
             layout     (some-> params :query :layout keyword)]

         [:> workspace-legacy-redirect*
          {:project-id project-id
           :file-id file-id
           :page-id page-id
           :layout layout}])

       :viewer-legacy
       (let [{:keys [query-params path-params]} route
             {:keys [index share-id section page-id interactions-mode frame-id share]
              :or {section :interactions interactions-mode :show-on-click}} query-params
             {:keys [file-id]} path-params]

         [:> viewer-legacy-redirect*
          {:page-id page-id
           :file-id file-id
           :section section
           :index index
           :share-id share-id
           :interactions-mode (keyword interactions-mode)
           :frame-id frame-id
           :share share}])

       :frame-preview
       [:> frame-preview/frame-preview*]

       nil)]))

(mf/defc app
  []
  (let [route   (mf/deref refs/route)
        edata   (mf/deref refs/exception)
        profile (mf/deref refs/profile)]

    ;; initialize themes
    (theme/use-initialize profile)

    (dom/prevent-browser-gesture-navigation!)

    [:& (mf/provider ctx/current-route) {:value route}
     [:& (mf/provider ctx/current-profile) {:value profile}
      (if edata
        [:> static/exception-page* {:data edata :route route}]
        [:> error-boundary* {:fallback static/exception-page*}
         [:> notifications/current-notification*]
         (when route
           [:> page* {:route route :profile profile}])])]]))
