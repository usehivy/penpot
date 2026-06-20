;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.routes
  (:require
   [app.common.data.macros :as dm]
   [app.common.uri :as u]
   [app.common.uuid :as uuid]
   [app.config :as cf]
   [app.main.errors :as errors]
   [app.main.features :as features]
   [app.main.repo :as rp]
   [app.main.router :as rt]
   [app.main.store :as st]
   [app.util.storage :as storage]
   [beicon.v2.core :as rx]
   [cuerdas.core :as str]
   [potok.v2.core :as ptk]))

(def routes
  (cond-> [["/frame-preview" :frame-preview]

           ["/view" :viewer]

           ["/view/:file-id" :viewer-legacy]

           ;; Used for export
           ["/render-sprite/:file-id" :render-sprite]

           ["/workspace" :workspace]
           ["/workspace/:project-id/:file-id" :workspace-legacy]]
    (contains? cf/flags :nitrate)
    (conj ["/subscribe-nitrate" :nitrate-entry])

    *assert*
    (conj ["/debug/icons-preview" :debug-icons-preview])

    *assert*
    (conj ["/debug/playground" :debug-playground])))


(defn- store-session-params
  [{:keys [template plugin]}]
  (binding [storage/*sync* true]
    (when (some? template)
      (swap! storage/session assoc
             :template template))
    (when (some? plugin)
      (swap! storage/session assoc
             :plugin-url plugin))))

(defn- check-sso-and-navigate
  "Authorization filter for dashboard and workspace routes.
  Checks if the team being navigated to has an organization with SSO
  active. If so, calls :check-nitrate-sso and either proceeds with navigation
  or redirects to the SSO provider URL."
  [match send-event-info? url]
  (let [route-name     (name (get-in match [:data :name]))
        relevant?      (and (contains? cf/flags :nitrate)
                            (or (str/starts-with? route-name "dashboard")
                                (str/starts-with? route-name "workspace")))
        team-id-str    (when relevant?
                         (or (get-in match [:query-params :team-id])
                             (get-in match [:params :path :team-id])))
        team-id        (some-> team-id-str uuid/parse*)]
    (if (some? team-id)
      (->> (rp/cmd! :check-nitrate-sso {:team-id team-id :url url})
           (rx/subs!
            (fn [{:keys [authorized redirect-uri]}]
              (if authorized
                (st/emit! (rt/navigated match send-event-info?))
                (when redirect-uri (st/emit! (rt/nav-raw :uri (str redirect-uri))))))
            (fn [cause]
              (errors/on-error cause))))
      (st/emit! (rt/navigated match send-event-info?)))))

(defn on-navigate
  [router path send-event-info?]
  (let [location        (.-location js/document)
        [base-path qs]  (str/split path "?")
        location-path   (dm/str (.-origin location) (.-pathname location))
        valid-location? (= location-path (dm/str cf/public-uri))
        match           (rt/match router path)
        empty-path?     (or (= base-path "") (= base-path "/"))
        query-params    (u/query-string->map qs)]

    (cond
      (not valid-location?)
      (st/emit! (rt/assign-exception {:type :not-found}))

      (some? match)
      (check-sso-and-navigate match send-event-info? (rt/get-current-href))

      :else
      (do
        (when empty-path?
          (store-session-params query-params))
        (st/emit! (rt/assign-exception {:type :not-found}))))))

(defn init-routes
  []
  (ptk/reify ::init-routes
    ptk/WatchEvent
    (watch [_ _ stream]
      (rx/merge
       (rx/of (rt/initialize-router routes)
              (rt/initialize-history on-navigate))
       (->> stream
            (rx/filter (ptk/type? ::rt/navigated))
            (rx/map deref)
            (rx/map #(dm/get-in % [:query-params :wasm]))
            (rx/buffer 2 1)
            (rx/filter (fn [[v1 v2]] (not= v1 v2)))
            (rx/map features/recompute-features))))))
