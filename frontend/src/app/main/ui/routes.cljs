;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.routes
  (:require
   [app.common.data.macros :as dm]
   [app.common.uri :as u]
   [app.config :as cf]
   [app.main.features :as features]
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
      (st/emit! (rt/navigated match send-event-info?))

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
