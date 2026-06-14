(ns frontend-tests.ui.routes-test
  (:require
   [app.main.router :as rt]
   [app.main.ui.routes :as routes]
   [cljs.test :as t]))

(t/deftest hivy-canvas-routes
  (let [router (rt/create routes/routes)
        match  #(some-> (rt/match router %) :data :name)]
    (t/is (= :workspace
             (match "/workspace?team-id=eaa65b25-bf83-5da9-8080-bdf3577447dc&file-id=92594769-6637-81b5-8008-2d1a5f9d2e30")))
    (t/is (= :viewer
             (match "/view?file-id=92594769-6637-81b5-8008-2d1a5f9d2e30")))
    (t/is (= :workspace-legacy
             (match "/workspace/ec7f0aa4-8b28-8039-8008-2d187e45387e/92594769-6637-81b5-8008-2d1a5f9d2e30")))

    (t/is (nil? (match "/auth/login")))
    (t/is (nil? (match "/settings/profile")))
    (t/is (nil? (match "/dashboard/recent")))
    (t/is (nil? (match "/dashboard/libraries")))))
