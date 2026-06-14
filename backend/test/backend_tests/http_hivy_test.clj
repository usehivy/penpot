;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns backend-tests.http-hivy-test
  (:require
   [app.common.time :as ct]
   [app.config :as cf]
   [app.db :as db]
   [app.http.hivy :as hivy]
   [app.rpc.commands.profile :as cmd.profile]
   [backend-tests.helpers :as th]
   [buddy.sign.jwt :as jwt]
   [clojure.string :as str]
   [clojure.test :as t]
   [yetti.request :as yreq]
   [yetti.response :as-alias yres]))

(t/use-fixtures :once th/state-init)
(t/use-fixtures :each th/database-reset)

(def ^:private hivy-key "hivy-test-secret")

(defrecord DummyRequest [params headers]
  yreq/IRequest
  (get-header [_ name]
    (get headers name)))

(defmacro with-hivy-config
  [& body]
  `(binding [cf/config (assoc cf/config
                              :hivy-control-plane-key hivy-key
                              :public-uri "https://canvas.usehivy.com")]
     ~@body))

(defn- call!
  [handler params]
  (db/tx-run! th/*system*
              (fn [cfg]
                (handler cfg {:params params}))))

(defn- call-with-request!
  [handler request]
  (db/tx-run! th/*system*
              (fn [cfg]
                (handler cfg request))))

(defn- create-hivy-team!
  []
  (let [team-id (th/mk-uuid "hivy-team" 1)]
    (call! #'hivy/upsert-team
           {:team-id team-id
            :hivy-id "org-1"
            :name "Hivy Org"})
    team-id))

(defn- create-hivy-profile!
  [team-id]
  (let [profile-id (th/mk-uuid "hivy-profile" 1)]
    (call! #'hivy/upsert-profile
           {:profile-id profile-id
            :team-id team-id
            :hivy-id "user-1-org-1"
            :email "designer@example.com"
            :fullname "Designer"})
    profile-id))

(t/deftest control-plane-auth
  (with-hivy-config
    (let [middleware ((:compile @#'hivy/control-plane-auth) nil nil)
          handler    (middleware (fn [_] {::yres/status 200}) hivy-key)]
      (t/is (= 403 (::yres/status (handler (->DummyRequest {} {})))))
      (t/is (= 403 (::yres/status (handler (->DummyRequest {} {"authorization" "Bearer wrong"})))))
      (t/is (= 200 (::yres/status (handler (->DummyRequest {} {"authorization" (str "Bearer " hivy-key)}))))))))

(t/deftest team-upsert-creates-team-and-quotas
  (let [team-id  (th/mk-uuid "hivy-team" 1)
        response (call! #'hivy/upsert-team
                        {:team-id team-id
                         :hivy-id "org-1"
                         :name "Hivy Org"})
        team     (th/db-get :team {:id team-id})
        quote    (th/db-get :usage-quote {:target "projects-per-team"
                                          :team-id team-id})]
    (t/is (= 200 (::yres/status response)))
    (t/is (= "org-1" (:hivy-id team)))
    (t/is (= team-id (-> response ::yres/body :team-id)))
    (t/is (some? (-> response ::yres/body :default-project-id)))
    (t/is (= Long/MAX_VALUE (:quote quote)))))

(t/deftest profile-upsert-adds-admin-enables-mcp-and-raises-quotas
  (with-hivy-config
    (let [team-id    (create-hivy-team!)
          profile-id (th/mk-uuid "hivy-profile" 1)
          params     {:profile-id profile-id
                      :team-id team-id
                      :hivy-id "user-1-org-1"
                      :email "designer@example.com"
                      :fullname "Designer"}
          response-1 (call! #'hivy/upsert-profile params)
          response-2 (call! #'hivy/upsert-profile params)
          profile    (-> (th/db-get :profile {:id profile-id})
                         (cmd.profile/decode-row))
          rel        (th/db-get :team-profile-rel {:team-id team-id
                                                   :profile-id profile-id})
          quote      (th/db-get :usage-quote {:target "access-tokens-per-profile"
                                              :profile-id profile-id})
          tokens     (th/db-query :access-token {:profile-id profile-id
                                                 :type "mcp"})]
      (t/is (= 200 (::yres/status response-1)))
      (t/is (= (:mcp-token (::yres/body response-1))
               (:mcp-token (::yres/body response-2))))
      (t/is (= "hivy" (:auth-backend profile)))
      (t/is (true? (:is-active profile)))
      (t/is (= "user-1-org-1" (get-in profile [:props :hivy-id])))
      (t/is (true? (get-in profile [:props :mcp-enabled])))
      (t/is (true? (:is-admin rel)))
      (t/is (true? (:can-edit rel)))
      (t/is (= Long/MAX_VALUE (:quote quote)))
      (t/is (= 1 (count tokens))))))

(t/deftest project-and-file-upsert-create-valid-rows-and-quotas
  (let [team-id    (create-hivy-team!)
        profile-id (create-hivy-profile! team-id)
        project-id (th/mk-uuid "hivy-project" 1)
        file-id    (th/mk-uuid "hivy-file" 1)
        project    (call! #'hivy/upsert-project
                          {:project-id project-id
                           :team-id team-id
                           :name "Landing Page"})
        file       (call! #'hivy/upsert-file
                          {:file-id file-id
                           :project-id project-id
                           :profile-id profile-id
                           :name "Hero Concepts"})
        pquote     (th/db-get :usage-quote {:target "files-per-project"
                                            :project-id project-id})
        fquote     (th/db-get :usage-quote {:target "snapshots-per-file"
                                            :file-id file-id})]
    (t/is (= 200 (::yres/status project)))
    (t/is (= 200 (::yres/status file)))
    (t/is (= team-id (-> project ::yres/body :team-id)))
    (t/is (= project-id (-> file ::yres/body :project-id)))
    (t/is (= Long/MAX_VALUE (:quote pquote)))
    (t/is (= Long/MAX_VALUE (:quote fquote)))))

(t/deftest session-endpoint-verifies-hivy-jwt-and-creates-cookie
  (with-hivy-config
    (let [team-id    (create-hivy-team!)
          profile-id (create-hivy-profile! team-id)
          project-id (th/mk-uuid "hivy-project" 1)
          file-id    (th/mk-uuid "hivy-file" 1)
          _          (call! #'hivy/upsert-project
                            {:project-id project-id
                             :team-id team-id
                             :name "Landing Page"})
          _          (call! #'hivy/upsert-file
                            {:file-id file-id
                             :project-id project-id
                             :profile-id profile-id
                             :name "Hero Concepts"})
          exp        (quot (+ (inst-ms (ct/now)) 60000) 1000)
          token      (jwt/sign {"iss" "hivy"
                                "aud" "penpot-canvas"
                                "exp" exp
                                "profile_id" (str profile-id)
                                "team_id" (str team-id)
                                "file_id" (str file-id)}
                               hivy-key
                               {:alg :hs256})
          response   (call-with-request! #'hivy/create-session
                                         (->DummyRequest {:token token}
                                                         {"user-agent" "test"}))
          location   (get (::yres/headers response) "location")]
      (t/is (= 302 (::yres/status response)))
      (t/is (contains? (::yres/cookies response) (cf/get :auth-token-cookie-name)))
      (t/is (str/starts-with? location "https://canvas.usehivy.com/#/workspace"))
      (t/is (str/includes? location (str "team-id=" team-id)))
      (t/is (str/includes? location (str "file-id=" file-id)))
      (t/is (= 403 (::yres/status
                    (call-with-request! #'hivy/create-session
                                        (->DummyRequest {:token "bad-token"} {}))))))))
