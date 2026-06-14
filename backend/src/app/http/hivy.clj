;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.http.hivy
  "Hivy control-plane API."
  (:require
   [app.common.data :as d]
   [app.common.exceptions :as ex]
   [app.common.features :as cfeat]
   [app.common.schema :as sm]
   [app.common.time :as ct]
   [app.common.uri :as u]
   [app.common.uuid :as uuid]
   [app.config :as cf]
   [app.db :as db]
   [app.http.session :as session]
   [app.rpc.commands.access-token :as cmd.access-token]
   [app.rpc.commands.auth :as cmd.auth]
   [app.rpc.commands.files-create :as cmd.files-create]
   [app.rpc.commands.profile :as cmd.profile]
   [app.rpc.commands.teams :as cmd.teams]
   [app.rpc.permissions :as perms]
   [buddy.sign.jwt :as jwt]
   [clojure.string :as str]
   [integrant.core :as ig]
   [yetti.request :as yreq]
   [yetti.response :as-alias yres]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare upsert-team)
(declare upsert-profile)
(declare upsert-project)
(declare upsert-file)
(declare create-session)

(defmethod ig/assert-key ::routes
  [_ params]
  (assert (db/pool? (::db/pool params)) "expect valid database pool")
  (assert (session/manager? (::session/manager params)) "expect valid session manager"))

(def ^:private default-system
  {:name ::default-system
   :compile
   (fn [_ _]
     (fn [handler cfg]
       (fn [request]
         (handler cfg request))))})

(def ^:private transaction
  {:name ::transaction
   :compile
   (fn [data _]
     (when (:transaction data)
       (fn [handler]
         (fn [cfg request]
           (db/tx-run! cfg handler request)))))})

(def ^:private control-plane-auth
  {:name ::control-plane-auth
   :compile
   (fn [_ _]
     (fn [handler key]
       (if (and (string? key) (not (str/blank? key)))
         (fn [request]
           (let [header (yreq/get-header request "authorization")
                 token  (some->> header (re-matches #"(?i)^Bearer\s+(.+)$") second)]
             (if (= key token)
               (handler request)
               {::yres/status 403})))
         (fn [_]
           {::yres/status 403}))))})

(defmethod ig/init-key ::routes
  [_ cfg]
  ["" {}
   ["/session"
    {:handler create-session
     :middleware [[default-system cfg]
                  [transaction]]
     :transaction true
     :allowed-methods #{:get}}]

   ["" {:middleware [[control-plane-auth (cf/get :hivy-control-plane-key)]
                     [default-system cfg]
                     [transaction]]}
    ["/teams"
     {:handler upsert-team
      :transaction true
      :allowed-methods #{:post}}]

    ["/profiles"
     {:handler upsert-profile
      :transaction true
      :allowed-methods #{:post}}]

    ["/projects"
     {:handler upsert-project
      :transaction true
      :allowed-methods #{:post}}]

    ["/files"
     {:handler upsert-file
      :transaction true
      :allowed-methods #{:post}}]]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schemas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- coercer
  [schema & {:as opts}]
  (let [decode-fn (sm/decoder schema sm/json-transformer)
        check-fn  (sm/check-fn schema opts)]
    (fn [data]
      (-> data decode-fn check-fn))))

(def ^:private schema:team
  [:map {:title "hivy-team"}
   [:team-id ::sm/uuid]
   [:hivy-id ::sm/text]
   [:name ::sm/text]])

(def ^:private schema:profile
  [:map {:title "hivy-profile"}
   [:profile-id ::sm/uuid]
   [:team-id ::sm/uuid]
   [:hivy-id ::sm/text]
   [:email ::sm/email]
   [:fullname ::sm/text]])

(def ^:private schema:project
  [:map {:title "hivy-project"}
   [:project-id ::sm/uuid]
   [:team-id ::sm/uuid]
   [:name ::sm/text]])

(def ^:private schema:file
  [:map {:title "hivy-file"}
   [:file-id ::sm/uuid]
   [:project-id ::sm/uuid]
   [:name ::sm/text]
   [:profile-id {:optional true} ::sm/uuid]])

(def ^:private coerce-team-params (coercer schema:team))
(def ^:private coerce-profile-params (coercer schema:profile))
(def ^:private coerce-project-params (coercer schema:project))
(def ^:private coerce-file-params (coercer schema:file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quotas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private unlimited-quote Long/MAX_VALUE)

(def ^:private profile-quota-targets
  ["teams-per-profile"
   "access-tokens-per-profile"
   "upload-sessions-per-profile"
   "team-access-requests-per-requester"])

(def ^:private team-quota-targets
  ["projects-per-team"
   "font-variants-per-team"
   "invitations-per-team"
   "profiles-per-team"
   "snapshots-per-team"
   "team-access-requests-per-team"])

(def ^:private project-quota-targets
  ["files-per-project"])

(def ^:private file-quota-targets
  ["comment-threads-per-file"
   "comments-per-file"
   "snapshots-per-file"])

(def ^:private sql:update-quote
  "UPDATE usage_quote
      SET quote = ?
    WHERE target = ?
      AND profile_id IS NOT DISTINCT FROM ?::uuid
      AND team_id IS NOT DISTINCT FROM ?::uuid
      AND project_id IS NOT DISTINCT FROM ?::uuid
      AND file_id IS NOT DISTINCT FROM ?::uuid
    RETURNING id")

(defn- upsert-quota!
  [{:keys [::db/conn]} target {:keys [profile-id team-id project-id file-id]}]
  (let [result (db/exec-one! conn [sql:update-quote
                                   unlimited-quote
                                   target
                                   profile-id
                                   team-id
                                   project-id
                                   file-id])]
    (when-not result
      (db/insert! conn :usage-quote
                  (d/without-nils
                   {:target target
                    :quote unlimited-quote
                    :profile-id profile-id
                    :team-id team-id
                    :project-id project-id
                    :file-id file-id})
                  {::db/return-keys false}))))

(defn- raise-profile-quotas!
  [cfg profile-id]
  (doseq [target profile-quota-targets]
    (upsert-quota! cfg target {:profile-id profile-id})))

(defn- raise-team-quotas!
  [cfg team-id]
  (doseq [target team-quota-targets]
    (upsert-quota! cfg target {:team-id team-id})))

(defn- raise-project-quotas!
  [cfg project-id]
  (doseq [target project-quota-targets]
    (upsert-quota! cfg target {:project-id project-id})))

(defn- raise-file-quotas!
  [cfg file-id]
  (doseq [target file-quota-targets]
    (upsert-quota! cfg target {:file-id file-id})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private sql:upsert-team-role
  "INSERT INTO team_profile_rel
     (team_id, profile_id, is_owner, is_admin, can_edit)
   VALUES (?, ?, ?, ?, ?)
   ON CONFLICT (team_id, profile_id)
   DO UPDATE SET is_owner = EXCLUDED.is_owner,
                 is_admin = EXCLUDED.is_admin,
                 can_edit = EXCLUDED.can_edit")

(def ^:private sql:file-team
  "SELECT f.id, f.project_id, p.team_id
     FROM file AS f
    INNER JOIN project AS p ON (p.id = f.project_id)
    WHERE f.id = ?
      AND f.deleted_at IS NULL
      AND p.deleted_at IS NULL")

(defn- upsert-team-role!
  [{:keys [::db/conn]} profile-id team-id role]
  (let [{:keys [is-owner is-admin can-edit]}
        (perms/assign-role-flags {} role)]
    (db/exec-one! conn [sql:upsert-team-role
                        team-id profile-id is-owner is-admin can-edit])))

(defn- ensure-default-project!
  [{:keys [::db/conn]} team-id]
  (or (db/get* conn :project {:team-id team-id
                              :is-default true})
      (let [project (cmd.teams/create-project conn {:team-id team-id
                                                    :name "Drafts"
                                                    :is-default true})]
        project)))

(defn- create-team!
  [{:keys [::db/conn]} {:keys [team-id hivy-id name]}]
  (let [features (->> (cfeat/get-enabled-features cf/flags)
                      (db/create-array conn "text"))]
    (db/insert! conn :team
                {:id team-id
                 :name name
                 :hivy-id hivy-id
                 :features features})))

(defn- get-hivy-team!
  [{:keys [::db/conn]} team-id]
  (let [team (-> (db/get conn :team {:id team-id})
                 (cmd.teams/decode-row))]
    (when-not (:hivy-id team)
      (ex/raise :type :validation
                :code :team-not-hivy-managed
                :hint "team is not managed by Hivy"))
    team))

(defn- get-project-team!
  [{:keys [::db/conn] :as cfg} project-id]
  (let [project (db/get conn :project {:id project-id})
        team    (get-hivy-team! cfg (:team-id project))]
    [project team]))

(defn- get-profile
  [conn profile-id]
  (some-> (db/get* conn :profile {:id profile-id})
          (cmd.profile/decode-row)))

(defn- update-profile!
  [{:keys [::db/conn]} profile-id params]
  (-> (db/update! conn :profile params {:id profile-id} {::db/return-keys true})
      (cmd.profile/decode-row)))

(defn- ensure-mcp-token!
  [cfg profile-id]
  (or (db/exec-one! cfg ["SELECT id, name, token, type, created_at, updated_at, expires_at
                            FROM access_token
                           WHERE profile_id = ?
                             AND type = 'mcp'
                             AND (expires_at IS NULL OR expires_at > now())
                           ORDER BY created_at ASC
                           LIMIT 1"
                         profile-id])
      (cmd.access-token/create-access-token cfg profile-id "Hivy MCP" nil "mcp")))

(defn- resolve-file-profile-id!
  [{:keys [::db/conn]} team-id profile-id]
  (if profile-id
    (if (db/get* conn :team-profile-rel {:team-id team-id
                                         :profile-id profile-id})
      profile-id
      (ex/raise :type :validation
                :code :profile-not-in-team
                :hint "file creator profile is not a member of the team"))
    (or (-> (db/exec-one! conn ["SELECT profile_id
                                   FROM team_profile_rel
                                  WHERE team_id = ?
                                  ORDER BY created_at ASC
                                  LIMIT 1"
                                team-id])
            :profile-id)
        (ex/raise :type :validation
                  :code :profile-required
                  :hint "create at least one Hivy profile for the team before creating files"))))

(defn- mcp-url
  [token]
  (str (u/join (cf/get :public-uri) "mcp/stream")
       "?"
       (u/map->query-string {"userToken" token})))

(defn- redirect-response
  [uri]
  {::yres/status 302
   ::yres/headers {"location" (str uri)}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API: Teams
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- upsert-team
  [{:keys [::db/conn] :as cfg} {:keys [params]}]
  (let [{:keys [team-id hivy-id name]} (coerce-team-params params)
        team     (if-let [team (db/get* conn :team {:id team-id})]
                   (db/update! conn :team
                               {:name name
                                :hivy-id hivy-id}
                               {:id (:id team)}
                               {::db/return-keys true})
                   (create-team! cfg {:team-id team-id
                                      :hivy-id hivy-id
                                      :name name}))
        project  (ensure-default-project! cfg (:id team))]
    (raise-team-quotas! cfg (:id team))
    {::yres/status 200
     ::yres/body {:team-id (:id team)
                  :hivy-id (:hivy-id team)
                  :default-project-id (:id project)}}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API: Profiles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- upsert-profile
  [{:keys [::db/conn] :as cfg} {:keys [params]}]
  (let [{:keys [profile-id team-id hivy-id email fullname]} (coerce-profile-params params)
        _       (get-hivy-team! cfg team-id)
        profile (if-let [profile (get-profile conn profile-id)]
                  (let [props (-> (:props profile)
                                  (assoc :hivy-id hivy-id)
                                  (assoc :mcp-enabled true))]
                    (update-profile! cfg profile-id
                                     {:email (str/lower-case email)
                                      :fullname fullname
                                      :auth-backend "hivy"
                                      :is-active true
                                      :is-muted false
                                      :is-blocked false
                                      :props (db/tjson props)}))
                  (->> {:id profile-id
                        :email email
                        :fullname fullname
                        :backend "hivy"
                        :password "!"
                        :is-active true
                        :is-muted false
                        :props {:hivy-id hivy-id
                                :mcp-enabled true}}
                       (cmd.auth/create-profile cfg)
                       (cmd.auth/create-profile-rels cfg)))
        token   (ensure-mcp-token! cfg (:id profile))]
    (upsert-team-role! cfg (:id profile) team-id :owner)
    (raise-profile-quotas! cfg (:id profile))
    {::yres/status 200
     ::yres/body {:profile-id (:id profile)
                  :team-id team-id
                  :hivy-id hivy-id
                  :mcp-token (:token token)
                  :mcp-url (mcp-url (:token token))}}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API: Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- upsert-project
  [{:keys [::db/conn] :as cfg} {:keys [params]}]
  (let [{:keys [project-id team-id name]} (coerce-project-params params)
        _       (get-hivy-team! cfg team-id)
        project (if-let [project (db/get* conn :project {:id project-id})]
                  (if (= (:team-id project) team-id)
                    (db/update! conn :project
                                {:name name}
                                {:id project-id}
                                {::db/return-keys true})
                    (ex/raise :type :validation
                              :code :project-team-mismatch
                              :hint "project already belongs to another team"))
                  (cmd.teams/create-project conn {:id project-id
                                                  :team-id team-id
                                                  :name name}))]
    (raise-project-quotas! cfg (:id project))
    {::yres/status 200
     ::yres/body {:project-id (:id project)
                  :team-id (:team-id project)}}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API: Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- upsert-file
  [{:keys [::db/conn] :as cfg} {:keys [params]}]
  (let [{:keys [file-id project-id name profile-id]} (coerce-file-params params)
        [project team] (get-project-team! cfg project-id)
        profile-id (resolve-file-profile-id! cfg (:id team) profile-id)
        features   (cfeat/get-team-enabled-features cf/flags team)
        file       (if-let [file (db/get* conn :file {:id file-id})]
                     (if (= (:project-id file) project-id)
                       (db/update! conn :file
                                   {:name name}
                                   {:id file-id}
                                   {::db/return-keys true})
                       (ex/raise :type :validation
                                 :code :file-project-mismatch
                                 :hint "file already belongs to another project"))
                     (cmd.files-create/create-file cfg {:id file-id
                                                        :project-id (:id project)
                                                        :profile-id profile-id
                                                        :name name
                                                        :features features}))]
    (raise-file-quotas! cfg (:id file))
    {::yres/status 200
     ::yres/body {:file-id (:id file)
                  :project-id (:project-id file)
                  :team-id (:id team)}}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API: Session
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- claim
  [claims k]
  (let [n (name k)
        s (str/replace n "-" "_")]
    (or (get claims k)
        (get claims n)
        (get claims (keyword s))
        (get claims s))))

(defn- parse-claim-uuid
  [value]
  (cond
    (uuid? value) value
    (string? value) (uuid/parse* value)
    :else nil))

(defn- expired?
  [exp]
  (cond
    (inst? exp)
    (ct/is-before? exp (ct/now))

    (integer? exp)
    (< (* 1000 (long exp)) (inst-ms (ct/now)))

    (number? exp)
    (< (* 1000 (long exp)) (inst-ms (ct/now)))

    (string? exp)
    (if-let [exp (parse-long exp)]
      (expired? exp)
      true)

    :else
    true))

(defn- valid-audience?
  [audience]
  (cond
    (= "penpot-canvas" audience)
    true

    (coll? audience)
    (contains? (set audience) "penpot-canvas")

    :else
    false))

(defn- verify-hivy-token
  [token]
  (when-let [key (cf/get :hivy-control-plane-key)]
    (when-not (str/blank? key)
      (try
        (let [claims (jwt/unsign token key {:alg :hs256})
              iss    (claim claims :iss)
              aud    (claim claims :aud)
              exp    (claim claims :exp)]
          (when (and (= "hivy" iss)
                     (valid-audience? aud)
                     (not (expired? exp)))
            claims))
        (catch Throwable _cause
          nil)))))

(defn- workspace-uri
  [team-id file-id page-id]
  (-> (u/uri (cf/get :public-uri))
      (assoc :path "/#/workspace")
      (assoc :query (u/map->query-string
                     {"team-id" team-id
                      "file-id" file-id
                      "page-id" page-id}))))

(defn- dashboard-uri
  [team-id]
  (-> (u/uri (cf/get :public-uri))
      (assoc :path (str "/#/dashboard/team/" team-id "/projects"))))

(defn- session-target-uri
  [{:keys [team-id file-id page-id]}]
  (if file-id
    (workspace-uri team-id file-id page-id)
    (dashboard-uri team-id)))

(defn- create-session
  [{:keys [::db/conn] :as cfg} {:keys [params] :as request}]
  (let [token  (:token params)
        claims (some-> token verify-hivy-token)]
    (if-not claims
      {::yres/status 403}
      (let [profile-id (parse-claim-uuid (claim claims :profile-id))
            team-id    (parse-claim-uuid (claim claims :team-id))
            file-id    (some-> (claim claims :file-id) parse-claim-uuid)
            page-id    (some-> (claim claims :page-id) parse-claim-uuid)
            profile    (some-> (and profile-id (get-profile conn profile-id))
                               (dissoc :password))
            team       (when team-id
                         (db/get* conn :team {:id team-id}))
            rel        (when (and profile-id team-id)
                         (db/get* conn :team-profile-rel {:profile-id profile-id
                                                          :team-id team-id}))
            file-row   (when file-id
                         (db/exec-one! conn [sql:file-team file-id]))]
        (cond
          (not (and profile-id team-id profile))
          {::yres/status 403}

          (not (get-in profile [:props :hivy-id]))
          {::yres/status 403}

          (not (:is-active profile))
          {::yres/status 403}

          (:is-blocked profile)
          {::yres/status 403}

          (not (:hivy-id team))
          {::yres/status 403}

          (not rel)
          {::yres/status 403}

          (and file-id (not= team-id (:team-id file-row)))
          {::yres/status 403}

          :else
          (let [response (redirect-response (session-target-uri {:team-id team-id
                                                                 :file-id file-id
                                                                 :page-id page-id}))]
            ((session/create-fn cfg profile) request response)))))))
