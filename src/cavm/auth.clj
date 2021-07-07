(ns
  ^{:author "Olof-Joachim Frahm"
    :doc "Wrapper to authenticate users via OAuth2 services.

         Implements a Google API endpoint."}
  cavm.auth
  (:require [clojure.java.io :as io])
  (:require [clj-http.client :as client])
  (:require [clojure.tools.logging :as log :refer [info trace warn error]])
  (:require [ring.util.response :as response])
  (:require hiccup.page)
  (:require crypto.equality)
  (:require crypto.random)
  (:import [com.google.api.client.auth.openidconnect IdTokenVerifier$Builder])
  (:import [com.google.api.client.json.gson GsonFactory])
  (:import [com.google.api.client.http.javanet NetHttpTransport])
  (:import [com.google.api.client.googleapis.auth.oauth2 GoogleIdToken GoogleIdToken$Payload GoogleIdTokenVerifier GoogleIdTokenVerifier$Builder]))

(def client-id)
(def client-secret)

(def google-openid-discovery-url "https://accounts.google.com/.well-known/openid-configuration")
(def google-openid-discovery-document (delay (:body (client/get google-openid-discovery-url {:as :json}))))

(defn valid-user-email? [user-email-whitelist email]
  (contains? user-email-whitelist email))

(defn authenticated-user? [{{:keys [authenticated?]} :user}]
  authenticated?)

(defn google-authentication-request [{:keys [uri params session] :as request} endpoint-url]
  (let [state (or (:state session) (crypto.random/base64 60))
        location (str (:authorization_endpoint @google-openid-discovery-document)
                      "?"
                      (client/generate-query-string
                       {"client_id" client-id
                        "response_type" "code"
                        "scope" "email"
                        "redirect_uri" endpoint-url
                        "state" state}))]
    {:status 403
     :headers {"Content-Type" "text/html"
               "Location" location}
     :body (hiccup.page/html5
            {}
            [:head
             [:title "Access Denied!"]
             [:meta {:http-equiv "refresh" :content (str "5; url=" location)}]]
            [:body
             [:h1 "Access denied!"]
             [:p "You'll be redirected shortly to log in via Google. "
              [:a {:href location} "Otherwise please click here to log in."]]])
     :session (-> session
                  (assoc :redirect-to (or (response/get-header request "X-Redirect-To")
                                          {:uri uri :params params}))
                  (assoc :state state))}))

(defn make-verifier []
  (let [factory (GsonFactory.)
        builder (GoogleIdTokenVerifier$Builder. (NetHttpTransport.) factory)]
    ;; both of these can be disabled by setting them to null/nil
    [factory
     (-> ^IdTokenVerifier$Builder builder
         (.setAudience [client-id])
         (.setIssuer "https://accounts.google.com")
         (.build))]))

(defn check-oauth-token [^String token-string]
  (let [[^GsonFactory factory ^GoogleIdTokenVerifier verifier] (make-verifier)
        token (GoogleIdToken/parse factory token-string)]
    (if (.verify verifier token)
      (let [payload (.getPayload token)]
        (if (= (.getAudience payload) client-id)
          (if (= (.getAuthorizedParty payload) client-id)
            payload
            (error "Client ID mismatch."))
          (error "Audience mismatch.")))
      (error "Couldn't verify Google ID token."))))

(defn login-failed-page [content]
  (info "Login failed:" content)
  {:status 400
   :headers {"Content-Type" "text/html"}
   :body (hiccup.page/html5
          {}
          [:head
           [:title "Authentication failed!"]]
          [:body
           [:h1 "Authentication failed!"]
           [:p content]])})

(defn google-authentication-handler [{:keys [params session]}
                                     user-email-whitelist
                                     server-address
                                     endpoint-url]
  (if (crypto.equality/eq? (:state session) (get params "state"))
    (let [response (client/post
                    (:token_endpoint @google-openid-discovery-document)
                    {:form-params {:code (get params "code")
                                   :client_id client-id
                                   :client_secret client-secret
                                   :redirect_uri endpoint-url
                                   :grant_type "authorization_code"}
                     :as :json})
          ^GoogleIdToken$Payload payload (check-oauth-token (:id_token (:body response)))]
      (if payload
        (let [email (.get payload "email")]
          (if (valid-user-email? user-email-whitelist email)
            (do
              (info "Login:" email)
              (-> (response/redirect
                   (let [{:keys [redirect-to]} session]
                     (if (string? redirect-to)
                       redirect-to
                       (let [{:keys [uri params]} redirect-to]
                         (str server-address (subs uri 1)
                              (and (seq params) (str "?" (client/generate-query-string params))))))))
                 (assoc :session {:user {:authenticated? true
                                         :email email}})))
            (login-failed-page (str "Invalid user email: " email "."))))
        (login-failed-page (str "Invalid Google ID token."))))
    (login-failed-page (str "Invalid state: " (get params "state") "."))))

(defn logout-user [{:keys [user]} server-address]
  (info "Logout:" (:email user))
  (-> (response/redirect server-address)
      (assoc :session nil)))

(defn wrap-google-authentication [handler server-address endpoint-query-string
                                  user-email-whitelist configuration]
  (def client-id (:client-id configuration))
  (def client-secret (:client-secret configuration))
  (let [endpoint-url (str server-address (subs endpoint-query-string 1))]
    (fn [{:keys [params session] :as request}]
      (if (= (:uri request) endpoint-query-string)
        (if (get params "logout")
          (logout-user session server-address)
          (google-authentication-handler request user-email-whitelist
                                         server-address endpoint-url))
        (if (authenticated-user? session)
          (handler request)
          (google-authentication-request request endpoint-url))))))

(defn load-users [filename]
  (with-open [f (io/reader (io/resource filename))]
    (into #{} (line-seq f))))
