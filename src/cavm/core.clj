(ns
  ^{:author "Brian Craft"
    :doc "xena server main."}
  cavm.core
  (:require [clojure.string :as s])
  (:require [cavm.h2 :as h2])
  (:require [cavm.auth :as auth])
  (:require [clojure.java.io :as io])
  (:require [ring.adapter.jetty9 :refer [run-jetty]])
  (:require [clojure.data.json :as json])
  (:require [me.raynes.fs :as fs])
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:require [cgdata.core :as cgdata])
  (:require [ring.middleware.resource :refer [wrap-resource resource-request]])
  (:require [ring.middleware.content-type :refer [wrap-content-type]])
  (:require [ring.middleware.not-modified :refer [wrap-not-modified]])
  (:require [ring.middleware.params :refer [wrap-params]])
  (:require [ring.middleware.multipart-params :refer [wrap-multipart-params]])
  (:require [ring.middleware.multipart-params.byte-array :refer [byte-array-store]])
  (:require [cavm.views.datasets])
  (:require [ring.middleware.gzip :refer [wrap-gzip]])
  (:require [cavm.ring-instrument :refer [wrap-stacktrace-web wrap-trace wrap-reload]])
  (:require [filevents.core :refer [watch]])
  (:require [cavm.readers :as cr])
  (:require [cavm.loader :as cl])
  (:require [cavm.fs-utils :refer [normalized-path]])
  (:require [cavm.cgdata])
  (:require [cavm.events :as events])
  (:require [clj-http.client :as client])
  (:require [clojure.tools.logging :as log :refer [info trace warn error]])
  (:require [taoensso.timbre :as timbre])
  (:require [less.awful.ssl :as ssl])
  (:require [ring.util.response :as response])
  (:require [ring.util.request :refer [path-info]])
  (:require hiccup.page)
  (:import cavm.XenaImport cavm.Splash)
  (:import org.slf4j.LoggerFactory
           ch.qos.logback.classic.joran.JoranConfigurator
           ch.qos.logback.core.util.StatusPrinter)
  (:require crypto.equality)
  (:require ring.middleware.session)
  (:require crypto.random)
  (:import [com.google.api.client.json.gson GsonFactory])
  (:import [com.google.api.client.http.javanet NetHttpTransport])
  (:import [com.google.api.client.googleapis.auth.oauth2 GoogleIdToken GoogleIdToken$Payload GoogleIdTokenVerifier GoogleIdTokenVerifier$Builder])
  (:import [org.eclipse.jetty.server HttpConnectionFactory])
  (:gen-class))

(defn- in-data-path [root path]
  (boolean (fs/child-of? (normalized-path root) (normalized-path path))))

(def pom-props
  (doto (java.util.Properties.)
    (.load (io/reader (io/resource "META-INF/maven/cavm/cavm/pom.properties")))))

(def version (get pom-props "version"))

;
; web services

(def trusted-hosts
  ["https://([-_A-Za-z0-9]+\\.)?xenabrowser\\.net(:[0-9]+)?"])

(def local-trusted-host "http://localhost(:[0-9]+)?")

(def public-url "https://xenabrowser.net")
;(def public-xena (str public-url "proj/public/xena"))
(def tcga-xena "https://tcga.xenahubs.net")
(def toil-xena "https://toil.xenahubs.net")
(def icgc-xena "https://icgc.xenahubs.net")
(def ucscPublic-xena "https://ucscpublic.xenahubs.net")
(defn local-url [port] (str "https://local.xena.ucsc.edu:" (inc port)))
(defn local-xena [port] (str "http://local.xena.ucsc.edu:" port))

; clojure re-matches has polymorphic return type, which is broken.
; Untangle it here.
(defn re-match [pat string]
  (let [m (re-matches pat string)]
    (cond
      (vector? m) (first m)
      :else m))) ; string, or nil

(def cohort-query
  '(map :cohort (query
                  {:select [:%distinct.cohort]
                   :from [:dataset]})))

(defn async-post [url post cb]
  (let [x (agent nil)]
    (set-error-handler! x (fn [_ err] (error err "Fetching cohort list") (cb [])))
    (send-off x (fn [_]
                  (cb (:body (client/post url {:body
                                               (str post)
                                               :as :json
                                               :content-type
                                               "text/plain"})))))))

; need to block in an agent & then invoke callback
(defn retrieve-cohorts [port cb]
  (let [x (atom [])
        update (fn [cohorts]
                 (swap! x (fn [all]
                            (let [ret (conj all cohorts)]
                              (when (= 5 (count ret))
                                (cb (filter identity (apply concat ret))))
                              ret))))]
    (async-post (str tcga-xena "/data/") cohort-query update)
    (async-post (str icgc-xena "/data/") cohort-query update)
    (async-post (str toil-xena "/data/") cohort-query update)
    (async-post (str ucscPublic-xena "/data/") cohort-query update)
    (async-post (str (local-xena port) "/data/") cohort-query update)))

(defn- wrap-access-control [handler allow-hosts]
  (fn [{:keys [request-method] :as request}]
    (let [response (if (= request-method :options)
                     (response/response "")
                     (handler request))]
      (if-let [origin (re-match allow-hosts (get-in request [:headers "origin"] ""))]
        (-> response
            (assoc-in [:headers "Access-Control-Allow-Origin"] origin)
            (assoc-in [:headers "Access-Control-Expose-Headers"] "Location")
            (assoc-in [:headers "Access-Control-Allow-Credentials"] "true")
            (assoc-in [:headers "Access-Control-Allow-Headers"] "Cancer-Browser-Api, X-Redirect-To"))
        response))))

(defn- add-headers [handler]
  (fn [request]
    (-> (handler request)
        (assoc-in [:headers "Xena-API"] version)
        (assoc-in [:headers "X-Content-Type-Options"] "nosniff")
        (update-in [:headers "Vary"] #(if % (s/join ", " [% "Origin"]) "Origin")))))

(defn- attr-middleware [app k v]
  (fn [req]
    (app (assoc req k v))))

(defn- log-middleware [handler]
  (fn [{:keys [uri query-string request-method session] :as request}]
    (info "Start:" (.toUpperCase (name request-method)) uri query-string
          (or (:email (:user session)) "(no session)"))
    (try
      (let [resp (handler request)]
        (info "End:" uri (:status resp))
        resp)
      (catch Exception ex
        (error ex "Error handling request" uri)
        (throw ex)))))

(comment (defn- del-datasets [args]
          (dorun (map del-exp args))))

(comment (defn- print-datasets []
          (dorun (map println (datasets)))))

(defn load-edn-from
  [filename]
  (with-open [reader (-> (io/resource filename)
                         io/reader
                         java.io.PushbackReader.)]
    (clojure.edn/read reader)))

(defn wrap-authentication [app userauth port]
  (if userauth
    (auth/wrap-google-authentication
      app
      (str (local-url port) "/") ; XXX local-url? Is this correct?
      "/code"
      (auth/load-users "users.txt")
      (load-edn-from "auth.config"))
    app))

; XXX Bug in clojure ring. May not be needed after upgrade.
; The bug is that :head requests from other handlers are clobbered.
(defn wrap-resource-workaround [handler root-path]
  (let [resource-handler (wrap-resource handler root-path)]
    (fn [request]
      (if (resource-request request root-path)
        (resource-handler request)
        (handler request)))))

(defn wrap-db-loading [handler db]
  (fn [request]
    (if @db
      (handler request)
      {:status 503
       :headers {}
       :body "Database booting"})))

(defn wrap-ping [handler]
  (fn [request]
    (if (= (path-info request) "/ping/")
      {:status 200
       :headers {}
       :body "pong"}
      (handler request))))

; XXX add ring jsonp?
(defn- get-app [docroot db loader load-queue port userauth allow-hosts]
  (-> cavm.views.datasets/routes
      (wrap-authentication userauth port)
      (wrap-trace :header :ui)
      (attr-middleware :docroot docroot)
      (attr-middleware :loader loader)
      (attr-middleware :load-queue load-queue)
      (wrap-resource-workaround "public")
      (wrap-content-type)
      (wrap-not-modified)
      (wrap-gzip)
      (log-middleware)
      ring.middleware.session/wrap-session
      (wrap-params)
      (wrap-multipart-params {:store (byte-array-store)})
      (wrap-stacktrace-web)
      (wrap-ping)
      (wrap-db-loading db)
      (attr-middleware :db db)
      (wrap-access-control allow-hosts)
      (wrap-reload) ; XXX only in dev? Does this slow things down?
      (add-headers)))

; jetty default thread use is availableProcessors/4 acceptor threads,
; and selector threads equal to acceptor threads. For https + http, that
; sums to exactly availableProcessors. Pick a max thread pool two that.
;
; We could also manipulate acceptor threads, or make it configurable, but
; trying this for now.
(def max-threads
  (delay (max 80 (* 2 (.availableProcessors (Runtime/getRuntime))))))

(defn configurator [server]
  ; disable Server header.
  (doseq [conn (filter #(instance? HttpConnectionFactory %)
                       (mapcat
                         #(.getConnectionFactories %)
                         (.getConnectors server)))]
    (.setSendServerVersion (.getHttpConfiguration conn) false)))

(defn- serv [app host port {:keys [keystore password]} ws-config]
  (run-jetty app {:configurator configurator
                  :websockets ws-config
                  :allow-null-path-info true
                  :host host
                  :port port
                  :ssl? true
                  :max-threads @max-threads
                  :ssl-port (+ port 1)
                  :keystore keystore
                  :key-password password
                  :join? false}))

; XXX call clean-sources somewhere?? Should be automated.
(comment (defn- loadfiles [load-fn root args]
          (when (not (> (count args) 0))
            (println "Usage\nload <filename>")
            (System/exit 0))

   ; Skip files outside the designated path
          (let [{in-path true, not-in-path false}
                (group-by #(in-data-path root %) args)]
            (when not-in-path
              (binding [*out* *err*]
                (println "These files are outside the CAVM data path and will not be served:")
                (println (s/join "\n" (in-path false)))))
            (create)
            (println "Loading " (count in-path) " file(s)")
            (dorun (map #(do (print %2 %1 "") (time (load-report load-fn root %1)))
                        in-path
                        (range (count in-path) 0 -1)))
            (clean-sources))))

(defn- load-files [port always files]
  (client/post (str "http://localhost:" port "/update/")
               {:form-params (merge {:file (map normalized-path files)}
                                    (when always {:always "true"}))}))

(defn- delete-files [port files]
  (client/post (str "http://localhost:" port "/update/")
               {:form-params {:file (map normalized-path files) :delete "true"}}))

(def ^:private detectors
  [cgdata/detect-cgdata
   cgdata/detect-tsv])

(defn- filter-hidden
  [s]
  (filter #(not (.isHidden ^java.io.File %)) s))

; Full reload metadata. The loader will skip
; data files with unchanged hashes.
(defn- file-changed [loader docroot kind file]
  (doseq [f (-> docroot
                (io/file)
                (file-seq)
                (rest) ; skip docroot (the first element)
                (filter-hidden))]
    (try (loader f)
      (catch Exception e (warn e "Dispatching load" f)))))

(def ^:private xenadir-default (str (io/file (System/getProperty  "user.home") "xena")))
(def ^:private docroot-default (str (io/file xenadir-default "files")))
(def ^:private db-default (str (io/file xenadir-default "database")))
(def ^:private logfile-default (str (io/file xenadir-default "xena.log")))
(def ^:private tmp-dir-default
  (str (io/file (System/getProperty "java.io.tmpdir") "xena-staging")))

(def ^:private argspec
  [[nil "--no-serve" "Don't start web server" :id :serve :parse-fn not :default true]
   ["-a" "--allow PATTERN" "Allow CORS from hosts matching pattern" :default [] :assoc-fn (fn [m k v] (update-in m [k] conj v))
    :validate [re-pattern "Invalid pattern"]]
   ["-p" "--port PORT" "Server port to listen on" :default 7222 :parse-fn #(Integer/parseInt %)]
   ["-l" "--load" "Load files into running server"]
   ["-x" "--delete" "Delete file from running server"]
   [nil "--force" "Force reload of unchanged files (with -l)"]
   [nil "--auto" "Auto-load files" :id :auto :default false]
   [nil "--no-gui" "Don't start GUI" :id :gui :parse-fn not :default true]
   ["-h" "--help" "Show help"]
   ["-H" "--host HOST" "Set host for listening socket" :default "localhost"]
   ["-r" "--root DIR" "Set document root directory" :default docroot-default]
   ["-D" "--localdev" "Allow CORS from localhost" :default false]
   ["-d" "--database FILE" "Database to use" :default db-default]
   [nil "--logfile FILE" "Log file to use" :default logfile-default]
   [nil "--keyfile FILE" "Private key file to use (requires --certfile)"]
   [nil "--certfile FILE" "Cert file to use (requires --keyfile)"]
   [nil "--version" "Print version and exit"]
   ["-j" "--json" "Fix json"]
   [nil "--userauth" "Enable user-based authentication"]
   ["-t" "--tmp DIR" "Set tmp dir" :default tmp-dir-default]])

(defn- mkdir [dir]
  (.mkdirs (io/file dir))
  (when (not (.exists (io/file dir)))
    (str "Unable to create directory: " dir)))

(def ^:private default-h2-opts-map
  {:cache_size 65536
   :undo_log 1
   :log 1
   :max_query_timeout 60000
   :multi_threaded "TRUE"
   :trace_level_file (h2/h2-log-level :slf4j)})

; Enable transaction log, rollback log, and MVCC (so we can load w/o blocking readers).
(def ^:private default-h2-opts
  (s/join ";" (for [[k v] default-h2-opts-map]
                (str (.toUpperCase (name k)) "=" v))))

; Might want to allow more piecemeal setting of options, by
; parsing them & allowing cli overrides. For now, if the
; user sets options they must be comprehensive. If the user
; doesn't set options, we use default-h2-opts.

(defn- h2-opts
  "Add default h2 options if none are specified"
  [^String database]
  (if (= -1 (.indexOf database ";"))
    (str database ";" default-h2-opts)
    database))

(defn- logback-config
  ([filename]
   (logback-config filename "logback_config.xml"))
  ([filename config]
   (System/setProperty "log_file" filename)
   (let [context (LoggerFactory/getILoggerFactory)
         configurator (JoranConfigurator.)]
     (try
       (.setContext configurator context)
       (.reset context)
       (.doConfigure configurator (io/resource config))
       (catch Exception e))
     (StatusPrinter/printInCaseOfErrorsOrWarnings context))))

(defn- log-config [filename]
  (logback-config filename)
  (log/log-capture! 'cavm.core) ; redirect System.out System.error to clojure.tools.logging
  ; Only use timbre for the profiling commands. Might want to extract
  ; them into a standalone lib.
  (timbre/set-level! :trace)
  (timbre/merge-config!
    {:appenders {:standard-out {:enabled? false}
                 :jl {:enabled? true
                      :fn (fn [{:keys [ns level throwable message]}]
                            (log/log ns level throwable message))}}}))

(defn validate-certkey [{{:keys [keyfile certfile]} :options :as p-opts}]
  (if (or (and certfile (not keyfile)) (and (not certfile) keyfile))
    (update-in p-opts [:errors] conj "--certfile and --keyfile must be used together")
    p-opts))

; XXX create dir for database as well?
(defn -main [& args]
  (let [{:keys [options arguments summary errors]} (validate-certkey (parse-opts args argspec))
        docroot (:root options)
        port (:port options)
        localdev (:localdev options)
        allow (:allow options)
        allow-hosts (re-pattern (s/join "|" (concat trusted-hosts allow (if localdev [local-trusted-host] []))))
        host (:host options)
        tmp (:tmp options)
        logfile (:logfile options)
        always (:force options)
        gui (:gui options)
        serve (:serve options)
        certfile (:certfile options)
        keyfile (:keyfile options)
        userauth (:userauth options)
        keystore (if keyfile
                   {:keystore (ssl/key-store keyfile certfile)
                    :password (String. ^chars ssl/key-store-password)}
                   {:keystore (.toString (io/resource "localhost.keystore"))
                    :password  "localxena"})
        database (h2-opts (:database options))]
    (if errors
      (binding [*out* *err*]
        (println (s/join "\n" errors)))
      (try
        (cond
          (:help options) (println summary)
          (:version options) (println (get pom-props "version"))
          (:json options) (cgdata/fix-json docroot)
          (:load options) (load-files port always arguments)
          (:delete options) (delete-files port arguments)
          :else (if-let [error (some mkdir [tmp docroot])]
                  (binding [*out* *err*] ; XXX move below log-config? What if we can't create the log file?
                    (println error)
                    (System/exit 1)) ; XXX notify gui
                  (do
                    (log-config logfile)
                    (info "Xena server starting"
                          (get pom-props "version")
                          (s/trim
                            (get pom-props "revision")))
                    (h2/set-tmp-dir! tmp)
                    (let [db (atom nil)
                          xena-import (atom nil)
                          detector (apply cr/detector docroot detectors)
                          [loader load-queue] (cl/loader-agent db detector docroot)]
                      (when (:auto options)
                        (watch (partial file-changed loader docroot) docroot))
                      ; Startup sequence is a bit complicated. We want to
                      ; bring up the server w/o waiting on the db, then bring
                      ; up the gui, but the server needs the db an gui hooks.
                      ; So we use a couple atoms, and fill them in. Also, we
                      ; can't block the main thread until after starting the gui
                      ; and db, so we defer the .join on jetty until the end.
                      (let [ws-config (events/jetty-config load-queue)
                            server
                            (when serve
                              (try
                                (serv (get-app docroot db loader load-queue port userauth allow-hosts) host port keystore ws-config)
                                (catch org.eclipse.jetty.util.MultiException ex
                                  (let [ex0 (.getThrowable ex 0)]
                                    (if (instance? java.net.BindException ex0)
                                      (do
                                        (binding [*out* *err*]
                                          (println "Port already bound. Notifying other process."))
                                        (System/exit 1))
                                      (throw ex))))))]
                        (when (and gui serve) (try
                                               (reset! xena-import (XenaImport/start))
                                               (catch Exception ex
                                                 (binding [*out* *err*]
                                                   (println "Failed to start gui. Logging error.")
                                                   (error "Failed to start gui." ex)
                                                   nil))))
                        (Splash/close)
                        (reset! db (h2/create-xenadb database))
                        (when server
                          (.join server)))))))
        (catch Exception ex
          ; XXX maybe should enable ERROR logging to console, instead of this.
          (binding [*out* *err*]
            (println "Uncaught exception" ex))
          (error "Uncaught exception" ex)
          (System/exit 1)))))
  (shutdown-agents))

; When logging to the repl from a future, *err* gets lost.
; This will set it to the repl terminal, for reasons I don't understand.
(comment (defn snoop [msg x]
          (.start (Thread. #(binding [*out* *err*]
                              (println msg x)
                              (flush))))
          x))


(comment

  (do
    (logback-config "log" "logback_repl.xml") ; set logging to terminal

    (def testdb (atom (h2/create-xenadb (str (io/file (System/getProperty "user.home") "xena/database") ";TRACE_LEVEL_FILE=3"))))
    (def docroot (str (io/file (System/getProperty "user.home") "xena" "files")))

    (def testdetector (apply cr/detector docroot detectors))
    (def testloader (cl/loader-agent testdb testdetector docroot))
    ;            (watch (partial file-changed #'testloader docroot-default) docroot-default)
    (def keystore {:keystore (.toString (io/resource "localhost.keystore"))
                   :password  "localxena"})

    (let [[loader load-queue] testloader]
      (def ws-config (events/jetty-config load-queue))
      (def app (get-app docroot testdb loader load-queue 7222 nil
                        (re-pattern (s/join "|" (conj trusted-hosts local-trusted-host))))))
    (defonce server (serv #'app "localhost" 7222 keystore ws-config))

    (.start server))


  (do (.stop server)
      (events/jetty-config-dispose)
      (cavm.db/close @testdb))
  )
