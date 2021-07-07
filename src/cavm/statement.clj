(ns
  ^{:author "Brian Craft"
    :doc "Utilities for sql prepared statements"}
  cavm.statement
  (:require [clojure.java.jdbc :as jdbc])
  (:require [clojure.java.jdbc.deprecated :as jdbcd])
  (:require [cavm.conn-customizer :refer [watch-destroy]])
  (:require [clojure.tools.logging :refer [spy trace]])
  (:import java.sql.PreparedStatement
           com.mchange.v2.c3p0.impl.NewProxyConnection
           org.h2.jdbc.JdbcConnection))

;
; Utils for prepared statements
;

; Record for PreparedStatements that can be a target for with-open, and
; execute the PreparedStatement.
(defrecord PStatement [^PreparedStatement stmt execute]
  java.sql.Statement
  (close [this] (.close stmt))
  clojure.lang.IFn
  (invoke [this values]
    (execute values))
  (applyTo [this args]
    (apply execute args)))

(defn- stmt-execute
  ([conn stmt fields]
   (fn [values]
     (jdbc/db-do-prepared {:connection conn} false stmt (mapv values fields))))
  ([conn stmt]
   (fn [values]
     (jdbc/db-do-prepared {:connection conn} false stmt values))))

(defn sql-stmt
  "Return a prepared statement which can participate in with-open."
  ^cavm.statement.PStatement [conn stmt-str & args]
  (let [stmt (jdbc/prepare-statement conn stmt-str)]
    (->PStatement
      stmt
      (apply stmt-execute conn stmt args))))

(defn- query-execute
  ([conn stmt fields]
   (fn [values]
     (jdbc/query {:connection conn} [stmt (mapv values fields)])))
  ([conn stmt]
   (fn [values]
     (jdbc/query {:connection conn} (into [stmt] values)))))

(defn sql-stmt-result
  "Return a prepared statement which returns a result set,
  and can participate in with-open."
  ^cavm.statement.PStatement [conn stmt-str & args]
  (let [stmt (jdbc/prepare-statement conn stmt-str)]
    (->PStatement
      stmt
      (apply query-execute conn stmt args))))

;
;
; Utility to cache prepared statements, per-connection. This may seem to
; duplicate c3p0 statement caching, however it's per-statement, so hashing is
; on the connection, not the statement string. Not positive this matters, but
; the c3p0 statement cache has been slow in testing, and it can deadlock when
; executing a prepared statement that needs to create a prepared statement.

(defn cached-statement
  "Utility to cache a prepared statement, per-connection.  Returns a function
  that will execute the <stmt-str> as a prepared statement on the given
  connection. The prepared statement will be cached for reuse on the same
  connection. When a connection is destroyed, any statement cached here is disposed.

  This is different from c3p0 statement caching, which hashes every query, and
  looks for a cached prepared statement. This has been slow in our performance
  tests, and will deadlock if the prepared statement ends up creating another
  prepared statement (i.e. via a sql function call)."
  [stmt-str result?]
  (let [mk-stmt (if result? sql-stmt-result sql-stmt)
        cache (atom {})
        lookup (fn [conn]
                 (or (@cache conn)
                     ((swap! cache assoc conn (mk-stmt conn stmt-str)) conn)))]
    (watch-destroy (fn [conn]
                     (swap! cache dissoc conn)))
    ; One could try to pass in the connection, however when running in the
    ; server context the connection object passed to functions will not be
    ; the connection object seen by the client, or watch-destroy. That causes
    ; the cache to leak. Instead we rely on a per-thread setting via jdbcd.
    (fn [& args]
      (let [conn (.unwrap ^NewProxyConnection (jdbcd/find-connection) JdbcConnection)]
        ((lookup conn) args)))))

;
; monkey-patch jdbc to fix performance bug
(in-ns 'clojure.java.jdbc)
(defn- db-do-execute-prepared-statement [db ^PreparedStatement stmt param-groups transaction?]
  (if (empty? param-groups)
    (if transaction?
      (with-db-transaction [t-db (add-connection db (.getConnection stmt))]
        (vector (.executeUpdate stmt)))
      (try
        (vector (.executeUpdate stmt))
        (catch Exception e
          (throw-non-rte e))))
    (do
      (doseq [param-group param-groups]
             ((or (:set-parameters db) set-parameters) stmt param-group)
             (.addBatch stmt))
      (if transaction?
        (with-db-transaction [t-db (add-connection db (.getConnection stmt))]
          (execute-batch stmt))
        (try
          (execute-batch stmt)
          (catch Exception e
            (throw-non-rte e)))))))

(in-ns 'cavm.statement)
