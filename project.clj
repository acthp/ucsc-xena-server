(def version "0.26.0-SNAPSHOT")

(defproject cavm version
  :description "Cancer Analytics Virtual Machine"
  :url "https://genome-cancer.ucsc.edu"
  :license {:name "Apache License Version 2.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
;  :auto-clean false ; Faster re-builds in dev
  :repl-options  {
                  ;; If nREPL takes too long to load it may timeout,
                  ;; increase this to wait longer before timing out.
                  ;; Defaults to 30000 (30 seconds)
                  :timeout 12000000}

  :profiles {:dev {:source-paths ["src-dev" "src"]
                   :dependencies [[org.jumpmind.symmetric.schemaspy/schemaspy "5.0.0"]
                                  [com.clojure-goes-fast/clj-java-decompiler  "0.1.0"]
                                  [criterium  "0.4.4"]]
                   :plugins [[test2junit  "1.4.2"]]}
             :benchmark {:jvm-opts ["-server"]
                         :plugins [[lein-nodisassemble  "0.1.3"]]}
             :uberjar {:omit-source true :aot [mikera.vectorz.core]}}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.async  "0.4.474"]
                 [org.clojure/tools.cli "0.3.1"]
                 [net.mikera/core.matrix "0.8.0"]
                 [net.mikera/vectorz-clj "0.11.0"]
                 [org.clojure/java.jdbc "0.3.4"]
                 [com.mchange/c3p0 "0.9.5-pre8"]
                 [org.clojars.smee/binary "0.2.0"]
                 [clj-time "0.5.0"]
                 [liberator "0.10.0"]
                 [compojure "1.1.3"]
                 [hiccup "1.0.5"]
                 [ring/ring-core "1.2.2"]
                 [ring/ring-servlet "1.2.2"]
                 [ring/ring-devel "1.2.2"] ; XXX only in dev
                 [info.sunng/ring-jetty9-adapter "0.13.0"]
                 [amalloy/ring-gzip-middleware "0.1.3"]
                 [me.raynes/fs "1.4.2"]
                 [digest "1.4.3"]
                 [org.clojure/data.json "0.2.3"]
;                 [org.codehaus.jsr166-mirror/jsr166y "1.7.0"] ; for core.reducers
                 [clojure-csv/clojure-csv "2.0.1"]
                 [honeysql "0.4.2"]
                 [com.h2database/h2 "1.3.175"]
                 [filevents "0.1.0"]
                 [clj-http "0.9.1"]
                 [crypto-equality "1.0.0"]
                 [crypto-random "1.2.0"]
                 [org.clojure/core.cache "0.6.3"]
                 [ch.qos.logback/logback-classic "1.1.1"]
                 [org.clojure/tools.logging "0.3.0"]
                 [org.clojure/test.check "0.5.9"]
                 [com.google.code.gson/gson "2.2.4"]
                 [less-awful-ssl "1.0.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 [com.taoensso/timbre "3.2.0"]
                 [org.clojure/data.int-map  "0.2.3"]
                 [com.google.api-client/google-api-client "1.20.0"]
                 [com.google.api-client/google-api-client-gson "1.20.0"]
                 [com.google.http-client/google-http-client "1.20.0"]
                 [com.google.http-client/google-http-client-gson "1.20.0"]
                 [com.google.apis/google-api-services-oauth2 "v2-rev97-1.20.0"]]
  :java-source-paths ["src-java"]
  :target-path "target/%s"
  :clean-targets [:target-path :compile-path "build"]
  :aliases {"uberdoc" ["do" ["alldocs"] ["clientsrc"] ["docinstall"] ["uberwrap"]]
            "benchmark" ["with-profile" "benchmark" "run" "-m" "cavm.benchmark.core"]
            ; XXX this is a bit raw
            "docinstall" ["do"
                          ["shell" "rm" "-rf" "resources/public/docs"]
                          ["shell" "mv" "doc/_build" "resources/public/docs"]]
            "uberwrap" ["do"
                        ["uberjar"]
                        ["uberfix"]
                        ["install4j"]
                        ["downloads"]]
            "downloads"  ["shell" "sh" "-c" "cp get-xena/* build"]
            "uberfix" ["shell" "./fixjar" ~(str "target/uberjar/cavm-" version "-standalone.jar")]
            "install4j" ["shell" "install4jc" "-r" ~version "build1.install4j"]
            "clientsrc" ["shell" "cp" "-r" "python" "doc/_build"]
            "schemaspy" ["shell" "./schemaspy"]
            "ensure-sphinx" ["shell" "./getsphinx"]
            "sphinx-with-path" ["shell" "sh" "-c" "PATH=sphinxEnv/bin:$PATH lein sphinx"]
            "alldocs" ["do"
                       ["ensure-sphinx"]
                       ["sphinx-with-path"]
                       ["doc"]
                       ["hiera"]
                       ["schemaspy"]]}
  :jar-exclusions     [#"schemaspy.clj" #"(?:^|/)\..*\.swp$" #"(?:^|/)\.nfs"]
  :uberjar-exclusions [#"schemaspy.clj" #"(?:^|/)\..*\.swp$" #"(?:^|/)\.nfs" #"\.(clj|java)$"]
  :javac-options ["-target" "1.8" "-source" "1.8"]
  :aot [cavm.core cavm.conn-customizer]
  :global-vars {*warn-on-reflection* true}
  :plugins [[lein-sphinx "1.0.1"]
            [codox "0.8.10"]
            [lein-shell "0.4.0"]
            [lein-hiera "0.8.0"]]
  :sphinx {:setting-values {:version ~version
                            :project ~(str "UCSC Xena Server " version)}}
  :codox {:output-dir "doc/_build/implementation"}
  :hiera {:ignore-ns #{cavm.h2-query}
          :cluster-depth 1
          :vertical? false
;          :show-external? true
          :path "doc/_build/dependencies.png"}
  :test2junit-output-dir "test-results"
  :main cavm.core)
