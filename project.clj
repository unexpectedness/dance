(defproject net.clojars.unexpectedness/dance "0.1.4.2"
  :description "Advanced tree walking in Clojure"
  :url "https://github.com/unexpectedness/dance"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [arity "0.2.0"]
                 ^:inline-dep [net.clojars.unexpectedness/weaving "0.1.5.1"]
                 [net.clojars.unexpectedness/threading "0.4.1"]
                 [net.clojars.unexpectedness/shuriken  "0.14.52"]
                 [net.clojars.unexpectedness/derrida   "0.1.1"]
                 [org.flatland/ordered "1.5.7"]]
  :profiles {:dev {:dependencies [[codox-theme-rdash "0.1.2"]]}}
  :plugins [[lein-codox "0.10.8"]
            [thomasa/mranderson "0.5.3"]]
  :aliases {"repl"    ["do" ["inline-deps"] ["with-profile" "+plugin.mranderson/config" "repl"]]
            "test"    ["do" ["inline-deps"] ["with-profile" "+plugin.mranderson/config" "test"]]
            "uberjar" ["do" ["inline-deps"] ["with-profile" "+plugin.mranderson/config" "uberjar"]]
            "jar"     ["do" ["inline-deps"] ["with-profile" "+plugin.mranderson/config" "jar"]]
            "deploy"  ["do" ["inline-deps"] ["with-profile" "+plugin.mranderson/config" "deploy" "clojars"]]
            "install" ["do" ["inline-deps"] ["with-profile" "+plugin.mranderson/config" "install"]]}
  :codox {:source-uri "https://github.com/unexpectedness/dance/" \
                      "blob/{version}/{filepath}#L{line}"
          :metadata {:doc/format :markdown}
          :themes [:rdash]})
