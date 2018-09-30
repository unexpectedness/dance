(defproject dance "0.2.0-SNAPSHOT"
  :description "Advanced tree walking in Clojure"
  :url "https://github.com/unexpectedness/dance"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [arity "0.2.0"]
                 [weaving "0.1.3"]
                 [threading "0.3.3"]
                 [net.clojars.unexpectedness/shuriken "0.14.20"]
                 [org.flatland/ordered "1.5.6"]]
  :profiles {:dev {:dependencies [[codox-theme-rdash "0.1.2"]]}}
  :plugins [[lein-codox "0.10.3"]]
  :codox {:source-uri "https://github.com/unexpectedness/dance/" \
                      "blob/{version}/{filepath}#L{line}"
          :metadata {:doc/format :markdown}
          :themes [:rdash]})
