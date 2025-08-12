(defproject parser "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}

  :dependencies [[org.clojure/clojure "1.12.1"]
                 [org.clojure/tools.logging "1.3.0"]]

  :deploy-repositories [["clojars" {:url      "https://repo.clojars.org"
                                    :username :env/clojars_user
                                    :password :env/clojars_pass}]]


  :resource-paths ["src/resourses"]
  :profiles {:test {:global-vars    {*warn-on-reflection* true}
                    :resource-paths ["test/resources"]}}

  :plugins [[org.clojars.jj/bump "1.0.4"]
            [org.clojars.jj/strict-check "1.0.2"]
            [org.clojars.jj/bump-md "1.0.0"]]

  :repl-options {:init-ns parser.core})
