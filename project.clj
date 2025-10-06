(defproject org.clojars.jj/majavat "1.12.1"
  :description "Templating engine for clojure"
  :url "https://github.com/ruroru/majavat"
  :license {:name "EPL-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}

  :dependencies [[org.clojure/clojure "1.12.3"]
                 [org.clojure/tools.logging "1.3.0"]]

  :deploy-repositories [["clojars" {:url      "https://repo.clojars.org"
                                    :username :env/clojars_user
                                    :password :env/clojars_pass}]]


  :resource-paths ["src/resourses"]
  :profiles {:test {:global-vars    {*warn-on-reflection* true}
                    :dependencies   [[ch.qos.logback/logback-classic "1.5.19"]
                                     [mock-clj "0.2.1"]]
                    :resource-paths ["test/resources"]}}

  :plugins [[org.clojars.jj/bump "1.0.4"]
            [org.clojars.jj/strict-check "1.1.0"]
            [org.clojars.jj/bump-md "1.1.0"]])
