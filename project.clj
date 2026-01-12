(defproject org.clojars.jj/majavat "1.17.4"
  :description "Templating engine for clojure"
  :url "https://github.com/ruroru/majavat"
  :license {:name "EPL-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}

  :dependencies [[org.clojure/clojure "1.12.4"]
                 [org.clojure/tools.logging "1.3.1"]]

  :deploy-repositories [["clojars" {:url      "https://repo.clojars.org"
                                    :username :env/clojars_user
                                    :password :env/clojars_pass}]]
  :java-source-paths ["java-src"]
  :javac-options ["--release" "11"]

  :resource-paths ["src/resources"]
  :profiles {:benchmark {:source-paths   ["benchmark/src"]
                         :resource-paths ["benchmark/resources"]
                         :dependencies   [[selmer "1.12.70"]
                                          [hiccup "2.0.0"]]}
             :test      {:global-vars    {*warn-on-reflection* true}
                         :dependencies   [[ch.qos.logback/logback-classic "1.5.24"]
                                          [criterium "0.4.6"]
                                          [mock-clj "0.2.1"]]
                         :resource-paths ["test/resources"]}}

  :plugins [[org.clojars.jj/bump "1.0.4"]
            [lein-cloverage "1.2.4"]
            [org.clojars.jj/strict-check "1.1.0"]
            [org.clojars.jj/lein-git-tag "1.0.0"]
            [org.clojars.jj/bump-md "1.1.0"]])
