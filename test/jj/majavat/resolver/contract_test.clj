(ns jj.majavat.resolver.contract-test
  "All resolvers satisfy the TemplateResolver contract identically, so the same
   assertions are run against each one."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [jj.majavat.protocol.resolver :as template-resolver]
            [jj.majavat.resolver.fs :refer [->FsResolver]]
            [jj.majavat.resolver.resource :refer [->ResourceResolver]]
            [jj.majavat.resolver.string :refer [->StringResolver]])
  (:import (java.io Reader)))

(defn- read-all [^Reader reader]
  (when reader
    (with-open [r reader]
      (slurp r))))

(def ^:private cases
  (let [resource-path "deeply-nested-conditionals.txt"
        fs-path       "./test/resources/deeply-nested-conditionals.txt"]
    {"FsResolver"       {:resolver         (->FsResolver)
                         :existing-path    fs-path
                         :expected-content (slurp fs-path)
                         :missing-path     "./test/resources/does-not-exist.txt"}
     "ResourceResolver" {:resolver         (->ResourceResolver)
                         :existing-path    resource-path
                         :expected-content (slurp (io/resource resource-path))
                         :missing-path     "does/not/exist.txt"}
     "StringResolver"   {:resolver         (->StringResolver "home.html" "<h1>hello</h1>")
                         :existing-path    "home.html"
                         :expected-content "<h1>hello</h1>"
                         :missing-path     "other.html"}}))

(deftest template-exists?-contract
  (doseq [[label {:keys [resolver existing-path missing-path]}] cases]
    (testing label
      (is (true? (template-resolver/template-exists? resolver existing-path))
          "returns true for an existing path")
      (is (false? (template-resolver/template-exists? resolver missing-path))
          "returns false for a missing path"))))

(deftest open-reader-contract
  (doseq [[label {:keys [resolver existing-path expected-content missing-path]}] cases]
    (testing label
      (is (= expected-content
             (read-all (template-resolver/open-reader resolver existing-path)))
          "returns a reader over the content for an existing path")
      (is (nil? (template-resolver/open-reader resolver missing-path))
          "returns nil for a missing path"))))
