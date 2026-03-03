(ns jj.majavat.renderer.tests)

(defn default-test [v]
  (and (some? v) (not= v false)))

(defn is-even? [v]
  (and (integer? v) (even? v)))

(defn is-odd? [v]
  (and (integer? v) (odd? v)))
