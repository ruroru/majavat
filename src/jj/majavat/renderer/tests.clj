(ns jj.majavat.renderer.tests)

(defn default-test [v]
  (and (some? v) (not= v false)))

(defn is-even? [v]
  (and (integer? v) (even? v)))

(defn is-odd? [v]
  (and (integer? v) (odd? v)))

(defn is-empty? [v]
  (cond
    (nil? v) true
    (string? v) (zero? (.length ^String v))
    (coll? v) (empty? v)
    :else false))
