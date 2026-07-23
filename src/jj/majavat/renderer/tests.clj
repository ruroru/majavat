(ns jj.majavat.renderer.tests)

(defn default-test [v]
  (and (some? v) (not= v false)))

(defn is-even? [v]
  (and (integer? v) (even? v)))

(defn is-odd? [v]
  (and (integer? v) (odd? v)))

(defn is-equal? [v arg]
  (= v arg))

(defn is-ge? [v arg]
  (and (number? v) (>= v arg)))

(defn is-gt? [v arg]
  (and (number? v) (> v arg)))

(defn is-le? [v arg]
  (and (number? v) (<= v arg)))

(defn is-lt? [v arg]
  (and (number? v) (< v arg)))

(defn is-empty? [v]
  (cond
    (nil? v) true
    (string? v) (zero? (.length ^String v))
    (coll? v) (empty? v)
    :else false))
