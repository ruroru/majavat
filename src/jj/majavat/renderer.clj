(ns jj.majavat.renderer
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [jj.majavat.renderer.escape :as rops])
  (:import (java.io ByteArrayInputStream SequenceInputStream PushbackReader)
           (java.nio.charset Charset StandardCharsets)
           (java.util Collections)))

(defn- read-edn-resource [resource-path]
  (when-let [resource (io/resource resource-path)]
    (with-open [stream (.openStream resource)
                reader (io/reader stream)
                pushback-reader (PushbackReader. reader)]
      (edn/read pushback-reader))))

(defn- resolve-path [context path]
  (if (vector? path)
    (get-in context path)
    (get context path)))

(defn- title-case [s]
  (let [sb (StringBuilder.)
        char-array (char-array s)]
    (loop [index 0 new-word? true]
      (if (< index (count char-array))
        (let [c (aget char-array index)]
          (if (or (Character/isWhitespace c) (= c \-))
            (do
              (.append sb c)
              (recur (inc index) true))
            (do
              (if new-word?
                (.append sb (Character/toUpperCase c))
                (.append sb (Character/toLowerCase c)))
              (recur (inc index) false))))
        (.toString sb)))))

(defn- evaluate-condition [condition context]
  (boolean (resolve-path context condition)))

(defn- escape-if-needed [val escaper]
  (if (nil? escaper)
    val
    (rops/escape escaper val)))

(defn- ->str [v]
  (if (string? v)
    v
    (str v)))

(defn- upper-roman [v]
  [v]
  (when v
    (let [roman-pattern #"(?i)\b(?=[mdclxvi])M{0,4}(?:CM|CD|D?C{0,3})(?:XC|XL|L?X{0,3})(?:IX|IV|V?I{0,3})\b"]
      (str/replace v roman-pattern str/upper-case))))

(defn- apply-filter [v filter-name]
  (cond
    (string? v) (case filter-name
                  :upper-case (clojure.string/upper-case v)
                  :lower-case (clojure.string/lower-case v)
                  :capitalize (clojure.string/capitalize v)
                  :title-case (title-case v)
                  :trim (clojure.string/trim v)
                  :upper-roman (upper-roman v)
                  v)
    (keyword? v) (case filter-name
                   :name (name v)
                   v)
    (number? v) (case filter-name
                  :inc (inc v)
                  :dec (dec v)
                  v)
    :else
    v))

(defn- render-nodes [nodes context ^StringBuilder sb escape-conf]
  (doseq [node nodes]
    (case (:type node)
      :text
      (.append sb (:value node ""))

      :value-node
      (let [val (resolve-path context (:value node))
            filtered-val (if-let [filters (:filters node)]
                           (reduce (fn [v filter-name]
                                     (apply-filter v filter-name))
                                   val filters)
                           val)]
        (.append sb (-> filtered-val
                        ->str
                        (escape-if-needed (:character-escaper escape-conf)))))
      :variable-assignment
      (let [variable-name (:variable-name node)
            variable-value (:variable-value node)
            body (:body node)
            new-context (assoc context variable-name (resolve-path context variable-value))]
        (render-nodes body new-context sb escape-conf))

      :variable-declaration
      (let [variable-name (:variable-name node)
            variable-value (:variable-value node)
            body (:body node)
            new-context (assoc context variable-name variable-value)]
        (render-nodes body new-context sb escape-conf))

      :for (let [identifier (:identifier node)
                 source-path (:source node)
                 body (:body node)
                 items (resolve-path context source-path)]
             (doseq [[index item] (map-indexed vector items)]
               (let [loop-context (assoc context
                                    :loop {:total  (count items)
                                           :index  index
                                           :first? (zero? index)
                                           :last?  (= index (dec (count items)))})
                     new-context (assoc loop-context identifier item)]
                 (render-nodes body new-context sb escape-conf))))

      :if
      (let [condition (:condition node)
            when-true (:when-true node)
            when-false (:when-false node)]
        (if (evaluate-condition condition context)
          (when (seq when-true) (render-nodes when-true context sb escape-conf))
          (when (seq when-false) (render-nodes when-false context sb escape-conf))))

      :if-not
      (let [condition (:condition node)
            when-true (:when-true node)
            when-false (:when-false node)]
        (if (not (evaluate-condition condition context))
          (when (seq when-true) (render-nodes when-true context sb escape-conf))
          (when (seq when-false) (render-nodes when-false context sb escape-conf))))
      nil))
  sb)

(defn render [template context escape-conf]
  (if-not (map? template)
    (.toString ^StringBuilder (render-nodes template context (StringBuilder.) escape-conf))
    (render (read-edn-resource "error-template.edn") template escape-conf)))

(defn- render-nodes-to-stream-seq [nodes context charset escape-conf]
  (lazy-seq
    (when (seq nodes)
      (let [node (first nodes)]
        (case (:type node)
          :text
          (let [text (:value node "")]
            (if (empty? text)
              (render-nodes-to-stream-seq (rest nodes) context charset escape-conf)
              (cons (ByteArrayInputStream. (.getBytes ^String text ^Charset charset))
                    (render-nodes-to-stream-seq (rest nodes) context charset escape-conf))))

          :value-node
          (let [val (resolve-path context (:value node))
                filtered-val (if-let [filters (:filters node)]
                               (reduce (fn [v filter-name]
                                         (apply-filter v filter-name))
                                       val filters)
                               val)
                resolved-value (-> filtered-val
                                   ->str
                                   (escape-if-needed (:character-escaper escape-conf)))]
            (cons (ByteArrayInputStream. (.getBytes ^String resolved-value ^Charset charset))
                  (render-nodes-to-stream-seq (rest nodes) context charset escape-conf)))

          :variable-declaration
          (let [variable-name (:variable-name node)
                variable-value (:variable-value node)
                body (:body node)
                new-context (assoc context variable-name variable-value)]
            (concat (render-nodes-to-stream-seq body new-context charset escape-conf)
                    (render-nodes-to-stream-seq (rest nodes) context charset escape-conf)))
          :variable-assignment
          (let [variable-name (:variable-name node)
                variable-value (:variable-value node)
                body (:body node)
                new-context (assoc context variable-name (resolve-path context variable-value))]
            (concat (render-nodes-to-stream-seq body new-context charset escape-conf)
                    (render-nodes-to-stream-seq (rest nodes) context charset escape-conf)))

          :for
          (let [identifier (:identifier node)
                source-path (:source node)
                body (:body node)
                items (resolve-path context source-path)
                for-streams (mapcat (fn [index item]
                                      (let [loop-context (assoc context
                                                           :loop {:total  (count items)
                                                                  :index  index
                                                                  :first? (zero? index)
                                                                  :last?  (= index (dec (count items)))})]
                                        (render-nodes-to-stream-seq
                                          body
                                          (assoc loop-context identifier item)
                                          charset escape-conf)))
                                    (range)
                                    items)]
            (concat for-streams
                    (render-nodes-to-stream-seq (rest nodes) context charset escape-conf)))

          :if
          (let [condition (:condition node)
                when-true (:when-true node)
                when-false (:when-false node)]
            (if (evaluate-condition condition context)
              (if when-true
                (concat (render-nodes-to-stream-seq when-true context charset escape-conf)
                        (render-nodes-to-stream-seq (rest nodes) context charset escape-conf))
                (render-nodes-to-stream-seq (rest nodes) context charset escape-conf))
              (if when-false
                (concat (render-nodes-to-stream-seq when-false context charset escape-conf)
                        (render-nodes-to-stream-seq (rest nodes) context charset escape-conf))
                (render-nodes-to-stream-seq (rest nodes) context charset escape-conf))))
          (render-nodes-to-stream-seq (rest nodes) context charset escape-conf))))))

(defn render-is
  ([template context escape-conf]
   (render-is template context StandardCharsets/UTF_8 escape-conf))
  ([template context charset escape-conf]
   (if-not (map? template)
     (let [stream-seq (render-nodes-to-stream-seq template context charset escape-conf)
           enumeration (Collections/enumeration stream-seq)]
       (SequenceInputStream. enumeration))
     (ByteArrayInputStream. (.getBytes ^String (render (read-edn-resource "error-template.edn") template escape-conf))))))


(defn pre-render [render-instructions context]
  (let [has-text-nodes? (some #(= :text (:type %)) render-instructions)]
    (reduce (fn [acc instruction]
              (case (:type instruction)
                :text
                (let [last-instruction (peek acc)]
                  (if (and last-instruction (= :text (:type last-instruction)))
                    (conj (pop acc)
                          (assoc last-instruction
                            :value (str (:value last-instruction) (:value instruction))))
                    (conj acc instruction)))

                :value-node
                (let [value-key (:value instruction)]
                  (if (contains? context value-key)
                    (if has-text-nodes?
                      (let [resolved-value (get context value-key)
                            last-instruction (peek acc)]
                        (if (and last-instruction (= :text (:type last-instruction)))
                          (conj (pop acc)
                                (assoc last-instruction
                                  :value (str (:value last-instruction) resolved-value)))
                          (conj acc {:type :text :value resolved-value})))
                      acc)
                    (conj acc instruction)))
                (conj acc instruction)))
            [] render-instructions)))
