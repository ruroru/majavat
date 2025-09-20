(ns jj.majavat.renderer
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [jj.majavat.renderer.filters :as filters]
            [jj.majavat.renderer.sanitizer :refer [sanitize]])
  (:import (java.io ByteArrayInputStream PushbackReader SequenceInputStream)
           (java.nio.charset Charset StandardCharsets)
           (java.time LocalDate)
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


(defn- evaluate-condition [condition context]
  (boolean (resolve-path context condition)))

(defn- escape-if-needed [val s]
  (if (nil? s) val (sanitize s val)))

(defn- ->str [v]
  (if (string? v)
    v
    (str v)))

(defn- apply-filter [v filter-obj]
  (let [filter-name (:filter-name filter-obj)
        filter-args (:args filter-obj)]
    (cond
      (string? v) (case filter-name
                    :upper-case (clojure.string/upper-case v)
                    :lower-case (clojure.string/lower-case v)
                    :capitalize (clojure.string/capitalize v)
                    :title-case (filters/title-case v)
                    :trim (clojure.string/trim v)
                    :upper-roman (filters/upper-roman v)
                    :int (filters/as-int v)
                    :long (filters/as-long v)

                    v)
      (keyword? v) (case filter-name
                     :name (name v)
                     v)
      (number? v) (case filter-name
                    :inc (inc v)
                    :dec (dec v)
                    :file-size-format (filters/file-size v)
                    v)
      (instance? LocalDate v) (case filter-name
                                :date (filters/->formatted-local-date v filter-args)
                                v)
      (nil? v) (case filter-name
                 :default (filters/get-default v filter-args))
      :else
      v)))

(defn- build-query-string [path context]
  (let [query-data (resolve-path context path)]
    (when (map? query-data)
      (let [sb (StringBuilder.)
            filtered-params (filter (fn [[_ v]] (not (nil? v))) query-data)]
        (when (seq filtered-params)
          (.append sb "?")
          (loop [params (seq filtered-params)
                 first? true]
            (when params
              (let [[k v] (first params)]
                (when-not first?
                  (.append sb "&"))
                (.append sb (name k))
                (.append sb "=")
                (.append sb (->str v))
                (recur (next params) false))))
          (.toString sb))))))

(defn- render-nodes [nodes context ^StringBuilder sb escape-conf]
  (doseq [node nodes]
    (case (:type node)
      :text
      (.append sb (:value node ""))

      :value-node
      (let [val (resolve-path context (:value node))
            filtered-val (if-let [filters (:filters node)]
                           (reduce (fn [v filter-obj]
                                     (apply-filter v filter-obj))
                                   val filters)
                           val)]
        (.append sb (-> filtered-val
                        ->str
                        (escape-if-needed (:sanitizer escape-conf)))))

      :query-string
      (when-let [query-str (build-query-string (:value node) context)]
        (.append sb query-str))

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
                               (reduce (fn [v filter-obj]
                                         (apply-filter v filter-obj))
                                       val filters)
                               val)
                resolved-value (-> filtered-val
                                   ->str
                                   (escape-if-needed (:sanitizer escape-conf)))]
            (cons (ByteArrayInputStream. (.getBytes ^String resolved-value ^Charset charset))
                  (render-nodes-to-stream-seq (rest nodes) context charset escape-conf)))

          :query-string
          (let [query-str (build-query-string (:value node) context)]
            (if query-str
              (cons (ByteArrayInputStream. (.getBytes ^String query-str ^Charset charset))
                    (render-nodes-to-stream-seq (rest nodes) context charset escape-conf))
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
