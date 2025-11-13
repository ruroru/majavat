(ns jj.majavat.renderer
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [jj.majavat.renderer.filters :as filters]
            [jj.majavat.renderer.sanitizer :refer [sanitize]])
  (:import (java.io ByteArrayInputStream PushbackReader SequenceInputStream)
           (java.net URLEncoder)
           (java.nio.charset Charset StandardCharsets)
           (java.time Instant)
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


(defn- remove-nil [[_ v]]
  (not (nil? v)))

(defn- build-query-string [path context]
  (let [query-data (resolve-path context path)]
    (when (map? query-data)
      (let [sb (StringBuilder.)
            filtered-params (filter remove-nil query-data)]
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
                (.append sb (URLEncoder/encode ^String (->str v) "UTF-8"))
                (recur (next params) false))))
          (.toString sb))))))

(defn- get-loop-context [context index count]
  (assoc context
    :loop {:total  count
           :index  index
           :first? (zero? index)
           :last?  (= index (dec count))}))

(defn- render-nodes [nodes context ^StringBuilder sb escape-conf]
  (doseq [node nodes]
    (case (:type node)
      :text
      (.append sb (:value node ""))

      :value-node
      (let [val (resolve-path context (:value node))
            filter-fn (get node :filter-fn identity)
            filtered-val (filter-fn val)]
        (.append sb (-> filtered-val
                        ->str
                        (escape-if-needed (:sanitizer escape-conf)))))

      :query-string
      (when-let [query-str (build-query-string (:value node) context)]
        (.append sb query-str))

      :keyword-now
      (.append sb (filters/->formatted-instant (Instant/now) [(get node :format)]))

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
               (let [loop-context (get-loop-context context index (count items))
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
                filter-fn (get node :filter-fn identity)
                filtered-val (filter-fn val)
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
                                      (let [loop-context (get-loop-context context index (count items))]
                                        (render-nodes-to-stream-seq body (assoc loop-context identifier item) charset escape-conf)))
                                    (range)
                                    items)]
            (concat for-streams
                    (render-nodes-to-stream-seq (rest nodes) context charset escape-conf)))

          :keyword-now
          (let [now-str (filters/->formatted-instant (Instant/now) [(get node :format)])]
            (cons (ByteArrayInputStream. (.getBytes ^String now-str ^Charset charset))
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

          :if-not
          (let [condition (:condition node)
                when-true (:when-true node)
                when-false (:when-false node)]
            (if (not (evaluate-condition condition context))
              (if when-true
                (concat (render-nodes-to-stream-seq when-true context charset escape-conf)
                        (render-nodes-to-stream-seq (rest nodes) context charset escape-conf))
                (render-nodes-to-stream-seq (rest nodes) context charset escape-conf))
              (if when-false
                (concat (render-nodes-to-stream-seq when-false context charset escape-conf)
                        (render-nodes-to-stream-seq (rest nodes) context charset escape-conf))
                (render-nodes-to-stream-seq (rest nodes) context charset escape-conf))))

          (render-nodes-to-stream-seq (rest nodes) context charset escape-conf))))))


(defn- process-instruction [context has-text-nodes? acc instruction]
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

(defn pre-render [render-instructions context]
  (let [has-text-nodes? (some #(= :text (:type %)) render-instructions)]
    (reduce (partial process-instruction context has-text-nodes?)
            []
            render-instructions)))

(defprotocol Renderer
  (render [this template context]))

(defrecord StringRenderer [config]
  Renderer
  (render [this template context]
    (if-not (map? template)
      (.toString ^StringBuilder (render-nodes template context (StringBuilder.) (:config this)))
      (render (->StringRenderer {}) (read-edn-resource "error-template.edn") template))))

(defrecord InputStreamRenderer [config]
  Renderer
  (render [this template context]
    (if-not (map? template)
      (let [stream-seq (render-nodes-to-stream-seq template context StandardCharsets/UTF_8 (:config this))
            enumeration (Collections/enumeration stream-seq)]
        (SequenceInputStream. enumeration))
      (render (->InputStreamRenderer {}) (read-edn-resource "error-template.edn") template))))