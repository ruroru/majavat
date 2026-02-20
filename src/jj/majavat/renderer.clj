(ns jj.majavat.renderer
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [jj.majavat.renderer.filters :as filters]
            [jj.majavat.renderer.sanitizer :refer [sanitize]])
  (:import (java.io PushbackReader)
           (java.net URLEncoder)
           (java.nio.charset Charset StandardCharsets)
           (java.time Instant)
           (java.util ArrayList)
           (jj.majavat.stream SequentialByteArrayInputStream)))

(defn- read-edn-resource [resource-path]
  (when-let [resource (io/resource resource-path)]
    (with-open [stream (.openStream resource)
                reader (io/reader stream)
                pushback-reader (PushbackReader. reader)]
      (edn/read pushback-reader))))

(defmacro ^:private reverse-call [a m]
  `(when-let [m# ~m]
     (m# ~a)))

(defn- local-get-in
  ([m a]
   (reverse-call a m))
  ([m a b]
   (-> m
       (reverse-call a)
       (reverse-call b)))
  ([m a b c]
   (-> m
       (reverse-call a)
       (reverse-call b)
       (reverse-call c)))
  ([m a b c d]
   (-> m
       (reverse-call a)
       (reverse-call b)
       (reverse-call c)
       (reverse-call d)))
  ([m a b c d e]
   (-> m
       (reverse-call a)
       (reverse-call b)
       (reverse-call c)
       (reverse-call d)
       (reverse-call e)))
  ([m a b c d e f]
   (-> m
       (reverse-call a)
       (reverse-call b)
       (reverse-call c)
       (reverse-call d)
       (reverse-call e)
       (reverse-call f))))

(defn- resolve-path [context path]
  (cond
    (not (vector? path)) (context path)
    (record? context) (get-in context path)
    :else (case (count path)
            1 (let [[a] path] (local-get-in context a))
            2 (let [[a b] path] (local-get-in context a b))
            3 (let [[a b c] path] (local-get-in context a b c))
            4 (let [[a b c d] path] (local-get-in context a b c d))
            5 (let [[a b c d e] path] (local-get-in context a b c d e))
            6 (let [[a b c d e f] path] (local-get-in context a b c d e f))
            (get-in context path))))


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
                (.append sb (.replace ^String (URLEncoder/encode ^String (->str v) "UTF-8") "+" "%20"))
                (recur (next params) false))))
          (.toString sb))))))

(defn- get-loop-context [context index count]
  (assoc context
    :loop {:total  count
           :index  index
           :first? (zero? index)
           :last?  (= index (dec count))}))

(defn- render-nodes [nodes context ^StringBuilder sb sanitizer]
  (doseq [node nodes]
    (case (node :type)
      :text
      (.append sb (node :value ""))

      :value-node
      (let [val (resolve-path context (node :value))
            filter-fn (node :filter-fn identity)
            filtered-val (filter-fn val)]
        (.append sb (-> filtered-val
                        ->str
                        (escape-if-needed sanitizer))))

      :query-string
      (when-let [query-str (build-query-string (node :value) context)]
        (.append sb query-str))

      :keyword-now
      (.append sb (filters/->formatted-instant (Instant/now) [(node :format)]))

      :variable-assignment
      (let [variable-name (node :variable-name)
            variable-value (node :variable-value)
            body (node :body)
            new-context (assoc context variable-name (resolve-path context variable-value))]
        (render-nodes body new-context sb sanitizer))

      :variable-declaration
      (let [variable-name (node :variable-name)
            variable-value (node :variable-value)
            body (node :body)
            new-context (assoc context variable-name variable-value)]
        (render-nodes body new-context sb sanitizer))

      :escape-block
      (let [body (node :body)
            new-sanitizer (node :sanitizer)]
        (render-nodes body context sb new-sanitizer))

      :for (let [identifier (node :identifier)
                 source-path (node :source)
                 body (node :body)
                 items (resolve-path context source-path)
                 item-count (count items)]
             (if (seq items)
               (loop [i 0
                      remaining (seq items)]
                 (when remaining
                   (let [item (first remaining)
                         loop-context (get-loop-context context i item-count)
                         new-context (assoc loop-context identifier item)]
                     (render-nodes body new-context sb sanitizer)
                     (recur (inc i) (next remaining)))))
               (when-let [when-empty (node :when-empty)]
                 (render-nodes when-empty context sb sanitizer))))

      :each
      (let [identifier (node :identifier)
            source-path (node :source)
            body (node :body)
            items (resolve-path context source-path)]
        (if (seq items)
          (loop [i 0
                 remaining (seq items)]
            (when remaining
              (let [item (first remaining)]
                (render-nodes body (assoc context identifier item) sb sanitizer)
                (recur (inc i) (next remaining)))))
          (when-let [when-empty (node :when-empty)]
            (render-nodes when-empty context sb sanitizer))))

      :if
      (let [condition (node :condition)
            when-true (node :when-true)
            when-false (node :when-false)
            is-negated (node :negate false)
            condition-result (boolean (resolve-path context condition))
            should-execute-true (if is-negated (not condition-result) condition-result)]
        (if should-execute-true
          (when (seq when-true) (render-nodes when-true context sb sanitizer))
          (when (seq when-false) (render-nodes when-false context sb sanitizer))))
      nil))
  sb)

(defn- render-nodes-to-bytes-vec
  ([nodes context charset sanitizer]
   (render-nodes-to-bytes-vec nodes context charset sanitizer (ArrayList. (count nodes))))
  ([nodes context ^Charset charset sanitizer ^ArrayList result]
   (loop [remaining nodes]
     (when (seq remaining)
       (let [node (first remaining)
             rest-nodes (rest remaining)]
         (case (node :type)
           :text
           (do
             (let [text (node :value "")]
               (when-not (empty? text)
                 (.add result (.getBytes ^String text charset))))
             (recur rest-nodes))

           :value-node
           (do
             (let [val (resolve-path context (node :value))
                   filter-fn (node :filter-fn identity)
                   resolved (-> val filter-fn ->str (escape-if-needed sanitizer))]
               (.add result (.getBytes ^String resolved charset)))
             (recur rest-nodes))

           :query-string
           (do
             (when-let [qs (build-query-string (node :value) context)]
               (.add result (.getBytes ^String qs charset)))
             (recur rest-nodes))

           :keyword-now
           (do
             (let [now-str (filters/->formatted-instant (Instant/now) [(node :format)])]
               (.add result (.getBytes ^String now-str charset)))
             (recur rest-nodes))

           :variable-declaration
           (do
             (let [new-ctx (assoc context (node :variable-name) (node :variable-value))]
               (render-nodes-to-bytes-vec (node :body) new-ctx charset sanitizer result))
             (recur rest-nodes))

           :variable-assignment
           (do
             (let [new-ctx (assoc context (node :variable-name) (resolve-path context (node :variable-value)))]
               (render-nodes-to-bytes-vec (node :body) new-ctx charset sanitizer result))
             (recur rest-nodes))

           :escape-block
           (do
             (let [new-sanitizer (node :sanitizer)]
               (render-nodes-to-bytes-vec (node :body) context charset new-sanitizer result))
             (recur rest-nodes))

           :for
           (do
             (let [items (resolve-path context (node :source))
                   total (count items)]
               (if (seq items)
                 (loop [i 0
                        item-remaining (seq items)]
                   (when item-remaining
                     (let [item (first item-remaining)
                           loop-ctx (get-loop-context context i total)
                           new-ctx (assoc loop-ctx (node :identifier) item)]
                       (render-nodes-to-bytes-vec (node :body) new-ctx charset sanitizer result)
                       (recur (inc i) (next item-remaining)))))
                 (when-let [when-empty (node :when-empty)]
                   (render-nodes-to-bytes-vec when-empty context charset sanitizer result))))
             (recur rest-nodes))

           :each
           (do
             (let [items (resolve-path context (node :source))]
               (if (seq items)
                 (loop [item-remaining (seq items)]
                   (when item-remaining
                     (let [item (first item-remaining)]
                       (render-nodes-to-bytes-vec (node :body) (assoc context (node :identifier) item) charset sanitizer result)
                       (recur (next item-remaining)))))
                 (when-let [when-empty (node :when-empty)]
                   (render-nodes-to-bytes-vec when-empty context charset sanitizer result))))
             (recur rest-nodes))

           :if
           (do
             (let [is-negated (node :negate false)
                   condition-result (resolve-path context (node :condition))
                   should-execute-true (if is-negated (not condition-result) condition-result)
                   branch (if should-execute-true :when-true :when-false)]
               (render-nodes-to-bytes-vec (node branch) context charset sanitizer result))
             (recur rest-nodes))

           (recur rest-nodes)))))
   result))




(defn- partial-render-nodes [nodes context sanitizer]
  (reduce
    (fn [acc node]
      (case (node :type)
        :text
        (conj acc node)

        :value-node
        (let [val (resolve-path context (node :value))]
          (if (some? val)
            (let [filter-fn (node :filter-fn identity)
                  filtered-val (filter-fn val)
                  resolved-str (-> filtered-val
                                   ->str
                                   (escape-if-needed sanitizer))]
              (conj acc {:type :text :value resolved-str}))
            (conj acc node)))

        :query-string
        (let [query-data (resolve-path context (node :value))]
          (if (some? query-data)
            (if-let [query-str (build-query-string (node :value) context)]
              (conj acc {:type :text :value query-str})
              acc)
            (conj acc node)))

        :variable-assignment
        (let [variable-name (node :variable-name)
              variable-value (node :variable-value)
              body (node :body)
              resolved-val (resolve-path context variable-value)]
          (if (some? resolved-val)
            (let [new-context (assoc context variable-name resolved-val)
                  rendered-body (partial-render-nodes body new-context sanitizer)]
              (into acc rendered-body))
            (conj acc (assoc node :body (partial-render-nodes body context sanitizer)))))

        :variable-declaration
        (let [variable-name (node :variable-name)
              variable-value (node :variable-value)
              body (node :body)
              new-context (assoc context variable-name variable-value)
              rendered-body (partial-render-nodes body new-context sanitizer)]
          (if (= rendered-body body)
            (conj acc node)
            (conj acc (assoc node :body rendered-body))))

        :escape-block
        (let [body (node :body)
              new-sanitizer (node :sanitizer)
              rendered-body (partial-render-nodes body context new-sanitizer)]
          (conj acc (assoc node :body rendered-body)))

        :for
        (let [identifier (node :identifier)
              source-path (node :source)
              body (node :body)
              items (resolve-path context source-path)]
          (if (some? items)
            (if (seq items)
              (let [item-count (count items)]
                (loop [i 0
                       remaining (seq items)
                       result acc]
                  (if remaining
                    (let [item (first remaining)
                          loop-context (get-loop-context context i item-count)
                          new-context (assoc loop-context identifier item)
                          rendered (partial-render-nodes body new-context sanitizer)]
                      (recur (inc i) (next remaining) (into result rendered)))
                    result)))
              (if-let [when-empty (node :when-empty)]
                (into acc (partial-render-nodes when-empty context sanitizer))
                acc))
            (conj acc (cond-> (assoc node :body (partial-render-nodes body context sanitizer))
                              (node :when-empty) (assoc :when-empty (partial-render-nodes (node :when-empty) context sanitizer))))))

        :each
        (let [identifier (node :identifier)
              source-path (node :source)
              body (node :body)
              items (resolve-path context source-path)]
          (if (some? items)
            (if (seq items)
              (let [item-count (count items)]
                (loop [i 0
                       remaining (seq items)
                       result acc]
                  (if remaining
                    (let [item (first remaining)
                          loop-context (get-loop-context context i item-count)
                          new-context (assoc loop-context identifier item)
                          rendered (partial-render-nodes body new-context sanitizer)]
                      (recur (inc i) (next remaining) (into result rendered)))
                    result)))
              (if-let [when-empty (node :when-empty)]
                (into acc (partial-render-nodes when-empty context sanitizer))
                acc))
            (conj acc (cond-> (assoc node :body (partial-render-nodes body context sanitizer))
                              (node :when-empty) (assoc :when-empty (partial-render-nodes (node :when-empty) context sanitizer))))))

        :if
        (let [condition (node :condition)
              condition-val (resolve-path context condition)
              is-negated (node :negate false)]
          (if (some? condition-val)
            (let [condition-result (boolean condition-val)
                  should-execute-true (if is-negated (not condition-result) condition-result)]
              (if should-execute-true
                (into acc (partial-render-nodes (node :when-true) context sanitizer))
                (into acc (partial-render-nodes (node :when-false) context sanitizer))))
            (conj acc (assoc node
                        :when-true (partial-render-nodes (node :when-true) context sanitizer)
                        :when-false (partial-render-nodes (node :when-false) context sanitizer)))))

        (conj acc node)))
    []
    nodes))

(defn- optimize-ast [nodes]
  "Merge consecutive text nodes and remove empty text nodes"
  (reduce
    (fn [acc node]
      (if (= :text (node :type))
        (let [last-node (peek acc)]
          (cond
            (empty? (node :value))
            acc

            (and last-node (= :text (last-node :type)))
            (conj (pop acc)
                  (assoc last-node :value (str (last-node :value) (node :value))))

            :else
            (conj acc node)))
        (conj acc node)))
    []
    nodes))

(defprotocol RenderTarget
  (render [this template context sanitizer]))

(defrecord StringRenderer []
  RenderTarget
  (render [this template context sanitizer]
    (if-not (map? template)
      (.toString ^StringBuilder (render-nodes template context (StringBuilder.) sanitizer))
      (render (->StringRenderer) (read-edn-resource "jj/majavat/error-template.edn") template sanitizer))))

(defrecord InputStreamRenderer []
  RenderTarget
  (render [this template context sanitizer]
    (if-not (map? template)
      (let [byte-arrays (render-nodes-to-bytes-vec template context StandardCharsets/UTF_8 sanitizer)]
        (SequentialByteArrayInputStream. byte-arrays))
      (render (->InputStreamRenderer) (read-edn-resource "jj/majavat/error-template.edn") template sanitizer))))

(defrecord PartialRenderer []
  RenderTarget
  (render [this template context sanitizer]
    (if-not (map? template)
      (-> (partial-render-nodes template context sanitizer)
          optimize-ast)
      (render (->PartialRenderer) (read-edn-resource "jj/majavat/error-template.edn") template sanitizer))))
