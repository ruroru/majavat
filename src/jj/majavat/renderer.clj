(ns jj.majavat.renderer
  (:require [clojure.pprint :as pprint]
            [jj.majavat.protocol.error-handler :as error]
            [jj.majavat.protocol.renderer.render-target :as render-target]
            [jj.majavat.string-builder :as sb])
  (:import (java.nio.charset Charset StandardCharsets)
           (java.util ArrayList)
           (jj.majavat.stream SequentialByteArrayInputStream)))

(defn- ->str [v]
  (if (string? v)
    v
    (str v)))

(defn- get-loop-context [context index count]
  (assoc context
    :loop {:total  count
           :index  index
           :first? (zero? index)
           :last?  (= index (dec count))}))

(defn- evaluate-condition
  [condition context]
  (let [eval-fn (or (:evaluation-function condition) boolean)
        result (boolean (eval-fn context))]
    (if (:negate condition) (not result) result)))

(defn- debug-output [node context]
  (let [target (node :target)]
    (if (= :default target)
      (pprint/pprint context)
      (if-let [w (get context target)]
        (binding [*out* w]
          (pprint/pprint (dissoc context target)))
        (pprint/pprint context)))))

(defn- assignment-context
  [node context]
  (assoc context (node :variable-name) ((node :variable-value) context)))

(defn- declaration-context
  [node context]
  (assoc context (node :variable-name) (node :variable-value)))

(defn- branch-body
  [node context]
  (or (reduce
        (fn [_ [condition body]]
          (when (evaluate-condition condition context)
            (reduced body)))
        nil
        (node :branches))
      (node :else)))

(declare render-nodes)

(defn- render-for
  [node context sb]
  (let [items ((node :source) context)]
    (if (seq items)
      (let [body (node :body)
            identifier (node :identifier)
            item-count (count items)]
        (loop [i 0
               remaining (seq items)]
          (when remaining
            (let [loop-context (get-loop-context context i item-count)]
              (render-nodes body (assoc loop-context identifier (first remaining)) sb)
              (recur (inc i) (next remaining))))))
      (when-let [when-empty (node :when-empty)]
        (render-nodes when-empty context sb)))))

(defn- render-each
  [node context sb]
  (let [items ((node :source) context)]
    (if (seq items)
      (let [body (node :body)
            identifier (node :identifier)]
        (loop [remaining (seq items)]
          (when remaining
            (render-nodes body (assoc context identifier (first remaining)) sb)
            (recur (next remaining)))))
      (when-let [when-empty (node :when-empty)]
        (render-nodes when-empty context sb)))))

(defn- render-translation
  [node context sb]
  (when-let [translated ((node :trans-fn) (get context :locale))]
    (sb/append sb (->str translated))))

(defn- render-nodes
  [nodes context sb]
  (let [node-count (count nodes)]
    (loop [n 0]
      (if (< n node-count)
        (let [node (nth nodes n)]
          (case (node :type)
            :text                 (sb/append sb (node :value ""))
            :value-node           ((node :render-fn) context sb nil)
            :variable-assignment  (render-nodes (node :body) (assignment-context node context) sb)
            :variable-declaration (render-nodes (node :body) (declaration-context node context) sb)
            :for                  (render-for node context sb)
            :each                 (render-each node context sb)
            :if                   (render-nodes (branch-body node context) context sb)
            :translation          (render-translation node context sb)
            :debug                (debug-output node context)
            nil)
          (recur (inc n)))
        sb))))

(defn- add-bytes
  [^ArrayList result ^String s ^Charset charset]
  (when (pos? (.length s))
    (.add result (.getBytes s charset))))

(declare render-nodes-to-bytes)

(defn- for-bytes
  [node context ^Charset charset ^ArrayList result]
  (let [items ((node :source) context)]
    (if (seq items)
      (let [body (node :body)
            identifier (node :identifier)
            item-count (count items)]
        (loop [i 0
               remaining (seq items)]
          (when remaining
            (let [loop-context (get-loop-context context i item-count)]
              (render-nodes-to-bytes body (assoc loop-context identifier (first remaining))
                                     charset result)
              (recur (inc i) (next remaining))))))
      (when-let [when-empty (node :when-empty)]
        (render-nodes-to-bytes when-empty context charset result)))))

(defn- each-bytes
  [node context ^Charset charset ^ArrayList result]
  (let [items ((node :source) context)]
    (if (seq items)
      (let [body (node :body)
            identifier (node :identifier)]
        (loop [remaining (seq items)]
          (when remaining
            (render-nodes-to-bytes body (assoc context identifier (first remaining))
                                   charset result)
            (recur (next remaining)))))
      (when-let [when-empty (node :when-empty)]
        (render-nodes-to-bytes when-empty context charset result)))))

(defn- translation-bytes
  [node context ^Charset charset ^ArrayList result]
  (when-let [translated ((node :trans-fn) (get context :locale))]
    (add-bytes result (->str translated) charset)))

(defn- render-nodes-to-bytes
  [nodes context ^Charset charset ^ArrayList result]
  (let [node-count (count nodes)]
    (loop [n 0]
      (if (< n node-count)
        (let [node (nth nodes n)]
          (case (node :type)
            :text                 (add-bytes result (node :value "") charset)
            :value-node           (add-bytes result ((node :render-fn) context) charset)
            :variable-assignment  (render-nodes-to-bytes (node :body) (assignment-context node context) charset result)
            :variable-declaration (render-nodes-to-bytes (node :body) (declaration-context node context) charset result)
            :for                  (for-bytes node context charset result)
            :each                 (each-bytes node context charset result)
            :if                   (render-nodes-to-bytes (branch-body node context) context charset result)
            :translation          (translation-bytes node context charset result)
            :debug                (debug-output node context)
            nil)
          (recur (inc n)))
        result))))

(defn- partial-render-nodes [nodes context]
  (reduce
    (fn [acc node]
      (case (node :type)
        :text
        (conj acc node)

        :value-node
        (let [render-fn (node :render-fn)
              raw (render-fn context ::raw)]
          (if (some? raw)
            (conj acc {:type :text :value (render-fn context)})
            (conj acc node)))

        :variable-assignment
        (let [variable-name (node :variable-name)
              variable-value (node :variable-value)
              body (node :body)
              resolved-val (variable-value context)]
          (if (some? resolved-val)
            (let [new-context (assoc context variable-name resolved-val)
                  rendered-body (partial-render-nodes body new-context)]
              (into acc rendered-body))
            (conj acc (assoc node :body (partial-render-nodes body context)))))

        :variable-declaration
        (let [variable-name (node :variable-name)
              variable-value (node :variable-value)
              body (node :body)
              new-context (assoc context variable-name variable-value)
              rendered-body (partial-render-nodes body new-context)]
          (if (= rendered-body body)
            (conj acc node)
            (conj acc (assoc node :body rendered-body))))

        :for
        (let [identifier (node :identifier)
              body (node :body)
              items ((node :source) context)]
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
                          rendered (partial-render-nodes body new-context)]
                      (recur (inc i) (next remaining) (into result rendered)))
                    result)))
              (if-let [when-empty (node :when-empty)]
                (into acc (partial-render-nodes when-empty context))
                acc))
            (conj acc (cond-> (assoc node :body (partial-render-nodes body context))
                              (node :when-empty) (assoc :when-empty (partial-render-nodes (node :when-empty) context))))))

        :each
        (let [identifier (node :identifier)
              body (node :body)
              items ((node :source) context)]
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
                          rendered (partial-render-nodes body new-context)]
                      (recur (inc i) (next remaining) (into result rendered)))
                    result)))
              (if-let [when-empty (node :when-empty)]
                (into acc (partial-render-nodes when-empty context))
                acc))
            (conj acc (cond-> (assoc node :body (partial-render-nodes body context))
                              (node :when-empty) (assoc :when-empty (partial-render-nodes (node :when-empty) context))))))

        :if
        (let [branches (node :branches)
              else-body (node :else)
              first-unresolved (reduce
                                 (fn [acc [condition body]]
                                   (if (some? ((:resolve condition) context))
                                     (if (evaluate-condition condition context)
                                       (reduced {:resolved true :body body})
                                       acc)
                                     (reduced {:resolved false})))
                                 nil
                                 branches)]
          (cond
            (and first-unresolved (:resolved first-unresolved))
            (into acc (partial-render-nodes (:body first-unresolved) context))

            (and (nil? first-unresolved))
            (into acc (partial-render-nodes else-body context))

            :else
            (conj acc (assoc node
                        :branches (mapv (fn [[condition body]]
                                          [condition (partial-render-nodes body context)])
                                        branches)
                        :else (partial-render-nodes else-body context)))))

        :translation
        (let [trans-fn (node :trans-fn)
              locale (get context :locale)]
          (if-let [translated (trans-fn locale)]
            (conj acc {:type :text :value (->str translated)})
            (conj acc node)))

        :debug
        (do
          (debug-output node context)
          acc)

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

(defrecord StringRenderer []
  render-target/RenderTarget
  (render [this template context error-handler]
    (if-not (map? template)
      (sb/build (render-nodes template context (sb/create-string-builder)))
      (error/handle-error error-handler this template))))

(defrecord InputStreamRenderer []
  render-target/RenderTarget
  (render [this template context error-handler]
    (if-not (map? template)
      (SequentialByteArrayInputStream.
        (render-nodes-to-bytes template context StandardCharsets/UTF_8
                               (ArrayList. (count template))))
      (error/handle-error error-handler this template))))

(defrecord PartialRenderer []
  render-target/RenderTarget
  (render [this template context error-handler]
    (if-not (map? template)
      (-> (partial-render-nodes template context)
          optimize-ast)
      (error/handle-error error-handler this template))))