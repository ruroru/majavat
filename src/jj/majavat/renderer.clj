(ns jj.majavat.renderer
  (:require [clojure.pprint :as pprint]
            [jj.majavat.protocol.error-handler :as error]
            [jj.majavat.protocol.renderer.render-target :as render-target]
            [jj.majavat.string-builder :as sb])
  (:import (java.nio.charset Charset StandardCharsets)
           (java.util ArrayList)
           (java.util.concurrent.atomic AtomicInteger AtomicLong)
           (jj.majavat.stream SequentialByteArrayInputStream)))

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

(defn- loop-context
  "The context an iteration renders with when the body reads `loop`."
  [context identifier item build-loop index item-count]
  (assoc (assoc context :loop (build-loop index item-count)) identifier item))

(declare render-nodes)

(defn- render-items
  [node context items sb]
  (let [body (node :body)
        identifier (node :identifier)]
    (loop [remaining (seq items)]
      (when remaining
        (render-nodes body (assoc context identifier (first remaining)) sb)
        (recur (next remaining))))))

(defn- render-items-with-loop
  [node context items build-loop sb]
  (let [body (node :body)
        identifier (node :identifier)
        item-count (num (count items))]
    (loop [i 0
           remaining (seq items)]
      (when remaining
        (render-nodes body (loop-context context identifier (first remaining) build-loop i item-count) sb)
        (recur (inc i) (next remaining))))))

(defn- render-for
  [node context sb]
  (let [items ((node :source) context)]
    (if (seq items)
      (if-let [build-loop (node :loop)]
        (render-items-with-loop node context items build-loop sb)
        (render-items node context items sb))
      (when-let [when-empty (node :when-empty)]
        (render-nodes when-empty context sb)))))

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
            :if                   (render-nodes (branch-body node context) context sb)
            :debug                (debug-output node context)
            nil)
          (recur (inc n)))
        sb))))

(defn- add-bytes
  [^ArrayList result ^String s ^Charset charset]
  (when (pos? (.length s))
    (.add result (.getBytes s charset))))

(declare render-nodes-to-bytes)

(defn- items-bytes
  [node context items ^Charset charset ^ArrayList result]
  (let [body (node :body)
        identifier (node :identifier)]
    (loop [remaining (seq items)]
      (when remaining
        (render-nodes-to-bytes body (assoc context identifier (first remaining)) charset result)
        (recur (next remaining))))))

(defn- items-with-loop-bytes
  [node context items build-loop ^Charset charset ^ArrayList result]
  (let [body (node :body)
        identifier (node :identifier)
        item-count (num (count items))]
    (loop [i 0
           remaining (seq items)]
      (when remaining
        (render-nodes-to-bytes body (loop-context context identifier (first remaining) build-loop i item-count)
                               charset result)
        (recur (inc i) (next remaining))))))

(defn- for-bytes
  [node context ^Charset charset ^ArrayList result]
  (let [items ((node :source) context)]
    (if (seq items)
      (if-let [build-loop (node :loop)]
        (items-with-loop-bytes node context items build-loop charset result)
        (items-bytes node context items charset result))
      (when-let [when-empty (node :when-empty)]
        (render-nodes-to-bytes when-empty context charset result)))))

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
            :if                   (render-nodes-to-bytes (branch-body node context) context charset result)
            :debug                (debug-output node context)
            nil)
          (recur (inc n)))
        result))))

(defn- bind-branches
  "Binds an if's branches to a pre-render context. A branch the context decides
   is dropped when false, and when true becomes the else body, ending the chain;
   the rest are kept, with what the context already holds baked in, since the
   render context won't have it."
  [branches else-body context]
  (loop [kept []
         remaining (seq branches)]
    (if remaining
      (let [[condition body] (first remaining)
            bound ((:resolve condition) context)]
        (cond
          (nil? bound) (recur (conj kept [condition body]) (next remaining))
          (map? bound) (recur (conj kept [bound body]) (next remaining))
          (if (:negate condition) (not bound) bound) [kept body]
          :else (recur kept (next remaining))))
      [kept else-body])))

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
              build-loop (node :loop)
              body (node :body)
              items ((node :source) context)]
          (if (some? items)
            (if (seq items)
              (let [item-count (num (count items))]
                (loop [i 0
                       remaining (seq items)
                       result acc]
                  (if remaining
                    (let [item (first remaining)
                          new-context (if build-loop
                                        (loop-context context identifier item build-loop i item-count)
                                        (assoc context identifier item))
                          rendered (partial-render-nodes body new-context)]
                      (recur (inc i) (next remaining) (into result rendered)))
                    result)))
              (if-let [when-empty (node :when-empty)]
                (into acc (partial-render-nodes when-empty context))
                acc))
            (conj acc (cond-> (assoc node :body (partial-render-nodes body context))
                              (node :when-empty) (assoc :when-empty (partial-render-nodes (node :when-empty) context))))))

        :if
        (let [[branches else-body] (bind-branches (node :branches) (node :else) context)]
          (if (seq branches)
            (conj acc (assoc node
                        :branches (mapv (fn [[condition body]]
                                          [condition (partial-render-nodes body context)])
                                        branches)
                        :else (partial-render-nodes else-body context)))
            (into acc (partial-render-nodes else-body context))))

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

(def default-capacity 128)

(def default-chunk-capacity 16)

(deftype StringRenderer [^AtomicInteger capacity]
  render-target/RenderTarget
  (render [this template context error-handler]
    (if-not (map? template)
      (let [cap (.get capacity)
            ^String s (sb/build (render-nodes template context
                                              (sb/create-string-builder cap)))
            len (.length s)]
        (let [current-cap (.get capacity)]
          (when (> len current-cap)
            (.set capacity len)))
        s)
      (error/handle-error error-handler this template))))

(defn ->StringRenderer
  ([] (->StringRenderer default-capacity))
  ([capacity] (StringRenderer. (if (instance? AtomicInteger capacity) capacity (AtomicInteger. (long capacity))))))

(deftype InputStreamRenderer [^AtomicInteger capacity]
  render-target/RenderTarget
  (render [this template context error-handler]
    (if-not (map? template)
      (let [^ArrayList result (render-nodes-to-bytes template context StandardCharsets/UTF_8
                                                     (ArrayList. (int (.get capacity))))
            len (.size result)]
        (let [current-cap (.get capacity)]
          (when (> len current-cap)
            (.set capacity len)))
        (SequentialByteArrayInputStream. result))
      (error/handle-error error-handler this template))))

(defn ->InputStreamRenderer
  ([] (->InputStreamRenderer default-chunk-capacity))
  ([capacity]
   (InputStreamRenderer.
     (if (instance? AtomicInteger capacity)
       capacity
       (AtomicInteger. (int capacity))))))

(defrecord PartialRenderer []
  render-target/RenderTarget
  (render [this template context error-handler]
    (if-not (map? template)
      (-> (partial-render-nodes template context)
          optimize-ast)
      (error/handle-error error-handler this template))))