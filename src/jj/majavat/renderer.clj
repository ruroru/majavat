(ns jj.majavat.renderer
  (:require [clojure.pprint :as pprint]
            [jj.majavat.parser :as parser]
            [jj.majavat.protocol.error-handler :as error]
            [jj.majavat.protocol.renderer.render-target :as render-target]
            [jj.majavat.string-builder :as sb])
  (:import (clojure.lang IReduceInit)
           (java.nio.charset Charset StandardCharsets)
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
        raw-val (parser/resolve-path context (:condition condition))
        result (boolean (eval-fn raw-val))
        ]
    (if (:negate condition) (not result) result)))

(defn- debug-output [node context]
  (let [target (node :target)]
    (if (= :default target)
      (pprint/pprint context)
      (if-let [w (get context target)]
        (binding [*out* w]
          (pprint/pprint (dissoc context target)))
        (pprint/pprint context)))))

(defn- reduce-nodes
  [rf acc nodes context]
  (reduce
    (fn [acc node]
      (case (node :type)
        :text
        (rf acc (node :value ""))

        :value-node
        (let [render-fn (node :render-fn)
              ^String resolved (if render-fn
                                 (render-fn context)
                                 (->str (parser/resolve-path context (node :value))))]
          (rf acc resolved))

        :variable-assignment
        (reduce-nodes rf acc (node :body)
                      (assoc context (node :variable-name)
                                     (parser/resolve-path context (node :variable-value))))

        :variable-declaration
        (reduce-nodes rf acc (node :body)
                      (assoc context (node :variable-name) (node :variable-value)))

        :for
        (let [body (node :body)
              identifier (node :identifier)
              items (parser/resolve-path context (node :source))]
          (if (seq items)
            (let [item-count (count items)]
              (loop [i 0
                     remaining (seq items)
                     acc acc]
                (if remaining
                  (let [loop-context (get-loop-context context i item-count)
                        new-context (assoc loop-context identifier (first remaining))]
                    (recur (inc i) (next remaining)
                           (reduce-nodes rf acc body new-context)))
                  acc)))
            (if-let [when-empty (node :when-empty)]
              (reduce-nodes rf acc when-empty context)
              acc)))

        :each
        (let [body (node :body)
              identifier (node :identifier)
              items (parser/resolve-path context (node :source))]
          (if (seq items)
            (loop [remaining (seq items)
                   acc acc]
              (if remaining
                (recur (next remaining)
                       (reduce-nodes rf acc body (assoc context identifier (first remaining))))
                acc))
            (if-let [when-empty (node :when-empty)]
              (reduce-nodes rf acc when-empty context)
              acc)))

        :if
        (let [matched (reduce
                        (fn [_ [condition body]]
                          (when (evaluate-condition condition context)
                            (reduced {:body body})))
                        nil
                        (node :branches))]
          (cond
            matched            (reduce-nodes rf acc (:body matched) context)
            (seq (node :else)) (reduce-nodes rf acc (node :else) context)
            :else              acc))

        :translation
        (let [result ((node :trans-fn) (get context :locale))]
          (if result
            (rf acc (->str result))
            acc))

        :debug
        (do (debug-output node context) acc)

        acc))
    acc
    nodes))

(defn- template-reducible
  [nodes context]
  (reify IReduceInit
    (reduce [_ rf init]
      (reduce-nodes rf init nodes context))))

(defn- partial-render-nodes [nodes context]
  (reduce
    (fn [acc node]
      (case (node :type)
        :text
        (conj acc node)

        :value-node
        (let [render-fn (node :render-fn)
              raw (if render-fn
                    (render-fn context ::raw)
                    (parser/resolve-path context (node :value)))]
          (if (some? raw)
            (conj acc {:type :text :value (if render-fn
                                            (render-fn context)
                                            (->str raw))})
            (conj acc node)))

        :variable-assignment
        (let [variable-name (node :variable-name)
              variable-value (node :variable-value)
              body (node :body)
              resolved-val (parser/resolve-path context variable-value)]
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
              source-path (node :source)
              body (node :body)
              items (parser/resolve-path context source-path)]
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
              source-path (node :source)
              body (node :body)
              items (parser/resolve-path context source-path)]
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
                                   (let [condition-val (parser/resolve-path context (:condition condition))]
                                     (if (some? condition-val)
                                       (let [eval-fn (or (:evaluation-function condition) boolean)
                                             result (boolean (eval-fn condition-val))
                                             matches? (if (:negate condition) (not result) result)]
                                         (if matches?
                                           (reduced {:resolved true :body body})
                                           acc))
                                       (reduced {:resolved false}))))
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
      (transduce identity
                 (fn
                   ([sb] (sb/build sb))
                   ([sb ^String s] (sb/append sb s)))
                 (sb/create-string-builder)
                 (template-reducible template context))
      (error/handle-error error-handler this template))))

(defrecord InputStreamRenderer []
  render-target/RenderTarget
  (render [this template context error-handler]
    (if-not (map? template)
      (let [^Charset charset StandardCharsets/UTF_8]
        (transduce identity
                   (fn
                     ([^ArrayList al] (SequentialByteArrayInputStream. al))
                     ([^ArrayList al ^String s]
                      (when (pos? (.length s))
                        (.add al (.getBytes s charset)))
                      al))
                   (ArrayList. (count template))
                   (template-reducible template context)))
      (error/handle-error error-handler this template))))

(defrecord PartialRenderer []
  render-target/RenderTarget
  (render [this template context error-handler]
    (if-not (map? template)
      (-> (partial-render-nodes template context)
          optimize-ast)
      (error/handle-error error-handler this template))))