(ns jj.majavat.renderer
  (:import (java.io ByteArrayInputStream SequenceInputStream)
           (java.nio.charset Charset StandardCharsets)
           (java.util Collections)))

(defn- resolve-path [context path]
  (if (vector? path)
    (get-in context path)
    (get context path)))

(defn- evaluate-condition [condition context]
  (boolean (resolve-path context condition)))

(defn- render-nodes [nodes context ^StringBuilder sb]
  (doseq [node nodes]
    (case (:type node)
      :text
      (.append sb (:value node ""))

      :value-node
      (let [resolved-value (resolve-path context (:value node))]
        (.append sb (str resolved-value)))

      :for
      (let [identifier (:identifier node)
            source-path (:source node)
            body (:body node)
            items (resolve-path context source-path)]
        (doseq [item items]
          (let [new-context (assoc context identifier item)]
            (render-nodes body new-context sb))))

      :if
      (let [condition (:condition node)
            when-true (:when-true node)
            when-false (:when-false node)]
        (if (evaluate-condition condition context)
          (when when-true (render-nodes when-true context sb))
          (when when-false (render-nodes when-false context sb))))
      nil))
  sb)

(defn render [template context]
  (if-not (map? template)
    (.toString ^StringBuilder (render-nodes template context (StringBuilder.)))
    (:message template)))


(defn- render-nodes-to-stream-seq [nodes context charset]
  (lazy-seq
    (when (seq nodes)
      (let [node (first nodes)]
        (case (:type node)
          :text
          (let [text (:value node "")]
            (if (empty? text)
              (render-nodes-to-stream-seq (rest nodes) context charset)
              (cons (ByteArrayInputStream. (.getBytes ^String text ^Charset charset))
                    (render-nodes-to-stream-seq (rest nodes) context charset))))

          :value-node
          (let [resolved-value (str (resolve-path context (:value node)))
                bytes (.getBytes ^String resolved-value ^Charset charset)]
            (cons (ByteArrayInputStream. bytes)
                  (render-nodes-to-stream-seq (rest nodes) context charset)))

          :for
          (let [identifier (:identifier node)
                source-path (:source node)
                body (:body node)
                items (resolve-path context source-path)
                for-streams (mapcat #(render-nodes-to-stream-seq
                                       body
                                       (assoc context identifier %)
                                       charset)
                                    items)]
            (concat for-streams
                    (render-nodes-to-stream-seq (rest nodes) context charset)))

          :if
          (let [condition (:condition node)
                when-true (:when-true node)
                when-false (:when-false node)]
            (if (evaluate-condition condition context)
              (if when-true
                (concat (render-nodes-to-stream-seq when-true context charset)
                        (render-nodes-to-stream-seq (rest nodes) context charset))
                (render-nodes-to-stream-seq (rest nodes) context charset))
              (if when-false
                (concat (render-nodes-to-stream-seq when-false context charset)
                        (render-nodes-to-stream-seq (rest nodes) context charset))
                (render-nodes-to-stream-seq (rest nodes) context charset))))
          (render-nodes-to-stream-seq (rest nodes) context charset))))))

(defn render-to-input-stream-streaming
  ([template context]
   (render-to-input-stream-streaming template context StandardCharsets/UTF_8))
  ([template context charset]
   (if-not (map? template)
     (let [stream-seq (render-nodes-to-stream-seq template context charset)
           enumeration (Collections/enumeration stream-seq)]
       (SequenceInputStream. enumeration))
     (ByteArrayInputStream. (.getBytes ^String (:message template))))))


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

