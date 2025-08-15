(ns jj.majavat.renderer
  (:import (java.io ByteArrayInputStream SequenceInputStream)
           (java.nio.charset Charset StandardCharsets)
           (java.util Collections)))

(defn- resolve-path [context path]
  (if (vector? path)
    (get-in context path)
    (get context path)))

(defn- html-escape
  "Escapes HTML characters in a string for best performance.
   Uses StringBuilder with pre-allocated capacity and primitive operations."
  [^String s]
  (if (nil? s)
    s
    (let [len (.length s)
          ;; Pre-allocate StringBuilder with extra capacity for escaped chars
          sb (StringBuilder. (int (* len 1.2)))]
      (loop [i 0]
        (if (< i len)
          (let [c (.charAt s i)]
            (case c
              \& (.append sb "&amp;")
              \< (.append sb "&lt;")
              \> (.append sb "&gt;")
              \" (.append sb "&quot;")
              (.append sb c))
            (recur (unchecked-inc i)))
          (.toString sb))))))


(defn- evaluate-condition [condition context]
  (boolean (resolve-path context condition)))

(defn- render-nodes [nodes context ^StringBuilder sb escape?]
  (doseq [node nodes]
    (case (:type node)
      :text
      (.append sb (:value node ""))

      :value-node
      (let [resolved-value (resolve-path context (:value node))]
        (if escape?
          (.append sb (html-escape (str resolved-value)))
          (.append sb (str resolved-value)))
        )

      :for
      (let [identifier (:identifier node)
            source-path (:source node)
            body (:body node)
            items (resolve-path context source-path)]
        (doseq [item items]
          (let [new-context (assoc context identifier item)]
            (render-nodes body new-context sb escape?))))

      :if
      (let [condition (:condition node)
            when-true (:when-true node)
            when-false (:when-false node)]
        (if (evaluate-condition condition context)
          (when (seq when-true) (render-nodes when-true context sb escape?))
          (when (seq when-false) (render-nodes when-false context sb escape?))))

      nil))
  sb)

(defn render [template context escape?]
  (if-not (map? template)
    (.toString ^StringBuilder (render-nodes template context (StringBuilder.) escape?))
    (:message template)))


(defn- render-nodes-to-stream-seq [nodes context charset escape?]
  (lazy-seq
    (when (seq nodes)
      (let [node (first nodes)]
        (case (:type node)
          :text
          (let [text (:value node "")]
            (if (empty? text)
              (render-nodes-to-stream-seq (rest nodes) context charset escape?)
              (cons (ByteArrayInputStream. (.getBytes ^String text ^Charset charset )  )
                    (render-nodes-to-stream-seq (rest nodes) context charset escape?))))

          :value-node
          (let [resolved-value (str (resolve-path context (:value node)))
                bytes (.getBytes ^String resolved-value ^Charset charset)]
            (cons (ByteArrayInputStream. bytes)
                  (render-nodes-to-stream-seq (rest nodes) context charset escape?)))

          :for
          (let [identifier (:identifier node)
                source-path (:source node)
                body (:body node)
                items (resolve-path context source-path)
                for-streams (mapcat #(render-nodes-to-stream-seq
                                       body
                                       (assoc context identifier %)
                                       charset escape?)
                                    items)]
            (concat for-streams
                    (render-nodes-to-stream-seq (rest nodes) context charset escape?) ) )

          :if
          (let [condition (:condition node)
                when-true (:when-true node)
                when-false (:when-false node)]
            (if (evaluate-condition condition context)
              (if when-true
                (concat (render-nodes-to-stream-seq when-true context charset escape?)
                        (render-nodes-to-stream-seq (rest nodes) context charset escape?))
                (render-nodes-to-stream-seq (rest nodes) context charset escape?))
              (if when-false
                (concat (render-nodes-to-stream-seq when-false context charset escape?)
                        (render-nodes-to-stream-seq (rest nodes) context charset escape?))
                (render-nodes-to-stream-seq (rest nodes) context charset escape?))))
          (render-nodes-to-stream-seq (rest nodes) context charset escape?) )))))

(defn render-is
  ([template context escape?]
   (render-is template context StandardCharsets/UTF_8 escape?))
  ([template context charset escape?]
   (if-not (map? template)
     (let [stream-seq (render-nodes-to-stream-seq template context charset escape?)
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

