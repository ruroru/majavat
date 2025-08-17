(ns jj.majavat.renderer
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [jj.majavat.renderer.escape :as rops])
  (:import (java.io ByteArrayInputStream SequenceInputStream)
           (java.nio.charset Charset StandardCharsets)
           (java.util Collections)))

(defn- resolve-path [context path]
  (if (vector? path)
    (get-in context path)
    (get context path)))

(defn- evaluate-condition [condition context]
  (boolean (resolve-path context condition)))

(defn- render-nodes [nodes context ^StringBuilder sb escape-conf]
  (doseq [node nodes]
    (case (:type node)
      :text
      (.append sb (:value node ""))

      :value-node
      (let [val (resolve-path context (:value node))]
        (if (some? (:character-escaper escape-conf))
          (.append sb (rops/escape (:character-escaper escape-conf) (str val)))
          (.append sb (str val))))

      :for
      (let [identifier (:identifier node)
            source-path (:source node)
            body (:body node)
            items (resolve-path context source-path)]
        (doseq [item items]
          (let [new-context (assoc context identifier item)]
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
    (render (edn/read-string (slurp (io/resource "error-template.edn"))) template escape-conf)))


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
          (let [resolved-value (let [val (resolve-path context (:value node))]
                                 (if (some? (:character-escaper escape-conf))
                                   (rops/escape (:character-escaper escape-conf) (str val))
                                   (str (resolve-path context (:value node)))))
                bytes (.getBytes ^String resolved-value ^Charset charset)]
            (cons (ByteArrayInputStream. bytes)
                  (render-nodes-to-stream-seq (rest nodes) context charset escape-conf)))

          :for
          (let [identifier (:identifier node)
                source-path (:source node)
                body (:body node)
                items (resolve-path context source-path)
                for-streams (mapcat #(render-nodes-to-stream-seq
                                       body
                                       (assoc context identifier %)
                                       charset escape-conf)
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
     (ByteArrayInputStream. (.getBytes ^String (render (edn/read-string (slurp (io/resource "error-template.edn"))) template escape-conf))))))


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

