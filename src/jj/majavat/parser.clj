(ns jj.majavat.parser
  (:require [clojure.java.io :as io]
            [jj.majavat.lexer :as lexer])
  (:import (java.nio.file Paths)))

(defn- resolve-relative-path [base-path relative-path]
  (let [base-path-obj    (Paths/get base-path (into-array String []))
        relative-path-obj (Paths/get relative-path (into-array String []))
        parent-path      (or (.getParent base-path-obj)
                             (Paths/get "" (into-array String [])))]
    (-> parent-path
        (.resolve relative-path-obj)
        (.normalize)
        (.toString))))

(defn- parse-ast
  ([lexed-list list current-block]
   (parse-ast lexed-list list current-block false nil))
  ([lexed-list list current-block parsing-for-body]
   (parse-ast lexed-list list current-block parsing-for-body nil))
  ([lexed-list list current-block parsing-for-body current-file-path]
   (if (empty? lexed-list)
     (if parsing-for-body
       [list lexed-list]
       list)
     (let [current-item (first lexed-list)]
       (case (:type current-item)
         :text (recur (rest lexed-list) (conj list current-item) current-block parsing-for-body current-file-path)

         :expression (recur (rest lexed-list) (conj list (assoc current-item :type :value-node)) current-block parsing-for-body current-file-path)

         :opening-bracket (recur (rest lexed-list) list current-block parsing-for-body current-file-path)
         :closing-bracket (recur (rest lexed-list) list current-block parsing-for-body current-file-path)
         :block-start (recur (rest lexed-list) list current-block parsing-for-body current-file-path)
         :block-end (recur (rest lexed-list) list current-block parsing-for-body current-file-path)

         :keyword-include (let [remaining (rest lexed-list)
                                filename-token (first remaining)
                                remaining-after-filename (rest remaining)
                                filename (:value filename-token)
                                resolved-filename (if current-file-path
                                                    (resolve-relative-path current-file-path filename)
                                                    filename)
                                include-resource (io/resource resolved-filename)]
                            (if (some? include-resource)
                              (let [included-lexed (lexer/tokenize (slurp include-resource))
                                    included-content (parse-ast included-lexed [] {} false resolved-filename)]
                                (recur remaining-after-filename (into list included-content) current-block parsing-for-body current-file-path))
                              (throw (Exception. (format "%s does not exist" filename)))))

         :keyword-extends (let [remaining (rest lexed-list)
                                block-name-token (first remaining)
                                remaining-after-block-name (rest remaining)
                                file-path-token (first remaining-after-block-name)
                                remaining-after-file-path (rest remaining-after-block-name)
                                block-name (:value block-name-token)
                                file-path (:value file-path-token)
                                resolved-file-path (if current-file-path
                                                     (resolve-relative-path current-file-path file-path)
                                                     file-path)
                                parent-resource (io/resource resolved-file-path)]
                            (if (some? parent-resource)
                              (let [parent-lexed (lexer/tokenize (slurp parent-resource))
                                    [block-content remaining-after-block] (parse-ast remaining-after-file-path [] current-block true current-file-path)
                                    parent-content (parse-ast parent-lexed [] {block-name block-content} false resolved-file-path)]
                                (recur remaining-after-block (into parent-content list) current-block parsing-for-body current-file-path))
                              (throw (Exception. (format "%s does not exist" file-path)))))

         :keyword-block (let [remaining (rest lexed-list)
                              block-name-token (first remaining)
                              block-name (:value block-name-token)
                              remaining-after-block-name (rest remaining)
                              replacement-content (get current-block block-name)]
                          (if replacement-content
                            (recur remaining-after-block-name (into list replacement-content) current-block parsing-for-body current-file-path)
                            (let [[block-content remaining-after-block] (parse-ast remaining-after-block-name [] current-block true current-file-path)]
                              (recur remaining-after-block (into list block-content) current-block parsing-for-body current-file-path))))

         :keyword-for (let [remaining (rest lexed-list)
                            identifier-token (first remaining)
                            remaining-after-id (rest remaining)
                            remaining-after-in (rest remaining-after-id)
                            source-token (first remaining-after-in)
                            remaining-after-source (rest remaining-after-in)
                            remaining-after-block-end (rest remaining-after-source)
                            [body remaining-after-body] (parse-ast remaining-after-block-end [] current-block true current-file-path)
                            for-node {:type       :for
                                      :identifier (:value identifier-token)
                                      :source     (:value source-token)
                                      :body       body}]
                        (recur remaining-after-body (conj list for-node) current-block parsing-for-body current-file-path))

         :keyword-if (let [remaining (rest lexed-list)
                           condition-token (first remaining)
                           remaining-after-condition (rest remaining)
                           remaining-after-block-end (rest remaining-after-condition)
                           [when-true remaining-after-true] (parse-ast remaining-after-block-end [] current-block true current-file-path)
                           [when-false remaining-final] (if (and (seq remaining-after-true)
                                                                 (= :keyword-else (:type (first remaining-after-true))))
                                                          (let [remaining-after-else (rest (rest remaining-after-true))
                                                                [else-body remaining-after-else-body] (parse-ast remaining-after-else [] current-block true current-file-path)]
                                                            [else-body remaining-after-else-body])
                                                          [{:type :text :value ""} remaining-after-true])
                           if-node {:type       :if
                                    :condition  (:value condition-token)
                                    :when-true  (if (= 1 (count when-true))
                                                  (first when-true)
                                                  when-true)
                                    :when-false (if (= 1 (count when-false))
                                                  (first when-false)
                                                  when-false)}]
                       (recur remaining-final (conj list if-node) current-block parsing-for-body current-file-path))

         :end-for (if parsing-for-body
                    [list (rest lexed-list)]
                    (recur (rest lexed-list) list current-block parsing-for-body current-file-path))

         :keyword-else (if parsing-for-body
                         [list lexed-list]
                         (recur (rest lexed-list) list current-block parsing-for-body current-file-path))

         :keyword-endif (if parsing-for-body
                          [list (rest lexed-list)]
                          (recur (rest lexed-list) list current-block parsing-for-body current-file-path))

         :keyword-in (recur (rest lexed-list) list current-block parsing-for-body current-file-path)
         :identifier (recur (rest lexed-list) list current-block parsing-for-body current-file-path)
         :extends-block-name (recur (rest lexed-list) list current-block parsing-for-body current-file-path)
         :file-path (recur (rest lexed-list) list current-block parsing-for-body current-file-path)
         :block-name (recur (rest lexed-list) list current-block parsing-for-body current-file-path)

         (recur (rest lexed-list) list current-block parsing-for-body current-file-path))))))

(defn parse [resource-path]
  (let [resource (io/resource resource-path)]
    (if (some? resource)
      (let [lexed-value (lexer/tokenize (slurp resource))]
        (try
          (parse-ast lexed-value [] {} false resource-path)
          (catch Exception e
            {:type    :error
             :message (.getMessage ^Exception e)})))
      {:type    :error
       :message (format "%s resource can not be found." resource-path)})))