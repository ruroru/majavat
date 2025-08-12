(ns jj.majavat.parser
  (:require [jj.majavat.lexer :as lexer]
            [jj.majavat.content-resolver :as cr]))

(defn- parse-ast
  ([lexed-list list current-block content-resolver]
   (parse-ast lexed-list list current-block false nil content-resolver))
  ([lexed-list list current-block parsing-for-body current-file-path content-resolver]
   (if (empty? lexed-list)
     (if parsing-for-body
       [list lexed-list]
       list)
     (let [current-item (first lexed-list)]
       (case (:type current-item)
         :text (recur (rest lexed-list) (conj list current-item) current-block parsing-for-body current-file-path content-resolver)

         :expression (recur (rest lexed-list) (conj list (assoc current-item :type :value-node)) current-block parsing-for-body current-file-path content-resolver)

         :opening-bracket (recur (rest lexed-list) list current-block parsing-for-body current-file-path content-resolver)
         :closing-bracket (recur (rest lexed-list) list current-block parsing-for-body current-file-path content-resolver)
         :block-start (recur (rest lexed-list) list current-block parsing-for-body current-file-path content-resolver)
         :block-end (recur (rest lexed-list) list current-block parsing-for-body current-file-path content-resolver)

         :keyword-include (let [remaining (rest lexed-list)
                                filename-token (first remaining)
                                remaining-after-filename (rest remaining)
                                filename (:value filename-token)
                                resolved-filename (if current-file-path
                                                    (cr/resolve-path content-resolver current-file-path filename)
                                                    filename)]
                            (if (cr/content-exists? content-resolver resolved-filename)
                              (let [file-content (cr/read-content content-resolver resolved-filename)
                                    included-lexed (lexer/tokenize file-content)
                                    included-content (parse-ast included-lexed [] {} false resolved-filename content-resolver)]
                                (recur remaining-after-filename (into list included-content) current-block parsing-for-body current-file-path content-resolver))
                              (throw (Exception. (format "%s does not exist" filename)))))

         :keyword-extends (let [remaining (rest lexed-list)
                                block-name-token (first remaining)
                                remaining-after-block-name (rest remaining)
                                file-path-token (first remaining-after-block-name)
                                remaining-after-file-path (rest remaining-after-block-name)
                                block-name (:value block-name-token)
                                file-path (:value file-path-token)
                                resolved-file-path (if current-file-path
                                                     (cr/resolve-path content-resolver current-file-path file-path)
                                                     file-path)]
                            (if (cr/content-exists? content-resolver resolved-file-path)
                              (let [parent-content-str (cr/read-content content-resolver resolved-file-path)
                                    parent-lexed (lexer/tokenize parent-content-str)
                                    [block-content remaining-after-block] (parse-ast remaining-after-file-path [] current-block true current-file-path content-resolver)
                                    parent-content (parse-ast parent-lexed [] {block-name block-content} false resolved-file-path content-resolver)]
                                (recur remaining-after-block (into parent-content list) current-block parsing-for-body current-file-path content-resolver))
                              (throw (Exception. (format "%s does not exist" file-path)))))

         :keyword-block (let [remaining (rest lexed-list)
                              block-name-token (first remaining)
                              block-name (:value block-name-token)
                              remaining-after-block-name (rest remaining)
                              replacement-content (get current-block block-name)]
                          (if replacement-content
                            (recur remaining-after-block-name (into list replacement-content) current-block parsing-for-body current-file-path content-resolver)
                            (let [[block-content remaining-after-block] (parse-ast remaining-after-block-name [] current-block true current-file-path content-resolver)]
                              (recur remaining-after-block (into list block-content) current-block parsing-for-body current-file-path content-resolver))))

         :keyword-for (let [remaining (rest lexed-list)
                            identifier-token (first remaining)
                            remaining-after-id (rest remaining)
                            remaining-after-in (rest remaining-after-id)
                            source-token (first remaining-after-in)
                            remaining-after-source (rest remaining-after-in)
                            remaining-after-block-end (rest remaining-after-source)
                            [body remaining-after-body] (parse-ast remaining-after-block-end [] current-block true current-file-path content-resolver)
                            for-node {:type       :for
                                      :identifier (:value identifier-token)
                                      :source     (:value source-token)
                                      :body       body}]
                        (recur remaining-after-body (conj list for-node) current-block parsing-for-body current-file-path content-resolver))

         :keyword-if (let [remaining (rest lexed-list)
                           condition-token (first remaining)
                           remaining-after-condition (rest remaining)
                           remaining-after-block-end (rest remaining-after-condition)
                           [when-true remaining-after-true] (parse-ast remaining-after-block-end [] current-block true current-file-path content-resolver)
                           [when-false remaining-final] (if (and (seq remaining-after-true)
                                                                 (= :keyword-else (:type (first remaining-after-true))))
                                                          (let [remaining-after-else (rest (rest remaining-after-true))
                                                                [else-body remaining-after-else-body] (parse-ast remaining-after-else [] current-block true current-file-path content-resolver)]
                                                            [else-body remaining-after-else-body])
                                                          [[{:type :text :value ""}] remaining-after-true])
                           if-node {:type       :if
                                    :condition  (:value condition-token)
                                    :when-true  when-true
                                    :when-false when-false}]
                       (recur remaining-final (conj list if-node) current-block parsing-for-body current-file-path content-resolver))

         :end-for (if parsing-for-body
                    [list (rest lexed-list)]
                    (recur (rest lexed-list) list current-block parsing-for-body current-file-path content-resolver))

         :keyword-else (if parsing-for-body
                         [list lexed-list]
                         (recur (rest lexed-list) list current-block parsing-for-body current-file-path content-resolver))

         :keyword-endif (if parsing-for-body
                          [list (rest lexed-list)]
                          (recur (rest lexed-list) list current-block parsing-for-body current-file-path content-resolver))

         :keyword-in (recur (rest lexed-list) list current-block parsing-for-body current-file-path content-resolver)
         :identifier (recur (rest lexed-list) list current-block parsing-for-body current-file-path content-resolver)
         :extends-block-name (recur (rest lexed-list) list current-block parsing-for-body current-file-path content-resolver)
         :file-path (recur (rest lexed-list) list current-block parsing-for-body current-file-path content-resolver)
         :block-name (recur (rest lexed-list) list current-block parsing-for-body current-file-path content-resolver)

         (recur (rest lexed-list) list current-block parsing-for-body current-file-path content-resolver))))))

(defn parse
  [resource-path content-resolver]
  (if (cr/content-exists? content-resolver resource-path)
    (let [file-content (cr/read-content content-resolver resource-path)
          lexed-value (lexer/tokenize file-content)]
      (try
        (parse-ast lexed-value [] {} false resource-path content-resolver)
        (catch Exception e
          {:type    :error
           :message (.getMessage ^Exception e)})))
    {:type    :error
     :message (format "%s resource can not be found." resource-path)}))