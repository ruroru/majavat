(ns jj.majavat.parser
  (:require [jj.majavat.lexer :as lexer]
            [clojure.pprint]
            [jj.majavat.resolver :as cr])
  (:import (java.io FileNotFoundException)))



(defn- parse-ast
  ([lexed-list list current-block template-resolver]
   (parse-ast lexed-list list current-block false nil template-resolver))
  ([lexed-list list current-block parsing-for-body current-file-path template-resolver]
   (if (empty? lexed-list)
     (if parsing-for-body
       [list lexed-list]
       list)
     (let [current-item (first lexed-list)]
       (case (:type current-item)
         :text (recur (rest lexed-list) (conj list current-item) current-block parsing-for-body current-file-path template-resolver)

         :expression (let [remaining (rest lexed-list)
                           [filters remaining-after-filters] (loop [remaining remaining
                                                                    filters []]
                                                               (if (and (seq remaining)
                                                                        (= :filter-tag (:type (first remaining))))
                                                                 (let [remaining-after-tag (rest remaining)]
                                                                   (if (and (seq remaining-after-tag)
                                                                            (= :filter-function (:type (first remaining-after-tag))))
                                                                     (let [filter-function (first remaining-after-tag)
                                                                           remaining-after-function (rest remaining-after-tag)]
                                                                       (recur remaining-after-function
                                                                              (conj filters (:value filter-function))))
                                                                     [filters remaining]))
                                                                 [filters remaining]))
                           value-node (if (empty? filters)
                                        (assoc current-item :type :value-node)
                                        (assoc current-item :type :value-node :filters filters))]
                       (recur remaining-after-filters (conj list value-node) current-block parsing-for-body current-file-path template-resolver))

         :opening-bracket (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver)
         :closing-bracket (if (not (nil? (:value (last list))))
                            (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver)
                            (throw (Exception. (str (:line current-item))))
                            )
         :block-start (let [remaining (rest lexed-list)]
                        (if (and (seq remaining) (= :block-end (:type (first remaining))))
                          (let [remaining-after-block-end (rest remaining)]
                            (if parsing-for-body
                              (let [value-node {:type :value-node :value [:foo]}]
                                (recur remaining-after-block-end (conj list value-node) current-block parsing-for-body current-file-path template-resolver))
                              (recur remaining-after-block-end list current-block parsing-for-body current-file-path template-resolver)))
                          (recur remaining list current-block parsing-for-body current-file-path template-resolver)))
         :block-end (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver)

         :keyword-include (let [remaining (rest lexed-list)
                                file-name-token (first remaining)
                                remaining-after-filename (rest remaining)]
                            (if (not (= :block-end (:type file-name-token)))
                              (let [
                                    filename (:value file-name-token)
                                    resolved-filename (if current-file-path
                                                        (cr/resolve-path template-resolver current-file-path filename)
                                                        filename)]
                                (if (cr/content-exists? template-resolver resolved-filename)
                                  (let [file-content (cr/read-content template-resolver resolved-filename)
                                        included-lexed (lexer/tokenize file-content)
                                        included-content (parse-ast included-lexed [] {} false resolved-filename template-resolver)]
                                    (recur remaining-after-filename (into list included-content) current-block parsing-for-body current-file-path template-resolver))
                                  (throw (FileNotFoundException. filename))))
                              (throw (Exception. (str (:line file-name-token))))))

         :keyword-extends (let [remaining (rest lexed-list)
                                block-name-token (first remaining)
                                remaining-after-block-name (rest remaining)]
                            (let [block-name (:value block-name-token)]
                              (if (not (= :block-end (:type block-name-token)))
                                (let [file-path-token (first remaining-after-block-name)
                                      remaining-after-file-path (rest remaining-after-block-name)]
                                  (if (not (= :block-end (:type file-path-token)))
                                    (let [file-path (:value file-path-token)
                                          resolved-file-path (if current-file-path
                                                               (cr/resolve-path template-resolver current-file-path file-path)
                                                               file-path)]
                                      (if (cr/content-exists? template-resolver resolved-file-path)
                                        (let [parent-content-str (cr/read-content template-resolver resolved-file-path)
                                              parent-lexed (lexer/tokenize parent-content-str)
                                              [block-content remaining-after-block] (parse-ast remaining-after-file-path [] current-block true current-file-path template-resolver)
                                              parent-content (parse-ast parent-lexed [] {block-name block-content} false resolved-file-path template-resolver)]

                                          (recur remaining-after-block (into parent-content list) current-block parsing-for-body current-file-path template-resolver))
                                        (throw (FileNotFoundException. file-path))))
                                    (throw (Exception. (str (:line file-path-token))))))


                                (throw (Exception. (str (:line block-name-token)))))))

         :keyword-block (let [remaining (rest lexed-list)
                              block-name-token (first remaining)
                              block-name (:value block-name-token)
                              remaining-after-block-name (rest remaining)
                              replacement-content (get current-block block-name)]
                          (if replacement-content
                            (recur remaining-after-block-name (into list replacement-content) current-block parsing-for-body current-file-path template-resolver)
                            (let [[block-content remaining-after-block] (parse-ast remaining-after-block-name [] current-block true current-file-path template-resolver)]
                              (recur remaining-after-block (into list block-content) current-block parsing-for-body current-file-path template-resolver))))

         :keyword-let (let [remaining (rest lexed-list)
                            var-decl-token (first remaining)]
                        (if (and var-decl-token (= :variable-declaration (:type var-decl-token)))
                          (let [remaining-after-var-decl (rest remaining)
                                block-end-token (first remaining-after-var-decl)]
                            (if (and block-end-token (= :block-end (:type block-end-token)))
                              (let [remaining-after-block-end (rest remaining-after-var-decl)
                                    [body remaining-after-body] (parse-ast remaining-after-block-end [] current-block true current-file-path template-resolver)
                                    let-node {:type           :variable-declaration
                                              :variable-name  (:variable-name var-decl-token)
                                              :variable-value (:variable-value var-decl-token)
                                              :body          body}]
                                (recur remaining-after-body (conj list let-node) current-block parsing-for-body current-file-path template-resolver))
                              (throw (Exception. (str (:line (or block-end-token var-decl-token)))))))
                          (throw (Exception. (str (:line (or var-decl-token current-item)))))))

         :keyword-for (let [remaining (rest lexed-list)
                            identifier-token (first remaining)]
                        (if (not (= :block-end (:type identifier-token)))
                          (let [remaining-after-id (rest remaining)]

                            (if (= :keyword-in (:type (first remaining-after-id)))
                              (let [remaining-after-in (rest remaining-after-id)
                                    source-token (first remaining-after-in)
                                    remaining-after-source (rest remaining-after-in)
                                    block-end-token (first remaining-after-source)]
                                (if (some? (:value source-token))
                                  (let [remaining-after-block-end (rest remaining-after-source)
                                        [body remaining-after-body] (parse-ast remaining-after-block-end [] current-block true current-file-path template-resolver)
                                        for-node {:type       :for
                                                  :identifier (:value identifier-token)
                                                  :source     (:value source-token)
                                                  :body       body}]

                                    (recur remaining-after-body (conj list for-node) current-block parsing-for-body current-file-path template-resolver))
                                  (throw (Exception. (str (:line block-end-token))))))
                              (throw (Exception. (str (:line (first remaining-after-id)))))))
                          (throw (Exception. (str (:line identifier-token))))))

         :keyword-if (let [remaining (rest lexed-list)
                           condition-token (first remaining)
                           remaining-after-condition (rest remaining)
                           remaining-after-block-end (rest remaining-after-condition)
                           [when-true remaining-after-true] (parse-ast remaining-after-block-end [] current-block true current-file-path template-resolver)
                           [when-false remaining-final] (if (and (seq remaining-after-true)
                                                                 (= :keyword-else (:type (first remaining-after-true))))
                                                          (let [remaining-after-else (rest (rest remaining-after-true))
                                                                [else-body remaining-after-else-body] (parse-ast remaining-after-else [] current-block true current-file-path template-resolver)]
                                                            [else-body remaining-after-else-body])
                                                          [[{:type :text :value ""}] remaining-after-true])
                           if-node {:type       :if
                                    :condition  (:value condition-token)
                                    :when-true  when-true
                                    :when-false when-false}]
                       (if (some? (:value condition-token))
                         (recur remaining-final (conj list if-node) current-block parsing-for-body current-file-path template-resolver)
                         (throw (Exception. (str (:line condition-token))))))

         :keyword-if-not (let [remaining (rest lexed-list)
                               condition-token (first remaining)
                               remaining-after-condition (rest remaining)
                               remaining-after-block-end (rest remaining-after-condition)
                               [when-true remaining-after-true] (parse-ast remaining-after-block-end [] current-block true current-file-path template-resolver)
                               [when-false remaining-final] (if (and (seq remaining-after-true)
                                                                     (= :keyword-else (:type (first remaining-after-true))))
                                                              (let [remaining-after-else (rest (rest remaining-after-true))
                                                                    [else-body remaining-after-else-body] (parse-ast remaining-after-else [] current-block true current-file-path template-resolver)]
                                                                [else-body remaining-after-else-body])
                                                              [[{:type :text :value ""}] remaining-after-true])
                               if-not-node {:type       :if-not
                                            :condition  (:value condition-token)
                                            :when-true  when-true
                                            :when-false when-false}]
                           (if (some? (:value condition-token))
                             (recur remaining-final (conj list if-not-node) current-block parsing-for-body current-file-path template-resolver)
                             (throw (Exception. (str (:line condition-token))))))

         :end-for (if parsing-for-body
                    [list (rest lexed-list)]
                    (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver))

         :keyword-else (if parsing-for-body
                         [list lexed-list]
                         (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver))

         :keyword-endif (if parsing-for-body
                          [list (rest lexed-list)]
                          (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver))

         :keyword-end-let (if parsing-for-body
                            [list (rest lexed-list)]
                            (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver))

         :keyword-in (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver)
         :variable-declaration (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver)
         :identifier (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver)
         :extends-block-name (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver)
         :file-path (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver)
         :block-name (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver)

         (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver))))))

(defn parse
  [resource-path template-resolver]
  (if (cr/content-exists? template-resolver resource-path)
    (let [file-content (cr/read-content template-resolver resource-path)
          lexed-value (lexer/tokenize file-content)]
      (try
        (parse-ast lexed-value [] {} false resource-path template-resolver)
        (catch FileNotFoundException e
          {:type          "template-not-found-error"
           :error-message (format "%s template can not be found" (.getMessage ^Exception e))
           })
        (catch Exception e
          {:type          "syntax-error"
           :error-message (format "error on line %s" (.getMessage ^Exception e))
           :line          (.getMessage ^Exception e)})))
    {:type          "template-not-found-error"
     :error-message (format "%s can not be found." resource-path)}))