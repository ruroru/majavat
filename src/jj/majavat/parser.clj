(ns jj.majavat.parser
  (:require [jj.majavat.lexer :as lexer]
            [jj.majavat.renderer.filters :as filters]
            [jj.majavat.resolver :as cr])
  (:import (clojure.lang ExceptionInfo)
           (java.io PushbackReader StringWriter)
           (java.nio.file Paths)
           (java.time ZoneId)))

(def ^:private default-filter-map
  {:trim             filters/trim-string
   :upper-case       filters/upper-case-string
   :lower-case       filters/lower-case-string
   :capitalize       filters/capitalize-string
   :title-case       filters/title-case
   :slugify          filters/slugify
   :upper-roman      filters/upper-roman
   :append           filters/append
   :prepend          filters/prepend
   :int              filters/as-int
   :long             filters/as-long
   :name             filters/get-name
   :inc              filters/inc-number
   :dec              filters/dec-number
   :round            filters/round-number
   :floor            filters/get-floor
   :ceil             filters/get-ceiling
   :abs              filters/get-absolute-value
   :file-size-format filters/file-size
   :default          filters/get-default
   :date             filters/handle-date
   :where            filters/->handle-where
   :first            filters/get-first
   :rest             filters/get-rest
   :str              filters/handle-str})

(defn- create-filter-fn [{:keys [filter-name args]} user-filter-map]
  (let [filters (if (empty? user-filter-map)
                  default-filter-map
                  (merge
                    default-filter-map
                    user-filter-map))]
    (if-let [f (get filters filter-name)]
      (fn [input] (f input args))
      (throw (ex-info (format "Unsupported filter: %s" (name filter-name))
                      {:type        :syntax-error
                       :filter-name filter-name
                       :line        1})))))


(defn- build-filter-fn [filter-specs user-filters]
  (if (empty? filter-specs)
    identity
    (apply comp (reverse (map #(create-filter-fn % user-filters) filter-specs)))))

(defn- resolve-path [base-path relative-path]
  (let [base-path-obj (Paths/get base-path (make-array String 0))
        relative-path-obj (Paths/get relative-path (make-array String 0))
        parent-path (or (.getParent base-path-obj)
                        (Paths/get "" (make-array String 0)))]

    (-> parent-path
        (.resolve relative-path-obj)
        (.normalize)
        (.toString))))

(defn- read-content-as-string [template-resolver content-path]
  (when-let [reader (PushbackReader. (cr/open-reader template-resolver content-path))]
    (with-open [r reader]
      (let [writer (StringWriter.)]
        (.transferTo r writer)
        (.toString writer)))))


(defn- parse-ast
  ([lexed-list list current-block template-resolver user-filters]
   (parse-ast lexed-list list current-block false nil template-resolver user-filters))
  ([lexed-list list current-block parsing-for-body current-file-path template-resolver custom-filter-map]
   (if (empty? lexed-list)
     (if parsing-for-body
       [list lexed-list]
       list)
     (let [current-item (first lexed-list)]
       (case (:type current-item)
         :text (recur (rest lexed-list) (conj list current-item) current-block parsing-for-body current-file-path template-resolver custom-filter-map)

         :expression (let [remaining (rest lexed-list)
                           [filters remaining-after-filters] (loop [remaining remaining
                                                                    filters []]
                                                               (if (and (seq remaining)
                                                                        (= :filter-tag (:type (first remaining))))
                                                                 (let [remaining-after-tag (rest remaining)]
                                                                   (if (and (seq remaining-after-tag)
                                                                            (= :filter-function (:type (first remaining-after-tag))))
                                                                     (let [filter-function (first remaining-after-tag)
                                                                           remaining-after-function (rest remaining-after-tag)
                                                                           [args remaining-after-args] (loop [remaining remaining-after-function
                                                                                                              args []]
                                                                                                         (if (and (seq remaining) (= :filter-arg (:type (first remaining))))
                                                                                                           (recur (rest remaining) (conj args (:value (first remaining))))
                                                                                                           [args remaining]))
                                                                           parsed-filter {:filter-name (:value filter-function) :args args}]
                                                                       (recur remaining-after-args (conj filters parsed-filter)))
                                                                     (throw (ex-info (format "error on line %s" (:line (first remaining-after-tag)))
                                                                                     {:type :syntax-error
                                                                                      :line (:line (first remaining-after-tag))}))))
                                                                 [filters remaining]))
                           value-node (if (empty? filters)
                                        (assoc current-item :type :value-node)
                                        (assoc current-item :type :value-node :filter-fn (build-filter-fn filters custom-filter-map)))]
                       (recur remaining-after-filters (conj list value-node) current-block parsing-for-body current-file-path template-resolver custom-filter-map))

         :opening-bracket (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver custom-filter-map)
         :closing-bracket (if (not (nil? (:value (last list))))
                            (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver custom-filter-map)
                            (throw (ex-info (format "error on line %s" (:line current-item))
                                            {:type :syntax-error
                                             :line (:line current-item)})))

         :block-start (let [remaining (rest lexed-list)]
                        (if (and (seq remaining) (= :block-end (:type (first remaining))))
                          (let [remaining-after-block-end (rest remaining)]
                            (recur remaining-after-block-end list current-block parsing-for-body current-file-path template-resolver custom-filter-map))
                          (recur remaining list current-block parsing-for-body current-file-path template-resolver custom-filter-map)))
         :block-end (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver custom-filter-map)

         :now (let [remaining (rest lexed-list)
                    [now-node final-remaining] (loop [remaining remaining
                                                      format "yyyy/MM/dd hh:mm"
                                                      timezone (.toString ^ZoneId (ZoneId/systemDefault))]
                                                 (let [current-token (first remaining)]
                                                   (cond
                                                     (and current-token (contains? current-token :now-format))
                                                     (recur (rest remaining) (:now-format current-token) timezone)

                                                     (and current-token (contains? current-token :now-timezone))
                                                     (recur (rest remaining) format (:now-timezone current-token))

                                                     :else
                                                     [{:type      :keyword-now
                                                       :format    format
                                                       :time-zone timezone} remaining])))]
                (recur final-remaining (conj list now-node) current-block parsing-for-body current-file-path template-resolver custom-filter-map))



         :keyword-csrf-token (recur (rest lexed-list) (apply conj list '({:type :text :value "<input type=\"hidden\" name=\"csrf_token\" value=\""}
                                                                         {:type :value-node :value [:csrf-token]}
                                                                         {:type :text :value "\">"}))
                                    current-block parsing-for-body current-file-path template-resolver custom-filter-map)

         :keyword-query-string (let [remaining (rest lexed-list)
                                     query-string-decl-token (first remaining)
                                     remaining-after-decl (rest remaining)]
                                 (if (and query-string-decl-token (= :query-string-declaration (:type query-string-decl-token)))
                                   (let [block-end-token (first remaining-after-decl)]
                                     (if (and block-end-token (= :block-end (:type block-end-token)))
                                       (let [remaining-after-block-end (rest remaining-after-decl)
                                             query-string-node {:type  :query-string
                                                                :value (:variable-value query-string-decl-token)}]
                                         (recur remaining-after-block-end (conj list query-string-node) current-block parsing-for-body current-file-path template-resolver custom-filter-map))
                                       (throw (ex-info (format "error on line %s" (:line (or block-end-token query-string-decl-token)))
                                                       {:type :syntax-error
                                                        :line (:line (or block-end-token query-string-decl-token))}))))
                                   (throw (ex-info (format "error on line %s" (:line (or query-string-decl-token current-item)))
                                                   {:type :syntax-error
                                                    :line (:line (or query-string-decl-token current-item))}))))

         :keyword-include (let [remaining (rest lexed-list)
                                file-name-token (first remaining)
                                remaining-after-filename (rest remaining)]
                            (if (not (= :block-end (:type file-name-token)))
                              (let [filename (:value file-name-token)
                                    resolved-filename (if current-file-path
                                                        (resolve-path current-file-path filename)
                                                        filename)]
                                (if (cr/template-exists? template-resolver resolved-filename)
                                  (let [file-content (read-content-as-string template-resolver resolved-filename)
                                        included-lexed (lexer/tokenize file-content)
                                        included-content (parse-ast included-lexed [] {} false resolved-filename template-resolver custom-filter-map)]
                                    (recur remaining-after-filename (into list included-content) current-block parsing-for-body current-file-path template-resolver custom-filter-map))
                                  (throw (ex-info (format "Template not found: %s" filename)
                                                  {:type     :template-not-found-error
                                                   :template filename}))))
                              (throw (ex-info (format "error on line %s" (:line file-name-token))
                                              {:type :syntax-error
                                               :line (:line file-name-token)}))))

         :keyword-extends (let [remaining (rest lexed-list)
                                file-path-token (first remaining)
                                remaining-after-file-path (rest remaining)]
                            (if (not (= :block-end (:type file-path-token)))
                              (let [file-path (:value file-path-token)
                                    resolved-file-path (if current-file-path
                                                         (resolve-path current-file-path file-path)
                                                         file-path)]
                                (if (cr/template-exists? template-resolver resolved-file-path)
                                  (let [parent-content-str (read-content-as-string template-resolver resolved-file-path)
                                        parent-lexed (lexer/tokenize parent-content-str)
                                        [block-content remaining-after-block] (parse-ast remaining-after-file-path [] current-block true current-file-path template-resolver custom-filter-map)
                                        parent-content (parse-ast parent-lexed [] {:content block-content} false resolved-file-path template-resolver custom-filter-map)]

                                    (recur remaining-after-block (into parent-content list) current-block parsing-for-body current-file-path template-resolver custom-filter-map))
                                  (throw (ex-info (format "Template not found: %s" file-path)
                                                  {:type     :template-not-found-error
                                                   :template file-path}))))
                              (throw (ex-info (format "error on line %s" (:line file-path-token))
                                              {:type :syntax-error
                                               :line (:line file-path-token)}))))

         :keyword-block (let [remaining (rest lexed-list)
                              block-name-token (first remaining)
                              block-name (or (:value block-name-token) (:type block-name-token))
                              remaining-after-block-name (rest remaining)
                              replacement-content (get current-block block-name)]
                          (if replacement-content
                            (recur remaining-after-block-name (into list replacement-content) current-block parsing-for-body current-file-path template-resolver custom-filter-map)
                            (let [[block-content remaining-after-block] (parse-ast remaining-after-block-name [] current-block true current-file-path template-resolver custom-filter-map)]
                              (recur remaining-after-block (into list block-content) current-block parsing-for-body current-file-path template-resolver custom-filter-map))))

         :keyword-let (let [remaining (rest lexed-list)
                            var-decl-token (first remaining)]
                        (if (and var-decl-token (= :variable-declaration (:type var-decl-token)))
                          (let [remaining-after-var-decl (rest remaining)
                                block-end-token (first remaining-after-var-decl)]
                            (if (and block-end-token (= :block-end (:type block-end-token)))
                              (let [remaining-after-block-end (rest remaining-after-var-decl)
                                    [body remaining-after-body] (parse-ast remaining-after-block-end [] current-block true current-file-path template-resolver custom-filter-map)
                                    node-type (if (string? (:variable-value var-decl-token))
                                                :variable-declaration
                                                :variable-assignment)
                                    let-node {:type           node-type
                                              :variable-name  (:variable-name var-decl-token)
                                              :variable-value (:variable-value var-decl-token)
                                              :body           body}]
                                (recur remaining-after-body (conj list let-node) current-block parsing-for-body current-file-path template-resolver custom-filter-map))
                              (throw (ex-info (format "error on line %s" (:line (or block-end-token var-decl-token)))
                                              {:type :syntax-error
                                               :line (:line (or block-end-token var-decl-token))}))))
                          (throw (ex-info (format "error on line %s" (:line (or var-decl-token current-item)))
                                          {:type :syntax-error
                                           :line (:line (or var-decl-token current-item))}))))

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
                                        [body remaining-after-body] (parse-ast remaining-after-block-end [] current-block true current-file-path template-resolver custom-filter-map)
                                        for-node {:type       :for
                                                  :identifier (:value identifier-token)
                                                  :source     (:value source-token)
                                                  :body       body}]

                                    (recur remaining-after-body (conj list for-node) current-block parsing-for-body current-file-path template-resolver custom-filter-map))
                                  (throw (ex-info (format "error on line %s" (:line block-end-token))
                                                  {:type :syntax-error
                                                   :line (:line block-end-token)}))))
                              (throw (ex-info (format "error on line %s" (:line (first remaining-after-id)))
                                              {:type :syntax-error
                                               :line (:line (first remaining-after-id))}))))
                          (throw (ex-info (format "error on line %s" (:line identifier-token))
                                          {:type :syntax-error
                                           :line (:line identifier-token)}))))

         :each-token (let [remaining (rest lexed-list)
                           identifier-token (first remaining)]
                       (if (not (= :block-end (:type identifier-token)))
                         (let [remaining-after-id (rest remaining)]
                           (if (= :each-in-token (:type (first remaining-after-id)))
                             (let [remaining-after-in (rest remaining-after-id)
                                   source-token (first remaining-after-in)
                                   remaining-after-source (rest remaining-after-in)
                                   block-end-token (first remaining-after-source)]
                               (if (some? (:value source-token))
                                 (let [remaining-after-block-end (rest remaining-after-source)
                                       [body remaining-after-body] (parse-ast remaining-after-block-end [] current-block true current-file-path template-resolver custom-filter-map)
                                       each-node {:type       :each
                                                  :identifier (:value identifier-token)
                                                  :source     (:value source-token)
                                                  :body       body}]
                                   (recur remaining-after-body (conj list each-node) current-block parsing-for-body current-file-path template-resolver custom-filter-map))
                                 (throw (ex-info (format "error on line %s" (:line block-end-token))
                                                 {:type :syntax-error
                                                  :line (:line block-end-token)}))))
                             (throw (ex-info (format "error on line %s" (:line (first remaining-after-id)))
                                             {:type :syntax-error
                                              :line (:line (first remaining-after-id))}))))
                         (throw (ex-info (format "error on line %s" (:line identifier-token))
                                         {:type :syntax-error
                                          :line (:line identifier-token)}))))

         :keyword-if (let [remaining (rest lexed-list)
                           condition-token (first remaining)
                           remaining-after-condition (rest remaining)
                           remaining-after-block-end (rest remaining-after-condition)
                           [when-true remaining-after-true] (parse-ast remaining-after-block-end [] current-block true current-file-path template-resolver custom-filter-map)
                           [when-false remaining-final] (if (and (seq remaining-after-true)
                                                                 (= :keyword-else (:type (first remaining-after-true))))
                                                          (let [remaining-after-else (rest (rest remaining-after-true))
                                                                [else-body remaining-after-else-body] (parse-ast remaining-after-else [] current-block true current-file-path template-resolver custom-filter-map)]
                                                            [else-body remaining-after-else-body])
                                                          [[{:type :text :value ""}] remaining-after-true])
                           if-node {:type       :if
                                    :condition  (:value condition-token)
                                    :when-true  when-true
                                    :when-false when-false}]
                       (if (some? (:value condition-token))
                         (recur remaining-final (conj list if-node) current-block parsing-for-body current-file-path template-resolver custom-filter-map)
                         (throw (ex-info (format "error on line %s" (:line condition-token))
                                         {:type :syntax-error
                                          :line (:line condition-token)}))))

         :keyword-if-not (let [remaining (rest lexed-list)
                               condition-token (first remaining)
                               remaining-after-condition (rest remaining)
                               remaining-after-block-end (rest remaining-after-condition)
                               [when-true remaining-after-true] (parse-ast remaining-after-block-end [] current-block true current-file-path template-resolver custom-filter-map)
                               [when-false remaining-final] (if (and (seq remaining-after-true)
                                                                     (= :keyword-else (:type (first remaining-after-true))))
                                                              (let [remaining-after-else (rest (rest remaining-after-true))
                                                                    [else-body remaining-after-else-body] (parse-ast remaining-after-else [] current-block true current-file-path template-resolver custom-filter-map)]
                                                                [else-body remaining-after-else-body])
                                                              [[{:type :text :value ""}] remaining-after-true])
                               if-not-node {:type       :if
                                            :negate     true
                                            :condition  (:value condition-token)
                                            :when-true  when-true
                                            :when-false when-false}]
                           (if (some? (:value condition-token))
                             (recur remaining-final (conj list if-not-node) current-block parsing-for-body current-file-path template-resolver custom-filter-map)
                             (throw (ex-info (format "error on line %s" (:line condition-token))
                                             {:type :syntax-error
                                              :line (:line condition-token)}))))

         :end-for (if parsing-for-body
                    [list (rest lexed-list)]
                    (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver custom-filter-map))

         :end-each-token (if parsing-for-body
                           [list (rest lexed-list)]
                           (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver custom-filter-map))

         :keyword-else (if parsing-for-body
                         [list lexed-list]
                         (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver custom-filter-map))

         :keyword-endif (if parsing-for-body
                          [list (rest lexed-list)]
                          (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver custom-filter-map))

         :keyword-end-let (if parsing-for-body
                            [list (rest lexed-list)]
                            (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver custom-filter-map))

         :keyword-in (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver custom-filter-map)
         :each-in-token (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver custom-filter-map)
         :each-identifier-token (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver custom-filter-map)
         :variable-declaration (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver custom-filter-map)
         :identifier (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver custom-filter-map)
         :file-path (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver custom-filter-map)
         :block-name (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver custom-filter-map)
         :query-string-declaration (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver custom-filter-map)
         :filter-tag (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver custom-filter-map)
         :filter-function (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver custom-filter-map)
         :filter-arg (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver custom-filter-map)

         (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver custom-filter-map))))))

(defn parse
  [resource-path template-resolver user-filters]
  (if (cr/template-exists? template-resolver resource-path)
    (let [file-content (read-content-as-string template-resolver resource-path)
          lexed-value (lexer/tokenize file-content)]
      (try
        (parse-ast lexed-value [] {} false resource-path template-resolver user-filters)
        (catch ExceptionInfo e
          (let [data (ex-data e)]
            (case (:type data)
              :template-not-found-error
              {:type          "template-not-found-error"
               :error-message (format "%s template can not be found" (:template data))}

              :syntax-error
              {:type          "syntax-error"
               :error-message (.getMessage e)
               :line          (str (:line data))}

              {:type          "error"
               :error-message (.getMessage e)})))
        (catch Exception e
          {:type          "error"
           :error-message (.getMessage e)})))
    {:type          "template-not-found-error"
     :error-message (format "%s can not be found." resource-path)}))