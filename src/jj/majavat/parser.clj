(ns jj.majavat.parser
  (:require [jj.majavat.lexer :as lexer]
            [jj.majavat.renderer.filters :as filters]
            [jj.majavat.renderer.sanitizer]
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


(def ^:private ^:const sanitizers {:html (jj.majavat.renderer.sanitizer/->Html)
                                   :json (jj.majavat.renderer.sanitizer/->Json)
                                   :none (jj.majavat.renderer.sanitizer/->None)
                                   })

(defn- create-filter-fn [{:keys [filter-name args]} filter-map]
  (if-let [f (get filter-map filter-name)]
    (fn [input] (f input args))
    (throw (ex-info (format "Unsupported filter: %s" (name filter-name))
                    {:type        :syntax-error
                     :filter-name filter-name
                     :line        1}))))

(defn- build-filter-fn [filter-specs filter-map]
  (if (empty? filter-specs)
    identity
    (apply comp (reverse (map #(create-filter-fn % filter-map) filter-specs)))))

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

(defn- push-tag [tag-stack tag-type line]
  (conj tag-stack {:tag tag-type :line (or line 1)}))

(defn- pop-tag [tag-stack expected-tag current-line]
  (if (empty? tag-stack)
    (throw (ex-info (format "Unexpected closing tag '%s' on line %s with no matching opening tag"
                            (name expected-tag) (or current-line "unknown"))
                    {:type         :syntax-error
                     :expected-tag expected-tag
                     :line         (or current-line 1)}))
    (let [top-tag (peek tag-stack)]
      (if (= (:tag top-tag) expected-tag)
        (pop tag-stack)
        (throw (ex-info (format "Mismatched closing tag on line %s: expected 'end%s' but found 'end%s' (opening tag was on line %s)"
                                (or current-line "unknown")
                                (name (:tag top-tag))
                                (name expected-tag)
                                (:line top-tag))
                        {:type         :syntax-error
                         :expected-tag (:tag top-tag)
                         :actual-tag   expected-tag
                         :opening-line (:line top-tag)
                         :closing-line (or current-line 1)}))))))

(defn- validate-tag-stack-empty [tag-stack]
  (when-not (empty? tag-stack)
    (let [unclosed-tag (peek tag-stack)]
      (throw (ex-info (format "Unclosed '%s' tag starting on line %s"
                              (name (:tag unclosed-tag))
                              (or (:line unclosed-tag) "unknown"))
                      {:type :syntax-error
                       :tag  (:tag unclosed-tag)
                       :line (or (:line unclosed-tag) 1)})))))

(defn- parse-ast
  ([lexed-list list current-block template-resolver filter-map merged-sanitizers]
   (parse-ast lexed-list list current-block false nil template-resolver filter-map  merged-sanitizers []))
  ([lexed-list list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack]
   (if (empty? lexed-list)
     (if parsing-for-body
       [list lexed-list tag-stack]
       (do
         (validate-tag-stack-empty tag-stack)
         list))
     (let [current-item (first lexed-list)]
       (case (:type current-item)
         :text (recur (rest lexed-list) (conj list current-item) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack)

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
                                        (assoc current-item :type :value-node :filter-fn (build-filter-fn filters filter-map)))]
                       (recur remaining-after-filters (conj list value-node) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack))

         :opening-bracket (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack)
         :closing-bracket (if (not (nil? (:value (last list))))
                            (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack)
                            (throw (ex-info (format "error on line %s" (:line current-item))
                                            {:type :syntax-error
                                             :line (:line current-item)})))

         :block-start (let [remaining (rest lexed-list)]
                        (if (and (seq remaining) (= :block-end (:type (first remaining))))
                          (let [remaining-after-block-end (rest remaining)]
                            (recur remaining-after-block-end list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack))
                          (recur remaining list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack)))
         :block-end (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack)

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
                (recur final-remaining (conj list now-node) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack))

         :keyword-csrf-token (recur (rest lexed-list) (apply conj list '({:type :text :value "<input type=\"hidden\" name=\"csrf_token\" value=\""}
                                                                         {:type :value-node :value [:csrf-token]}
                                                                         {:type :text :value "\">"}))
                                    current-block parsing-for-body current-file-path template-resolver filter-map  merged-sanitizers tag-stack)

         :keyword-query-string (let [remaining (rest lexed-list)
                                     query-string-decl-token (first remaining)
                                     remaining-after-decl (rest remaining)]
                                 (if (and query-string-decl-token (= :query-string-declaration (:type query-string-decl-token)))
                                   (let [block-end-token (first remaining-after-decl)]
                                     (if (and block-end-token (= :block-end (:type block-end-token)))
                                       (let [remaining-after-block-end (rest remaining-after-decl)
                                             query-string-node {:type  :query-string
                                                                :value (:variable-value query-string-decl-token)}]
                                         (recur remaining-after-block-end (conj list query-string-node) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack))
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
                                        included-content (parse-ast included-lexed [] {} false resolved-filename template-resolver filter-map merged-sanitizers [])]
                                    (recur remaining-after-filename (into list included-content) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack))
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
                                        [block-content remaining-after-block new-tag-stack] (parse-ast remaining-after-file-path [] current-block true current-file-path template-resolver filter-map merged-sanitizers tag-stack)
                                        parent-content (parse-ast parent-lexed [] {:content block-content} false resolved-file-path template-resolver filter-map merged-sanitizers [])]
                                    (recur remaining-after-block (into parent-content list) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers new-tag-stack))
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
                            (recur remaining-after-block-name (into list replacement-content) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack)
                            (let [[block-content remaining-after-block _] (parse-ast remaining-after-block-name [] current-block true current-file-path template-resolver filter-map merged-sanitizers tag-stack)]
                              (recur remaining-after-block (into list block-content) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers  tag-stack))))

         :keyword-let (let [remaining (rest lexed-list)
                            var-decl-token (first remaining)]
                        (if (and var-decl-token (= :variable-declaration (:type var-decl-token)))
                          (let [remaining-after-var-decl (rest remaining)
                                block-end-token (first remaining-after-var-decl)]
                            (if (and block-end-token (= :block-end (:type block-end-token)))
                              (let [remaining-after-block-end (rest remaining-after-var-decl)
                                    new-tag-stack (push-tag tag-stack :let (:line current-item))
                                    [body remaining-after-body updated-tag-stack] (parse-ast remaining-after-block-end [] current-block true current-file-path template-resolver filter-map merged-sanitizers new-tag-stack)
                                    node-type (if (string? (:variable-value var-decl-token))
                                                :variable-declaration
                                                :variable-assignment)
                                    let-node {:type           node-type
                                              :variable-name  (:variable-name var-decl-token)
                                              :variable-value (:variable-value var-decl-token)
                                              :body           body}]
                                (recur remaining-after-body (conj list let-node) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers updated-tag-stack))
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
                                        new-tag-stack (push-tag tag-stack :for (:line current-item))
                                        [body remaining-after-body updated-tag-stack] (parse-ast remaining-after-block-end [] current-block true current-file-path template-resolver filter-map merged-sanitizers new-tag-stack)
                                        for-node {:type       :for
                                                  :identifier (:value identifier-token)
                                                  :source     (:value source-token)
                                                  :body       body}]
                                    (recur remaining-after-body (conj list for-node) current-block parsing-for-body current-file-path template-resolver filter-map  merged-sanitizers updated-tag-stack))
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
                                       new-tag-stack (push-tag tag-stack :each (:line current-item))
                                       [body remaining-after-body updated-tag-stack] (parse-ast remaining-after-block-end [] current-block true current-file-path template-resolver filter-map merged-sanitizers new-tag-stack)
                                       each-node {:type       :each
                                                  :identifier (:value identifier-token)
                                                  :source     (:value source-token)
                                                  :body       body}]
                                   (recur remaining-after-body (conj list each-node) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers updated-tag-stack))
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
                           new-tag-stack (push-tag tag-stack :if (:line current-item))
                           [when-true remaining-after-true updated-tag-stack-1] (parse-ast remaining-after-block-end [] current-block true current-file-path template-resolver filter-map merged-sanitizers new-tag-stack)
                           [when-false remaining-final updated-tag-stack-2] (if (and (seq remaining-after-true)
                                                                                     (= :keyword-else (:type (first remaining-after-true))))
                                                                              (let [remaining-after-else (rest (rest remaining-after-true))
                                                                                    [else-body remaining-after-else-body updated-stack] (parse-ast remaining-after-else [] current-block true current-file-path template-resolver filter-map merged-sanitizers updated-tag-stack-1)]
                                                                                [else-body remaining-after-else-body updated-stack])
                                                                              [[{:type :text :value ""}] remaining-after-true updated-tag-stack-1])
                           if-node {:type       :if
                                    :condition  (:value condition-token)
                                    :when-true  when-true
                                    :when-false when-false}]
                       (if (some? (:value condition-token))
                         (recur remaining-final (conj list if-node) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers updated-tag-stack-2)
                         (throw (ex-info (format "error on line %s" (:line condition-token))
                                         {:type :syntax-error
                                          :line (:line condition-token)}))))

         :keyword-if-not (let [remaining (rest lexed-list)
                               condition-token (first remaining)
                               remaining-after-condition (rest remaining)
                               remaining-after-block-end (rest remaining-after-condition)
                               new-tag-stack (push-tag tag-stack :if (:line current-item))
                               [when-true remaining-after-true updated-tag-stack-1] (parse-ast remaining-after-block-end [] current-block true current-file-path template-resolver filter-map merged-sanitizers new-tag-stack)
                               [when-false remaining-final updated-tag-stack-2] (if (and (seq remaining-after-true)
                                                                                         (= :keyword-else (:type (first remaining-after-true))))
                                                                                  (let [remaining-after-else (rest (rest remaining-after-true))
                                                                                        [else-body remaining-after-else-body updated-stack] (parse-ast remaining-after-else [] current-block true current-file-path template-resolver filter-map merged-sanitizers updated-tag-stack-1)]
                                                                                    [else-body remaining-after-else-body updated-stack])
                                                                                  [[{:type :text :value ""}] remaining-after-true updated-tag-stack-1])
                               if-not-node {:type       :if
                                            :negate     true
                                            :condition  (:value condition-token)
                                            :when-true  when-true
                                            :when-false when-false}]
                           (if (some? (:value condition-token))
                             (recur remaining-final (conj list if-not-node) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers updated-tag-stack-2)
                             (throw (ex-info (format "error on line %s" (:line condition-token))
                                             {:type :syntax-error
                                              :line (:line condition-token)}))))

         :end-for (if parsing-for-body
                    (let [updated-tag-stack (pop-tag tag-stack :for (or (:line current-item) 1))]
                      [list (rest lexed-list) updated-tag-stack])
                    (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack))

         :keyword-escape (let [remaining (rest lexed-list)
                               name-token (first remaining)
                               remaining-after-name (rest remaining)
                               block-end-token (first remaining-after-name)]
                           (if (and (= :escape-name (:type name-token))
                                    (= :block-end (:type block-end-token)))
                             (let [remaining-after-block-end (rest remaining-after-name)
                                   new-tag-stack (push-tag tag-stack :sanitizer (:line current-item))
                                   [body remaining-after-body updated-tag-stack] (parse-ast remaining-after-block-end [] current-block true current-file-path template-resolver filter-map merged-sanitizers new-tag-stack)
                                   sanitizer-node {:type      :escape-block
                                                   :sanitizer (get merged-sanitizers (:value name-token))
                                                   :body      body}]
                               (recur remaining-after-body (conj list sanitizer-node) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers updated-tag-stack))
                             (throw (ex-info (format "error on line %s" (:line (or block-end-token name-token current-item)))
                                             {:type :syntax-error
                                              :line (:line (or block-end-token name-token current-item))}))))

         :keyword-end-escape (if parsing-for-body
                               (let [updated-tag-stack (pop-tag tag-stack :sanitizer (or (:line current-item) 1))]
                                 [list (rest lexed-list) updated-tag-stack])
                               (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack))

         :escape-name (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack)
         :end-each-token (if parsing-for-body
                           (let [updated-tag-stack (pop-tag tag-stack :each (or (:line current-item) 1))]
                             [list (rest lexed-list) updated-tag-stack])
                           (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack))

         :keyword-else (if parsing-for-body
                         [list lexed-list tag-stack]
                         (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack))

         :keyword-endif (if parsing-for-body
                          (let [updated-tag-stack (pop-tag tag-stack :if (or (:line current-item) 1))]
                            [list (rest lexed-list) updated-tag-stack])
                          (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack))

         :keyword-end-let (if parsing-for-body
                            (let [updated-tag-stack (pop-tag tag-stack :let (or (:line current-item) 1))]
                              [list (rest lexed-list) updated-tag-stack])
                            (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack))

         :keyword-in (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack)
         :each-in-token (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack)
         :each-identifier-token (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack)
         :variable-declaration (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack)
         :identifier (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack)
         :file-path (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack)
         :block-name (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack)
         :query-string-declaration (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack)
         :filter-tag (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack)
         :filter-function (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack)
         :filter-arg (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack)

         (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack))))))

(defn parse
  [resource-path template-resolver user-filters user-sanitizers]
  (if (cr/template-exists? template-resolver resource-path)
    (let [file-content (read-content-as-string template-resolver resource-path)
          lexed-value (lexer/tokenize file-content)
          filter-map (merge default-filter-map user-filters)
          merged-sanitizers (merge user-sanitizers sanitizers)
          ]
      (try
        (parse-ast lexed-value [] {} false resource-path template-resolver filter-map merged-sanitizers [] )
        (catch ExceptionInfo e
          (let [data (ex-data e)]
            (case (:type data)
              :template-not-found-error
              {:type          "template-not-found-error"
               :error-message (format "%s template can not be found" (:template data))}

              :syntax-error
              {:type          "syntax-error"
               :error-message (.getMessage e)
               :line          (str (get data :line 1))}

              {:type          "error"
               :error-message (.getMessage e)})))
        (catch Exception e
          {:type          "error"
           :error-message (.getMessage e)})))
    {:type          "template-not-found-error"
     :error-message (format "%s can not be found." resource-path)}))