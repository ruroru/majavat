(ns jj.majavat.parser
  (:require [clojure.walk :as walk]
            [jj.majavat.lexer :as lexer]
            [jj.majavat.renderer.filters :as filters]
            [jj.majavat.renderer.tests :as tests]
            [jj.majavat.renderer.sanitizer :as sanitizer]
            [jj.majavat.protocol.renderer.sanitizer :refer [sanitize]]
            [jj.majavat.protocol.resolver :as resolver]
            [jj.majavat.protocol.dictionary :as dictionary])
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

(def ^:private ^:const sanitizers {:html (sanitizer/->Html)
                                   :json (sanitizer/->Json)
                                   :none (sanitizer/->None)})

(def ^:private evalaution-functions {:default tests/default-test
                                     :even    tests/is-even?
                                     :odd     tests/is-odd?
                                     :empty   tests/is-empty?})

(defn- create-filter-fn [{:keys [filter-name args]} filter-map]
  (if-let [f (get filter-map filter-name)]
    (fn [input] (apply f input args))
    (throw (ex-info (format "Unsupported filter: %s" (name filter-name))
                    {:type        :syntax-error
                     :filter-name filter-name
                     :line        1}))))

(defn- build-filter-fn [filter-specs filter-map]
  (if (empty? filter-specs)
    identity
    (apply comp (reverse (map #(create-filter-fn % filter-map) filter-specs)))))

(defn- ->str [v]
  (if (string? v)
    v
    (str v)))

(defmacro ^:private reverse-call [a m]
  `(when-let [m# ~m]
     (m# ~a)))

(defn- local-get-in
  ([m a]
   (reverse-call a m))
  ([m a b]
   (-> m
       (reverse-call a)
       (reverse-call b)))
  ([m a b c]
   (-> m
       (reverse-call a)
       (reverse-call b)
       (reverse-call c)))
  ([m a b c d]
   (-> m
       (reverse-call a)
       (reverse-call b)
       (reverse-call c)
       (reverse-call d)))
  ([m a b c d e]
   (-> m
       (reverse-call a)
       (reverse-call b)
       (reverse-call c)
       (reverse-call d)
       (reverse-call e)))
  ([m a b c d e f]
   (-> m
       (reverse-call a)
       (reverse-call b)
       (reverse-call c)
       (reverse-call d)
       (reverse-call e)
       (reverse-call f))))

(defn resolve-path [context path]
  (cond
    (not (vector? path)) (context path)
    (record? context) (get-in context path)
    :else (case (count path)
            1 (let [[a] path] (local-get-in context a))
            2 (let [[a b] path] (local-get-in context a b))
            3 (let [[a b c] path] (local-get-in context a b c))
            4 (let [[a b c d] path] (local-get-in context a b c d))
            5 (let [[a b c d e] path] (local-get-in context a b c d e))
            6 (let [[a b c d e f] path] (local-get-in context a b c d e f))
            (get-in context path))))

(defn- bake-render-fn [{:keys [value filter-fn sanitizer]}]
  (let [filter-fn (or filter-fn identity)
        finish (if sanitizer
                 (fn [v] (sanitize sanitizer (->str (filter-fn v))))
                 (fn [v] (->str (filter-fn v))))]
    (fn
      ([context] (finish (resolve-path context value)))
      ([context _raw] (resolve-path context value)))))

(defn- expand-macro [body param arg]
  (walk/postwalk
    (fn [node]
      (if (map? node)
        (cond
          (= :macro-param-node (:type node))
          ((:builder node) arg)

          (vector? arg)
          (reduce (fn [n k]
                    (let [v (get n k)]
                      (if (and (vector? v) (= param (first v)))
                        (assoc n k (into arg (subvec v 1)))
                        n)))
                  node
                  [:value :source :condition])

          :else node)
        node))
    body))

(defn- resolve-file-path [base-path relative-path]
  (let [base-path-obj (Paths/get base-path (make-array String 0))
        relative-path-obj (Paths/get relative-path (make-array String 0))
        parent-path (or (.getParent base-path-obj)
                        (Paths/get "" (make-array String 0)))]
    (-> parent-path
        (.resolve relative-path-obj)
        (.normalize)
        (.toString))))

(defn- read-content-as-string [template-resolver content-path]
  (when-let [reader (PushbackReader. (resolver/open-reader template-resolver content-path))]
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

(defn- parse-operator-test
  [remaining-after-condition]
  (let [next-token (first remaining-after-condition)]
    (if (and next-token
             (= :operator (:type next-token))
             (= :is (:value next-token)))
      (let [remaining-after-op (rest remaining-after-condition)
            test-token (first remaining-after-op)
            remaining-after-test (rest remaining-after-op)]
        (if (= :equals (:value test-token))
          (let [ref-token (first remaining-after-test)
                ref-value (:value ref-token)
                remaining-after-ref (rest remaining-after-test)]
            [(fn [v] (= ref-value v))
             (rest remaining-after-ref)])
          [(get evalaution-functions (:value test-token) (:default evalaution-functions))
           (rest remaining-after-test)]))
      [(:default evalaution-functions)
       (rest remaining-after-condition)])))

(defn- parse-ast
  ([lexed-list list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param]
   (if (empty? lexed-list)
     (if parsing-for-body
       [list lexed-list tag-stack]
       (do
         (validate-tag-stack-empty tag-stack)
         list))
     (let [current-item (first lexed-list)]
       (case (:type current-item)
         :text (recur (rest lexed-list) (conj list current-item) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)

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
                                                                           has-paren (and (seq remaining-after-function)
                                                                                          (= :filter-paren-open (:type (first remaining-after-function))))
                                                                           remaining-after-paren (if has-paren (rest remaining-after-function) remaining-after-function)
                                                                           [args remaining-after-args] (loop [remaining remaining-after-paren
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
                           value-node (let [path (:value current-item)
                                            filter-fn (build-filter-fn filters filter-map)]
                                        (cond
                                          (nil? path)
                                          (assoc current-item :type :value-node)

                                          (and macro-param (= macro-param (first path)))
                                          {:type    :macro-param-node
                                           :builder (fn [arg]
                                                      (cond
                                                        (and (string? arg) (= [macro-param] path))
                                                        {:type :text :value (let [s (->str (filter-fn arg))]
                                                                              (if current-sanitizer
                                                                                (sanitize current-sanitizer s)
                                                                                s))}

                                                        (vector? arg)
                                                        {:type :value-node :render-fn (bake-render-fn {:value (into arg (subvec path 1)) :filter-fn filter-fn :sanitizer current-sanitizer})}

                                                        :else
                                                        {:type :value-node :render-fn (bake-render-fn {:value path :filter-fn filter-fn :sanitizer current-sanitizer})}))}

                                          :else
                                          {:type :value-node :render-fn (bake-render-fn {:value path :filter-fn filter-fn :sanitizer current-sanitizer})}))]
                       (recur remaining-after-filters (conj list value-node) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param))

         :opening-bracket (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)
         :closing-bracket (if (or (:render-fn (last list))
                                  (:builder (last list))
                                  (not (nil? (:value (last list)))))
                            (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)
                            (throw (ex-info (format "error on line %s" (:line current-item))
                                            {:type :syntax-error
                                             :line (:line current-item)})))

         :block-start (let [remaining (rest lexed-list)]
                        (if (and (seq remaining) (= :block-end (:type (first remaining))))
                          (let [remaining-after-block-end (rest remaining)]
                            (recur remaining-after-block-end list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param))
                          (recur remaining list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)))
         :block-end (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)

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
                (recur final-remaining (conj list now-node) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param))

         :keyword-csrf-token (recur (rest lexed-list) (apply conj list [{:type :text :value "<input type=\"hidden\" name=\"csrf_token\" value=\""}
                                                                        {:type :value-node :render-fn (bake-render-fn {:value [:csrf-token] :sanitizer current-sanitizer})}
                                                                        {:type :text :value "\">"}])
                                    current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)

         :keyword-query-string (let [remaining (rest lexed-list)
                                     query-string-decl-token (first remaining)
                                     remaining-after-decl (rest remaining)]
                                 (if (and query-string-decl-token (= :query-string-declaration (:type query-string-decl-token)))
                                   (let [block-end-token (first remaining-after-decl)]
                                     (if (and block-end-token (= :block-end (:type block-end-token)))
                                       (let [remaining-after-block-end (rest remaining-after-decl)
                                             query-string-node {:type  :query-string
                                                                :value (:variable-value query-string-decl-token)}]
                                         (recur remaining-after-block-end (conj list query-string-node) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param))
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
                                                        (resolve-file-path current-file-path filename)
                                                        filename)]
                                (if (resolver/template-exists? template-resolver resolved-filename)
                                  (let [file-content (read-content-as-string template-resolver resolved-filename)
                                        included-lexed (lexer/tokenize file-content)
                                        included-content (parse-ast included-lexed [] {} false resolved-filename template-resolver filter-map merged-sanitizers [] macros dictionary current-sanitizer macro-param)]
                                    (recur remaining-after-filename (into list included-content) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param))
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
                                                         (resolve-file-path current-file-path file-path)
                                                         file-path)]
                                (if (resolver/template-exists? template-resolver resolved-file-path)
                                  (let [parent-content-str (read-content-as-string template-resolver resolved-file-path)
                                        parent-lexed (lexer/tokenize parent-content-str)
                                        [block-content remaining-after-block new-tag-stack] (parse-ast remaining-after-file-path [] current-block true current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)
                                        parent-content (parse-ast parent-lexed [] {:content block-content} false resolved-file-path template-resolver filter-map merged-sanitizers [] macros dictionary current-sanitizer macro-param)]
                                    (recur remaining-after-block (into parent-content list) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers new-tag-stack macros dictionary current-sanitizer macro-param))
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
                            (recur remaining-after-block-name (into list replacement-content) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)
                            (let [[block-content remaining-after-block _] (parse-ast remaining-after-block-name [] current-block true current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)]
                              (recur remaining-after-block (into list block-content) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param))))

         :keyword-let (let [remaining (rest lexed-list)
                            var-decl-token (first remaining)]
                        (if (and var-decl-token (= :variable-declaration (:type var-decl-token)))
                          (let [remaining-after-var-decl (rest remaining)
                                block-end-token (first remaining-after-var-decl)]
                            (if (and block-end-token (= :block-end (:type block-end-token)))
                              (let [remaining-after-block-end (rest remaining-after-var-decl)
                                    new-tag-stack (push-tag tag-stack :let (:line current-item))
                                    [body remaining-after-body updated-tag-stack] (parse-ast remaining-after-block-end [] current-block true current-file-path template-resolver filter-map merged-sanitizers new-tag-stack macros dictionary current-sanitizer macro-param)
                                    node-type (if (string? (:variable-value var-decl-token))
                                                :variable-declaration
                                                :variable-assignment)
                                    let-node {:type           node-type
                                              :variable-name  (:variable-name var-decl-token)
                                              :variable-value (:variable-value var-decl-token)
                                              :body           body}]
                                (recur remaining-after-body (conj list let-node) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers updated-tag-stack macros dictionary current-sanitizer macro-param))
                              (throw (ex-info (format "error on line %s" (:line (or block-end-token var-decl-token)))
                                              {:type :syntax-error
                                               :line (:line (or block-end-token var-decl-token))}))))
                          (throw (ex-info (format "error on line %s" (:line (or var-decl-token current-item)))
                                          {:type :syntax-error
                                           :line (:line (or var-decl-token current-item))}))))

         :keyword-for (let [remaining (rest lexed-list)
                            identifier-token (first remaining)]
                        (if (not (= :block-end (:type identifier-token)))
                          (let [remaining-after-id (rest remaining)
                                separator-token (first remaining-after-id)
                                separator-type (:type separator-token)
                                each-mode? (= :only-token separator-type)]
                            (if (or (= :keyword-in separator-type) each-mode?)
                              (let [remaining-after-in (rest remaining-after-id)
                                    source-token (first remaining-after-in)
                                    remaining-after-source (rest remaining-after-in)
                                    block-end-token (first remaining-after-source)]
                                (if (some? (:value source-token))
                                  (let [remaining-after-block-end (rest remaining-after-source)
                                        tag-kind (if each-mode? :each :for)
                                        node-type (if each-mode? :each :for)
                                        new-tag-stack (push-tag tag-stack tag-kind (:line current-item))
                                        [body remaining-after-body updated-tag-stack] (parse-ast remaining-after-block-end [] current-block true current-file-path template-resolver filter-map merged-sanitizers new-tag-stack macros dictionary current-sanitizer macro-param)
                                        [when-empty remaining-after-empty final-tag-stack] (if (and (seq remaining-after-body)
                                                                                                    (= :keyword-empty (:type (first remaining-after-body))))
                                                                                             (let [remaining-after-empty-kw (rest (rest remaining-after-body))]
                                                                                               (parse-ast remaining-after-empty-kw [] current-block true current-file-path template-resolver filter-map merged-sanitizers updated-tag-stack macros dictionary current-sanitizer macro-param))
                                                                                             [nil remaining-after-body updated-tag-stack])
                                        for-node (cond-> {:type       node-type
                                                          :identifier (:value identifier-token)
                                                          :source     (:value source-token)
                                                          :body       body}
                                                         when-empty (assoc :when-empty when-empty))]
                                    (recur remaining-after-empty (conj list for-node) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers final-tag-stack macros dictionary current-sanitizer macro-param))
                                  (throw (ex-info (format "error on line %s" (:line block-end-token))
                                                  {:type :syntax-error
                                                   :line (:line block-end-token)}))))
                              (throw (ex-info (format "error on line %s" (:line separator-token))
                                              {:type :syntax-error
                                               :line (:line separator-token)}))))
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
                                       [body remaining-after-body updated-tag-stack] (parse-ast remaining-after-block-end [] current-block true current-file-path template-resolver filter-map merged-sanitizers new-tag-stack macros dictionary current-sanitizer macro-param)
                                       [when-empty remaining-after-empty final-tag-stack] (if (and (seq remaining-after-body)
                                                                                                   (= :keyword-empty (:type (first remaining-after-body))))
                                                                                            (let [remaining-after-empty-kw (rest (rest remaining-after-body))]
                                                                                              (parse-ast remaining-after-empty-kw [] current-block true current-file-path template-resolver filter-map merged-sanitizers updated-tag-stack macros dictionary current-sanitizer macro-param))
                                                                                            [nil remaining-after-body updated-tag-stack])
                                       each-node (cond-> {:type       :each
                                                          :identifier (:value identifier-token)
                                                          :source     (:value source-token)
                                                          :body       body}
                                                         when-empty (assoc :when-empty when-empty))]
                                   (recur remaining-after-empty (conj list each-node) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers final-tag-stack macros dictionary current-sanitizer macro-param))
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
                           [eval-fn remaining-after-block-end] (parse-operator-test remaining-after-condition)]
                       (if (some? (:value condition-token))
                         (let [new-tag-stack (push-tag tag-stack :if (:line current-item))
                               [branches else-body remaining-final final-tag-stack]
                               (loop [branches []
                                      current-condition {:condition           (:value condition-token)
                                                         :evaluation-function eval-fn}
                                      remaining remaining-after-block-end
                                      ts new-tag-stack]
                                 (let [[body remaining-after-body updated-ts] (parse-ast remaining [] current-block true current-file-path template-resolver filter-map merged-sanitizers ts macros dictionary current-sanitizer macro-param)
                                       next-token (first remaining-after-body)]
                                   (cond
                                     (and next-token (or (= :keyword-elif (:type next-token))
                                                         (= :keyword-elif-not (:type next-token))))
                                     (let [elif-remaining (rest remaining-after-body)
                                           elif-condition-token (first elif-remaining)
                                           elif-remaining-after-condition (rest elif-remaining)
                                           elif-remaining-after-block-end (rest elif-remaining-after-condition)
                                           negate? (= :keyword-elif-not (:type next-token))
                                           elif-cond (cond-> {:condition           (:value elif-condition-token)
                                                              :evaluation-function (:default evalaution-functions)}
                                                             negate? (assoc :negate true))]
                                       (recur (conj branches [current-condition body])
                                              elif-cond
                                              elif-remaining-after-block-end
                                              updated-ts))

                                     (and next-token (= :keyword-else (:type next-token)))
                                     (let [remaining-after-else (rest (rest remaining-after-body))
                                           [else-body remaining-after-else-body final-ts] (parse-ast remaining-after-else [] current-block true current-file-path template-resolver filter-map merged-sanitizers updated-ts macros dictionary current-sanitizer macro-param)]
                                       [(conj branches [current-condition body]) else-body remaining-after-else-body final-ts])

                                     :else
                                     [(conj branches [current-condition body]) [] remaining-after-body updated-ts])))
                               if-node {:type     :if
                                        :branches branches
                                        :else     else-body}]
                           (recur remaining-final (conj list if-node) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers final-tag-stack macros dictionary current-sanitizer macro-param))
                         (throw (ex-info (format "error on line %s" (:line condition-token))
                                         {:type :syntax-error
                                          :line (:line condition-token)}))))

         :keyword-if-not (let [remaining (rest lexed-list)
                               condition-token (first remaining)
                               remaining-after-condition (rest remaining)
                               remaining-after-block-end (rest remaining-after-condition)]
                           (if (some? (:value condition-token))
                             (let [new-tag-stack (push-tag tag-stack :if (:line current-item))
                                   [branches else-body remaining-final final-tag-stack]
                                   (loop [branches []
                                          current-condition {:condition           (:value condition-token)
                                                             :negate              true
                                                             :evaluation-function (:default evalaution-functions)}
                                          remaining remaining-after-block-end
                                          ts new-tag-stack]
                                     (let [[body remaining-after-body updated-ts] (parse-ast remaining [] current-block true current-file-path template-resolver filter-map merged-sanitizers ts macros dictionary current-sanitizer macro-param)
                                           next-token (first remaining-after-body)]
                                       (cond
                                         (and next-token (or (= :keyword-elif (:type next-token))
                                                             (= :keyword-elif-not (:type next-token))))
                                         (let [elif-remaining (rest remaining-after-body)
                                               elif-condition-token (first elif-remaining)
                                               elif-remaining-after-condition (rest elif-remaining)
                                               elif-remaining-after-block-end (rest elif-remaining-after-condition)
                                               negate? (= :keyword-elif-not (:type next-token))
                                               elif-cond (cond-> {:condition           (:value elif-condition-token)
                                                                  :evaluation-function (:default evalaution-functions)}
                                                                 negate? (assoc :negate true))]
                                           (recur (conj branches [current-condition body])
                                                  elif-cond
                                                  elif-remaining-after-block-end
                                                  updated-ts))

                                         (and next-token (= :keyword-else (:type next-token)))
                                         (let [remaining-after-else (rest (rest remaining-after-body))
                                               [else-body remaining-after-else-body final-ts] (parse-ast remaining-after-else [] current-block true current-file-path template-resolver filter-map merged-sanitizers updated-ts macros dictionary current-sanitizer macro-param)]
                                           [(conj branches [current-condition body]) else-body remaining-after-else-body final-ts])

                                         :else
                                         [(conj branches [current-condition body]) [] remaining-after-body updated-ts])))
                                   if-node {:type     :if
                                            :branches branches
                                            :else     else-body}]
                               (recur remaining-final (conj list if-node) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers final-tag-stack macros dictionary current-sanitizer macro-param))
                             (throw (ex-info (format "error on line %s" (:line condition-token))
                                             {:type :syntax-error
                                              :line (:line condition-token)}))))

         :end-for (if parsing-for-body
                    (let [updated-tag-stack (pop-tag tag-stack :for (or (:line current-item) 1))]
                      [list (rest lexed-list) updated-tag-stack])
                    (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param))

         :keyword-escape (let [remaining (rest lexed-list)
                               name-token (first remaining)
                               remaining-after-name (rest remaining)
                               block-end-token (first remaining-after-name)]
                           (if (and (= :escape-name (:type name-token))
                                    (= :block-end (:type block-end-token)))
                             (let [remaining-after-block-end (rest remaining-after-name)
                                   new-tag-stack (push-tag tag-stack :sanitizer (:line current-item))
                                   [body remaining-after-body updated-tag-stack] (parse-ast remaining-after-block-end [] current-block true current-file-path template-resolver filter-map merged-sanitizers new-tag-stack macros dictionary (get merged-sanitizers (:value name-token)) macro-param)]
                               (recur remaining-after-body (into list body) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers updated-tag-stack macros dictionary current-sanitizer macro-param))
                             (throw (ex-info (format "error on line %s" (:line (or block-end-token name-token current-item)))
                                             {:type :syntax-error
                                              :line (:line (or block-end-token name-token current-item))}))))

         :keyword-end-escape (if parsing-for-body
                               (let [updated-tag-stack (pop-tag tag-stack :sanitizer (or (:line current-item) 1))]
                                 [list (rest lexed-list) updated-tag-stack])
                               (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param))

         :escape-name (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)
         :end-each-token (if parsing-for-body
                           (let [updated-tag-stack (pop-tag tag-stack :each (or (:line current-item) 1))]
                             [list (rest lexed-list) updated-tag-stack])
                           (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param))

         :keyword-else (if parsing-for-body
                         [list lexed-list tag-stack]
                         (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param))

         :keyword-elif (if parsing-for-body
                         [list lexed-list tag-stack]
                         (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param))

         :keyword-elif-not (if parsing-for-body
                             [list lexed-list tag-stack]
                             (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param))

         :keyword-empty (if parsing-for-body
                          [list lexed-list tag-stack]
                          (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param))

         :keyword-endif (if parsing-for-body
                          (let [updated-tag-stack (pop-tag tag-stack :if (or (:line current-item) 1))]
                            [list (rest lexed-list) updated-tag-stack])
                          (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param))

         :keyword-end-let (if parsing-for-body
                            (let [updated-tag-stack (pop-tag tag-stack :let (or (:line current-item) 1))]
                              [list (rest lexed-list) updated-tag-stack])
                            (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param))

         :keyword-macro (let [remaining (rest lexed-list)
                                 name-token (first remaining)
                                 remaining-after-name (rest remaining)
                                 [param-token remaining-after-signature]
                                 (if (= :open-paren (:type (first remaining-after-name)))
                                   (let [after-open (rest remaining-after-name)]
                                     (if (= :macro-param (:type (first after-open)))
                                       [(first after-open) (rest (rest after-open))]
                                       [nil (rest after-open)]))
                                   [nil remaining-after-name])
                                 block-end-token (first remaining-after-signature)]
                             (if (and name-token (= :macro-name (:type name-token))
                                      block-end-token (= :block-end (:type block-end-token)))
                               (let [remaining-after-block-end (rest remaining-after-signature)
                                     new-tag-stack (push-tag tag-stack :macro (:line current-item))
                                     [body remaining-after-body updated-tag-stack] (parse-ast remaining-after-block-end [] current-block true current-file-path template-resolver filter-map merged-sanitizers new-tag-stack macros dictionary current-sanitizer (:value param-token))]
                                 (swap! macros assoc (:value name-token) (fn [arg] (expand-macro body (:value param-token) arg)))
                                 (recur remaining-after-body list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers updated-tag-stack macros dictionary current-sanitizer macro-param))
                               (throw (ex-info (format "error on line %s" (:line (or block-end-token name-token current-item)))
                                               {:type :syntax-error
                                                :line (:line (or block-end-token name-token current-item))}))))

         :keyword-end-macro (if parsing-for-body
                                 (let [updated-tag-stack (pop-tag tag-stack :macro (or (:line current-item) 1))]
                                   [list (rest lexed-list) updated-tag-stack])
                                 (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param))

         :macro-name (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)

         :macro-call (let [macro-def (get @macros (:value current-item))
                           after-open (rest (rest lexed-list))
                           [arg-token remaining-after-call]
                           (if (= :macro-arg (:type (first after-open)))
                             [(first after-open) (rest (rest after-open))]
                             [nil (rest after-open)])]
                       (if macro-def
                         (recur remaining-after-call (into list (macro-def (:value arg-token))) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)
                         (throw (ex-info (format "unknown tag or macro '%s' on line %s" (name (:value current-item)) (:line current-item 1))
                                         {:type :syntax-error
                                          :line (:line current-item 1)}))))


         :keyword-in (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)
         :each-in-token (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)
         :each-identifier-token (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)
         :variable-declaration (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)
         :identifier (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)

         :token/translation (let [remaining (rest lexed-list)
                                  next-token (first remaining)]
                              (if (= :token/translation-key (:type next-token))
                                (let [key (:value next-token)
                                      trans-fn (if dictionary
                                                (fn [locale] (dictionary/translate dictionary locale key))
                                                (constantly nil))]
                                  (recur (rest remaining) (conj list {:type :translation :trans-fn trans-fn}) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param))
                                (recur remaining list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)))

         :token/translation-key (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)

         :token/debug (let [remaining (rest lexed-list)
                            next-token (first remaining)]
                        (if (= :token/debug-target (:type next-token))
                          (recur (rest remaining) (conj list {:type :debug :target (:value next-token)}) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)
                          (recur remaining (conj list {:type :debug :target :default}) current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)))

         :token/debug-target (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)

         :file-path (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)
         :block-name (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)
         :query-string-declaration (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)
         :filter-tag (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)
         :filter-function (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)
         :filter-arg (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)
         :operator (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)
         :operator-test (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)
         :reference-objet (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)
         :comparative (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param)

         (recur (rest lexed-list) list current-block parsing-for-body current-file-path template-resolver filter-map merged-sanitizers tag-stack macros dictionary current-sanitizer macro-param))))))

(defn parse
  ([resource-path template-resolver user-filters user-sanitizers]
   (parse resource-path template-resolver user-filters user-sanitizers nil nil))
  ([resource-path template-resolver user-filters user-sanitizers dictionary]
   (parse resource-path template-resolver user-filters user-sanitizers dictionary nil))
  ([resource-path template-resolver user-filters user-sanitizers dictionary current-sanitizer]
  (if (resolver/template-exists? template-resolver resource-path)
    (let [file-content (read-content-as-string template-resolver resource-path)
          lexed-value (lexer/tokenize file-content)
          filter-map (merge default-filter-map user-filters)
          merged-sanitizers (merge user-sanitizers sanitizers)
          macros (atom {})]
      (try
        (parse-ast lexed-value [] {} false resource-path template-resolver filter-map merged-sanitizers [] macros dictionary current-sanitizer nil)
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
     :error-message (format "%s can not be found." resource-path)})))
