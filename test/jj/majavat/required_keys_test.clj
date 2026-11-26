(ns jj.majavat.required-keys-test
  (:require [clojure.test :refer [are deftest is]]
            [jj.majavat.parser :as parser]
            [jj.majavat.renderer :refer [->PartialRenderer]]
            [jj.majavat.renderer.sanitizer :refer [->None]]
            [jj.majavat.renderer.json :refer [->DefaultJsonSerializer]]
            [jj.majavat.protocol.mock-dictionary :refer [create-mock-dictionary]]
            [jj.majavat.protocol.renderer.render-target :as rt]
            [jj.majavat.error-handler.fail-fast :as fail-fast]
            [jj.majavat.resolver.resource :as rcr]
            [jj.majavat.resolver.string :refer [->StringResolver]]))

(def ^:private dict (create-mock-dictionary))
(def ^:private json-ser (->DefaultJsonSerializer))

(defn- build
  ([template] (build template nil))
  ([template fragment]
   (parser/parse-template "template" (->StringResolver "template" template) {} {} dict (->None) json-ser fragment)))

(defn- keys-of
  ([template] (parser/required-keys (build template)))
  ([template fragment] (parser/required-keys (build template fragment))))

(deftest values
  (are [expected template] (= expected (keys-of template))
    #{} "plain text"
    #{[:title] [:user :name]} "{{ title }} {{ user.name | upper-case }}"
    #{[:user :ns/user.id]} "{{ user.`ns/user.id` }}"
    #{[:user] [:user :name]} "{{ user }} {{ user.name }}"))

(deftest loops-bind-their-item
  (are [expected template] (= expected (keys-of template))
    #{[:items] [:title]} "{% for item in items %}{{ item.name }} {{ loop.index }} {{ title }}{% endfor %}"
    #{[:items] [:item]} "{% for item in items %}x{% empty %}{{ item }}{% endfor %}"
    #{[:groups]} "{% for g in groups %}{% for m in g.members %}{{ m.name }}{% endfor %}{% endfor %}"
    #{[:item :children]} "{% for item in item.children %}{{ item }}{% endfor %}"))

(deftest let-binds-its-variable
  (are [expected template] (= expected (keys-of template))
    #{[:account :owner]} "{% let u = account.owner %}{{ u.name }}{% endlet %}"
    #{[:u]} "{% let u = \"x\" %}{{ u }}{% endlet %}{{ u }}"
    #{[:users]} "{% for u in users %}{% let n = u.name %}{{ n }}{% endlet %}{% endfor %}"))

(deftest conditions-and-branches
  (are [expected template] (= expected (keys-of template))
    #{[:a :b] [:x] [:c] [:y] [:z]} "{% if a.b %}{{ x }}{% elif c > 1 %}{{ y }}{% else %}{{ z }}{% endif %}"
    #{[:a]} "{% if not a %}no{% endif %}"
    #{[:a] [:b :c]} "{% if a == b.c %}yes{% endif %}"
    #{[:a] [:b]} "{% macro same(x) %}{% if a == x %}y{% endif %}{% endmacro %}{% same(b) %}"
    #{[:items]} "{% for i in items %}{% if i is even %}{{ i }}{% endif %}{% endfor %}"))

(deftest macros
  (are [expected template] (= expected (keys-of template))
    #{} "{% macro unused %}{{ x }}{% endmacro %}"
    #{[:user :name] [:suffix]} "{% macro greet(who) %}{{ who.name }}{{ suffix }}{% endmacro %}{% greet(user) %}"
    #{} "{% macro greet(who) %}{{ who }}{% endmacro %}{% greet(\"jj\") %}"
    #{[:xs]} "{% macro li(v) %}<li>{{ v }}</li>{% endmacro %}{% for x in xs %}{% li(x) %}{% endfor %}"
    #{[:csrf-token] [:locale] [:params]} "{% csrf-token() %}{% trans(hello) %}{% query-string(params) %}{% now() %}"))

(deftest includes-and-extends
  (is (= #{[:user :name] [:user :email]}
         (parser/required-keys
           (parser/parse-template "inheritance-test" (rcr/->ResourceResolver) {} {} dict (->None) json-ser nil)))))

(deftest fragments
  (let [template "<ul>{% fragment row %}<li>{{ item.name }}</li>{% endfragment %}</ul>{{ title }}"]
    (is (= #{[:item :name] [:title]} (keys-of template)))
    (is (= #{[:item :name]} (keys-of template :row)))))

(deftest pre-rendered-keys-are-gone
  (is (= #{[:user] [:flag]}
         (parser/required-keys
           (rt/render (->PartialRenderer)
                      (build "{{ site }} {{ user }}{% if beta %}{{ flag }}{% endif %}")
                      {:site "Acme" :beta true}
                      (fail-fast/->FailFast)))))
  (is (= #{[:limit]}
         (parser/required-keys
           (rt/render (->PartialRenderer)
                      (build "{% if score > limit %}high{% endif %}")
                      {:score 3}
                      (fail-fast/->FailFast))))))

(deftest error-passes-through
  (let [error (build "{% if x %}")]
    (is (map? error))
    (is (= error (parser/required-keys error)))))
