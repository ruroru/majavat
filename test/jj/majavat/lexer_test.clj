(ns jj.majavat.lexer-test
  (:require [clojure.test :refer [deftest is]]
            [jj.majavat.lexer :as lexer]))

(deftest lex-test
  (is (= [{:type  :text
           :value "hello world"}]
         (lexer/tokenize "hello world"))))

(deftest lex-opening-closing-bracket-test
  (is (= [
          {:type :text :value "hello "}
          {:type :opening-bracket}
          {:type :expression :value [:name]}
          {:type :closing-bracket}
          {:type :text :value ", hello "}
          {:type :opening-bracket}
          {:type :expression :value [:user :name]}
          {:type :closing-bracket}
          ]
         (lexer/tokenize "hello {{ name }}, hello {{ user.name }}"))))



(deftest for-loop-test1asd
  (is (= [
          {:type :block-start}
          {:type :keyword-for}
          {:type :identifier :value :world}

          {:type :keyword-in}
          {:type :identifier :value [:planets]}
          {:type :block-end}
          {:type :text :value " hello "}

          {:type :opening-bracket}
          {:type :expression :value [:world]}
          {:type :closing-bracket}

          {:type :block-start}
          {:type :end-for}
          {:type :block-end}
          ]
         (lexer/tokenize "{% for world in planets %} hello {{ world }}{% endfor %}"))))




(deftest if-statement
  (is (= [{:type :block-start}
          {:type :keyword-if}
          {:type  :identifier
           :value [:some :condition]}
          {:type :block-end}
          {:type  :text
           :value "yes"}
          {:type :block-start}
          {:type :keyword-endif}
          {:type :block-end}]
         (lexer/tokenize "{% if some.condition %}yes{% endif %}"))))


(deftest if-else-statement
  (is (= [
          {:type :text :value "testing if "}
          {:type :block-start}
          {:type :keyword-if}
          {:type :identifier :value [:condition]}
          {:type :block-end}
          {:type :text :value "yes!"}
          {:type :block-start}
          {:type :keyword-else}
          {:type :block-end}
          {:type :text :value "no!"}
          {:type :block-start}
          {:type :keyword-endif}
          {:type :block-end}
          ]
         (lexer/tokenize "testing if {% if condition %}yes!{% else %}no!{% endif %}"))))

(deftest if-else-statement
  (is (= [{:type  :text
           :value "testing "}
          {:type :block-start}
          {:type :keyword-include}
          {:type  :file-path
           :value "file.txt"}
          {:type :block-end}]
         (lexer/tokenize "testing {% include \"file.txt\" %}"))))

(deftest bloc-extends-test
  (is (= [{:type  :text
           :value "hello world "}
          {:type :block-start}
          {:type :keyword-block}
          {:type  :block-name
           :value :special-block}
          {:type :block-end}]
         (lexer/tokenize "hello world {% block special-block %}"))))


(deftest extends-test1
  (is (= [{:type  :text
           :value "testing "}
          {:type :block-start}
          {:type :keyword-extends}
          {:type  :extends-block-name
           :value :special-block}
          {:type  :file-path
           :value "file.txt"}
          {:type :block-end}]
         (lexer/tokenize "testing {% extends special-block \"file.txt\" %}"))))


