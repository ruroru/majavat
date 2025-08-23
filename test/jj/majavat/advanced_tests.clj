(ns jj.majavat.advanced-tests
  (:require [clojure.test :refer [deftest is testing]]
            [jj.majavat.lexer :as lexer]
            [jj.majavat.parser :as parser]
            [jj.majavat.resolver.resource :as rcr]))

(def contentResolver (rcr/->ResourceResolver))

(deftest test-deeply-nested-conditionals
  (is (= [{:condition  [:user
                        :is_admin]
           :type       :if
           :when-false [{:condition  [:user
                                      :is_logged_in]
                         :type       :if
                         :when-false [{:type  :text
                                       :value "Please log in"}]
                         :when-true  [{:type  :text
                                       :value "Welcome "}
                                      {:type  :value-node
                                       :value [:user
                                               :name]}
                                      {:type  :text
                                       :value "!"}]}]
           :when-true  [{:condition  [:user
                                      :has_permissions]
                         :type       :if
                         :when-false [{:type  :text
                                       :value "Access Denied - No Permissions"}]
                         :when-true  [{:type  :text
                                       :value "Admin Panel: "}
                                      {:type  :value-node
                                       :value [:user
                                               :name]}]}]}]

         (parser/parse "deeply-nested-conditionals.txt" contentResolver))))

(deftest test-for-loop-with-nested-everything
  (is (= [{:body       [{:type  :text
                         :value "Department: "}
                        {:type  :value-node
                         :value [:department
                                 :name]}
                        {:type  :text
                         :value " (Budget: "}
                        {:type  :value-node
                         :value [:department
                                 :budget]}
                        {:type  :text
                         :value ")"}
                        {:body       [{:condition  [:employeew
                                                    :is_manager]
                                       :type       :if
                                       :when-false [{:type  :text
                                                     :value "👤 "}
                                                    {:type  :value-node
                                                     :value [:employee
                                                             :name]}
                                                    {:type  :text
                                                     :value " - "}
                                                    {:type  :value-node
                                                     :value [:employee
                                                             :title]}]
                                       :when-true  [{:type  :text
                                                     :value "👔 MANAGER: "}
                                                    {:type  :value-node
                                                     :value [:employee
                                                             :name]}
                                                    {:type  :text
                                                     :value " - "}
                                                    {:type  :value-node
                                                     :value [:employee
                                                             :title]}
                                                    {:body       [{:type  :value-node
                                                                   :value [:report
                                                                           :name]}
                                                                  {:type  :text
                                                                   :value " ("}
                                                                  {:type  :value-node
                                                                   :value [:report
                                                                           :role]}
                                                                  {:type  :text
                                                                   :value ")"}]
                                                     :identifier :report
                                                     :source     [:employee
                                                                  :direct_reports]
                                                     :type       :for}]}]
                         :identifier :employee
                         :source     [:department
                                      :employees]
                         :type       :for}]
           :identifier :department
           :source     [:company
                        :departments]
           :type       :for}]

         (parser/parse "nested-for-loops.txt" contentResolver))))


(deftest test-simple-text
  "Test lexing plain text without any template syntax"
  (is (= [{:type :text :value "Hello world!"}]
         (lexer/tokenize "Hello world!"))))

(deftest test-simple-expression
  "Test lexing a simple variable expression"
  (is (= [{:type  :text
           :value "Hello "}
          {:type :opening-bracket}
          {:type  :expression
           :value [:name]}
          {:line 1
           :type :closing-bracket}
          {:type  :text
           :value "!"}]
         (lexer/tokenize "Hello {{name}}!"))))

(deftest test-nested-property-expression
  "Test lexing expressions with nested properties"
  (is (= [{:type :opening-bracket}
          {:type  :expression
           :value [:user
                   :profile
                   :name]}
          {:line 1
           :type :closing-bracket}]
         (lexer/tokenize "{{user.profile.name}}"))))

(deftest test-simple-for-loop
  "Test lexing a basic for loop structure"
  (is (= [{:type :block-start}
          {:type :keyword-for}
          {:type  :identifier
           :value :item}
          {:type :keyword-in}
          {:type  :identifier
           :value [:items]}
          {:line 1
           :type :block-end}
          {:type  :text
           :value "Item: "}
          {:type :opening-bracket}
          {:type  :expression
           :value [:item]}
          {:line 1
           :type :closing-bracket}
          {:type :block-start}
          {:type :end-for}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{% for item in items %}Item: {{item}}{% endfor %}"))))

(deftest test-for-loop-with-nested-source
  "Test for loop with nested property as source"
  (is (= [{:type :block-start}
          {:type :keyword-for}
          {:type  :identifier
           :value :product}
          {:type :keyword-in}
          {:type  :identifier
           :value [:category
                   :products]}
          {:line 1
           :type :block-end}
          {:type :opening-bracket}
          {:type  :expression
           :value [:product
                   :name]}
          {:line 1
           :type :closing-bracket}
          {:type :block-start}
          {:type :end-for}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{% for product in category.products %}{{product.name}}{% endfor %}"))))

(deftest test-simple-if-statement
  "Test lexing a basic if statement"
  (is (= [{:type :block-start}
          {:type :keyword-if}
          {:type  :identifier
           :value [:user
                   :is_active]}
          {:line 1
           :type :block-end}
          {:type  :text
           :value "Welcome!"}
          {:type :block-start}
          {:type :keyword-endif}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{% if user.is_active %}Welcome!{% endif %}"))))

(deftest test-if-else-statement
  "Test lexing if-else structure"
  (is (= [{:type :block-start}
          {:type :keyword-if}
          {:type  :identifier
           :value [:user
                   :is_premium]}
          {:line 1
           :type :block-end}
          {:type  :text
           :value "Premium features enabled"}
          {:type :block-start}
          {:type :keyword-else}
          {:line 1
           :type :block-end}
          {:type  :text
           :value "Upgrade to premium"}
          {:type :block-start}
          {:type :keyword-endif}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{% if user.is_premium %}Premium features enabled{% else %}Upgrade to premium{% endif %}"))))

(deftest test-nested-for-loops
  "Test lexing nested for loops"
  (is (= [{:type :block-start}
          {:type :keyword-for}
          {:type  :identifier
           :value :category}
          {:type :keyword-in}
          {:type  :identifier
           :value [:categories]}
          {:line 1
           :type :block-end}
          {:type  :text
           :value "Category: "}
          {:type :opening-bracket}
          {:type  :expression
           :value [:category
                   :name]}
          {:line 1
           :type :closing-bracket}
          {:type  :text
           :value "\n"}
          {:type :block-start}
          {:type :keyword-for}
          {:type  :identifier
           :value :item}
          {:type :keyword-in}
          {:type  :identifier
           :value [:category
                   :items]}
          {:line 2
           :type :block-end}
          {:type  :text
           :value "  - "}
          {:type :opening-bracket}
          {:type  :expression
           :value [:item
                   :name]}
          {:line 2
           :type :closing-bracket}
          {:type  :text
           :value "\n"}
          {:type :block-start}
          {:type :end-for}
          {:line 3
           :type :block-end}
          {:type :block-start}
          {:type :end-for}
          {:line 3
           :type :block-end}]
         (lexer/tokenize "{% for category in categories %}Category: {{category.name}}\n{% for item in category.items %}  - {{item.name}}\n{% endfor %}{% endfor %}"))))

(deftest test-nested-if-statements
  "Test lexing nested if statements"
  (is (= [{:type :block-start}
          {:type :keyword-if}
          {:type  :identifier
           :value [:user
                   :is_logged_in]}
          {:line 1
           :type :block-end}
          {:type  :text
           :value "Welcome "}
          {:type :opening-bracket}
          {:type  :expression
           :value [:user
                   :name]}
          {:line 1
           :type :closing-bracket}
          {:type  :text
           :value "! "}
          {:type :block-start}
          {:type :keyword-if}
          {:type  :identifier
           :value [:user
                   :has_notifications]}
          {:line 1
           :type :block-end}
          {:type  :text
           :value "You have "}
          {:type :opening-bracket}
          {:type  :expression
           :value [:user
                   :notification_count]}
          {:line 1
           :type :closing-bracket}
          {:type  :text
           :value " new messages."}
          {:type :block-start}
          {:type :keyword-else}
          {:line 1
           :type :block-end}
          {:type  :text
           :value "No new messages."}
          {:type :block-start}
          {:type :keyword-endif}
          {:line 1
           :type :block-end}
          {:type :block-start}
          {:type :keyword-else}
          {:line 1
           :type :block-end}
          {:type  :text
           :value "Please log in."}
          {:type :block-start}
          {:type :keyword-endif}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{% if user.is_logged_in %}Welcome {{user.name}}! {% if user.has_notifications %}You have {{user.notification_count}} new messages.{% else %}No new messages.{% endif %}{% else %}Please log in.{% endif %}"))))

(deftest test-for-loop-with-nested-if
  "Test for loop containing if-else statements"
  (is (= [{:type :block-start}
          {:type :keyword-for}
          {:type  :identifier
           :value :product}
          {:type :keyword-in}
          {:type  :identifier
           :value [:products]}
          {:line 1
           :type :block-end}
          {:type  :text
           :value "Product: "}
          {:type :opening-bracket}
          {:type  :expression
           :value [:product
                   :name]}
          {:line 1
           :type :closing-bracket}
          {:type  :text
           :value " - "}
          {:type :block-start}
          {:type :keyword-if}
          {:type  :identifier
           :value [:product
                   :in_stock]}
          {:line 1
           :type :block-end}
          {:type  :text
           :value "Available ("}
          {:type :opening-bracket}
          {:type  :expression
           :value [:product
                   :price]}
          {:line 1
           :type :closing-bracket}
          {:type  :text
           :value ")"}
          {:type :block-start}
          {:type :keyword-else}
          {:line 1
           :type :block-end}
          {:type  :text
           :value "Out of Stock"}
          {:type :block-start}
          {:type :keyword-endif}
          {:line 1
           :type :block-end}
          {:type  :text
           :value "\n"}
          {:type :block-start}
          {:type :end-for}
          {:line 2
           :type :block-end}]
         (lexer/tokenize "{% for product in products %}Product: {{product.name}} - {% if product.in_stock %}Available ({{product.price}}){% else %}Out of Stock{% endif %}\n{% endfor %}"))))

(deftest test-whitespace-handling
  "Test lexing with various whitespace patterns"
  (testing "Extra spaces in expressions"
    (is (= [{:type :opening-bracket}
            {:type  :expression
             :value [:user
                     :name]}
            {:line 1
             :type :closing-bracket}]
           (lexer/tokenize "{{  user.name  }}"))))

  (testing "Extra spaces in control blocks"
    (is (= [{:type :block-start}
            {:type :keyword-for}
            {:type  :identifier
             :value :item}
            {:type :keyword-in}
            {:type  :identifier
             :value [:items]}
            {:line 1
             :type :block-end}
            {:type :block-start}
            {:type :end-for}
            {:line 1
             :type :block-end}]
           (lexer/tokenize "{%  for  item  in  items  %}{%  endfor  %}")))))

(deftest test-empty-blocks
  "Test lexing empty control structures"
  (is (= [{:type :block-start}
          {:type :keyword-if}
          {:type  :identifier
           :value [:condition]}
          {:line 1
           :type :block-end}
          {:type :block-start}
          {:type :keyword-endif}
          {:line 1
           :type :block-end}]
         (lexer/tokenize "{% if condition %}{% endif %}"))))

(deftest test-multiline-template
  "Test lexing template with newlines and indentation"
  (is (= [{:type  :text
           :value "Hello "}
          {:type :opening-bracket}
          {:type  :expression
           :value [:name]}
          {:line 1
           :type :closing-bracket}
          {:type  :text
           :value "!\n"}
          {:type :block-start}
          {:type :keyword-if}
          {:type  :identifier
           :value [:show_details]}
          {:line 2
           :type :block-end}
          {:type  :text
           :value "\n  Details: "}
          {:type :opening-bracket}
          {:type  :expression
           :value [:details]}
          {:line 3
           :type :closing-bracket}
          {:type  :text
           :value "\n"}
          {:type :block-start}
          {:type :keyword-endif}
          {:line 4
           :type :block-end}]
         (lexer/tokenize "Hello {{name}}!\n{% if show_details %}\n  Details: {{details}}\n{% endif %}"))))


(deftest test-special-characters-in-text

  (is (= [{:type :text :value "Price: $29.99 (was $39.99) - 25% off!"}]
         (lexer/tokenize "Price: $29.99 (was $39.99) - 25% off!"))))

(deftest test-expressions-with-numbers-in-property-names
  (is (= [{:type :opening-bracket}
          {:type  :expression
           :value [:user
                   :address1
                   :street]}
          {:line 1
           :type :closing-bracket}]
         (lexer/tokenize "{{user.address1.street}}"))))