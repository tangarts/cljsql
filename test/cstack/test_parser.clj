(ns cstack.test-parser
  (:require [clojure.test :refer :all]
            [cstack.parser :refer :all]))

(deftest test-parse-select
  (are [x y] (= x y)
    (parse-select [{:token "select", :type :keyword}
                   {:token "id", :type :identifier}
                   {:token "from", :type :keyword}
                   {:token "t", :type :identifier}])
    {:from :t, :item [:id]}

    (parse-select [{:token "select", :type :keyword}
                   {:token "id", :type :identifier}
                   {:token ",", :type :symbol}
                   {:token "username", :type :identifier}
                   {:token "from", :type :keyword}
                   {:token "customer", :type :identifier}])
    {:from :customer, :item [:id :username]}))

(deftest test-parse-create
  (are [x y] (= x y)
    (parse-create
     [{:token "create", :type :keyword}
      {:token "table", :type :keyword}
      {:token "customer", :type :identifier}
      {:token "(", :type :symbol}
      {:token "id", :type :identifier}
      {:token ",", :type :symbol}
      {:token "int", :type :keyword}
      {:token ")", :type :symbol}
      {:token ";", :type :symbol}])
    {:name :customer, :cols [{:name "id", :datatype "int"}]}))

(deftest test-parse-binary
  (is (= (parse-binary '(1)) 1))
  (is (= (parse-binary '(1 + 2)) ; fail
         {:kind :binary, :expr {:a 1, :op '+, :b 2}})
      (is (= (parse-binary '(2 = 3 and 4 = 5))
             {:kind :binary,
              :expr
              {:a {:kind :binary,
                   :expr {:a 2, :op '=, :b 3}},
               :op 'and,
               :b {:kind :binary,
                   :expr {:a 4, :op '=, :b 5}}}})))) ; fail

(run-tests 'cstack.test-parser)

(comment
  (parse-exprs [{:token "id", :type :identifier}
                {:token "int", :type :keyword}
                {:token ",", :type :symbol}
                {:token "name", :type :identifier}
                {:token "text", :type :keyword}
                {:token ")", :type :symbol}
                {:token ";", :type :symbol}] ")")
  (parse-exprs [{:token "id", :type :identifier}
                {:token ",", :type :symbol}
                {:token "name", :type :identifier}
                {:token ")", :type :symbol}
                {:token ";", :type :symbol}] ")")
  [])

