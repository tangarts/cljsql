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
      {:token "int", :type :keyword}
      {:token ")", :type :symbol}
      {:token ";", :type :symbol}])
    {:name :customer, :cols [{:name "id", :datatype "int"}]}))

(run-tests 'cstack.test-parser)

(comment
  (parse-exprs [{:token "id", :type :identifier}
                {:token "int", :type :keyword}
                {:token ",", :type :symbol}
                {:token "name", :type :identifier}
                {:token "text", :type :keyword}
                {:token ")", :type :symbol}
                {:token ";", :type :symbol}] ")")
  [])

