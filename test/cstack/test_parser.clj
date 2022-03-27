(ns cstack.test-parser
  (:require [clojure.test :refer :all]
            [cstack.parser :refer :all]))

(deftest test-parse-binary
  (is (= (parse-binary '(1)) 1))
  (is (= (parse-binary '(1 + 2))
         '(+ 1 2)))
  (is (= (parse-binary '(2 = 3 and 4 = 5))
         '(and (= 2 3) (= 4 5)))))

(deftest test-parse-exprs
  (is (= 1 1))
  (is (=  (parse-exprs [{:token "id", :type :identifier}
                        {:token ",", :type :symbol}
                        {:token "name", :type :identifier}
                        {:token ",", :type :symbol}
                        {:token "lname", :type :identifier}
                        {:token ")", :type :symbol}] ")")
          [{:token "id", :type :identifier}
           {:token "name", :type :identifier}
           {:token "lname", :type :identifier}]))

  (is (= (parse-exprs [{:token "id", :type :identifier}
                       {:token ",", :type :symbol}
                       {:token "name", :type :identifier}
                       {:token "text", :type :keyword}
                       {:token ")", :type :symbol}
                       {:token ";", :type :symbol}] ")")
         nil)))

(deftest test-parse-column-def
  (is (= (column-def [{:token "id", :type :identifier}
                      {:token "int", :type :keyword}
                      {:token ",", :type :symbol}
                      {:token "age", :type :identifier}
                      {:token "int", :type :keyword}
                      {:token ")", :type :symbol}
                      {:token ";", :type :symbol}] ")")
         [{:token "id", :type :identifier}
          {:token "int", :type :keyword}
          {:token "age", :type :identifier}
          {:token "int", :type :keyword}])))

(deftest test-parse-insert
  (are [x y] (= x y)
    (parse-insert [{:token "insert", :type :keyword}
                   {:token "into", :type :keyword}
                   {:token "db", :type :identifier}
                   {:token "values", :type :keyword}
                   {:token "(", :type :symbol}
                   {:token "1", :type :number}
                   {:token ",", :type :symbol}
                   {:token "'user'", :type :string}
                   {:token ")", :type :symbol}
                   {:token ";", :type :symbol}])
    {:table :db, :values [1 "'user'"]}))

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

(deftest test-parse-select
  (are [x y] (= x y)
    (parse-select [{:token "select", :type :keyword}
                   {:token "id", :type :identifier}
                   {:token "from", :type :keyword}
                   {:token "t", :type :identifier}])
    {:from :t, :item [:id] :where []}

    (parse-select [{:token "select", :type :keyword}
                   {:token "id", :type :identifier}
                   {:token ",", :type :symbol}
                   {:token "username", :type :identifier}
                   {:token "from", :type :keyword}
                   {:token "customer", :type :identifier}])
    {:from :customer, :item [:id :username] :where []}))

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

  (parse-select [{:token "select", :type :keyword}
                 {:token "*", :type :symbol}
                 {:token "from", :type :string}
                 {:token "t", :type :identifier}
                 {:token "where" :type :keyword}
                 {:token "id", :type :identifier}
                 {:token ">", :type :symbol}
                 {:token "1", :type :number}
                 {:token "and", :type :symbol}
                 {:token "id", :type :identifier}
                 {:token "<", :type :symbol}
                 {:token "5", :type :number}
                 {:token ";", :type :symbol}])

  (parse-select [{:token "select", :type :keyword}
                 {:token "1", :type :number}
                 {:token "+", :type :symbol}
                 {:token "1", :type :number}
                 {:token ";", :type :symbol}])
  (mapv convert nil)
  (->> {:token "<", :type :symbol}
       convert)

  (->>  [{:token "id", :type :identifier}
         {:token ">", :type :symbol}
         {:token "1", :type :number}
         {:token "and", :type :symbol}
         {:token "id", :type :identifier}
         {:token "<", :type :symbol}
         {:token "5", :type :number}
         {:token ";", :type :symbol}]
        (map convert))

  comment)

