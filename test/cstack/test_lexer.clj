(ns cstack.test-lexer
  (:require [clojure.test :refer :all]
            [cstack.lexer :refer [lex typer]]))

(deftest lex-number-type
  (are [x] (= ((typer x) :type) :number)
       ; .3 FAILS
    "1" "1.03" "0.03" "100" "1e3" "1E4"))

(deftest lex-keyword-type
  (are [x] (= ((typer x) :type) :string)
    "'user'"
    "\"GBP\""))

(deftest lex-symbol-type
  (are [x] (= ((typer x) :type) :symbol)
    ";" "||" "<>" "<=" "<=" "*" "=" ","))

(deftest lex-identifier-type
  (is (= ((typer "t") :type) :identifier))
  (is (= ((typer "steven") :type) :identifier)))

(deftest full-lexer
  (is (= (lex "INSERT INTO users VALUES ('Stephen', 16);")
         [{:token "insert", :col 0, :type :identifier}
          {:token "into", :col 6, :type :identifier}
          {:token "users", :col 10, :type :identifier}
          {:token "values", :col 15, :type :identifier}
          {:token "(", :col 21, :type :symbol}
          {:token "'stephen'", :col 22, :type :string}
          {:token ",", :col 31, :type :symbol}
          {:token "16", :col 32, :type :number}
          {:token ")", :col 34, :type :symbol}
          {:token ";", :col 35, :type :symbol}])))

(comment
  (def query "create table customer (id int, name text);")
  (def sql-str "select id, username from places where locality=\"los angeles\";")

  (lex "SELECT age + 2, name FROM users WHERE age = 23;")
  (lex "SELECT name FROM users;"))

(run-tests 'cstack.test-lexer)
