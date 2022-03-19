(ns cstack.repl
  (:require [clojure.string :as str]
            [cstack.lexer :refer [lex]]))

(def db (atom []))

(defn read-input [input]
  (str/split input #"\s+|\n|\t|\n|;"))

(defn start-message []
  (println "SQLite clone v.0.0.1")
  (println "Enter \".help\" for usage hints")
  (println "Connected to a transient in-memory database")
  (println "Use \".open FILENAME\" to re-open a persistent database."))

(defrecord Row [^Integer id ^String username ^String email])

(defn convert [value type]
  (case type
    :number (Integer/parseInt value)
    value))

(defn insert-row
  [expr]
  (apply ->Row
         (mapv
          (fn [v] (convert (v :token) (v :type)))
          (filter #(not= (-> % :type) :symbol) expr))))

(defn expect-token
  [token expected]
  (= (token :token) expected))

(comment

  (def v (lex "insert into db values (1, 'user', 'user@clj.org')"))
  (insert-row (subvec v 4))
  (def -vs (subvec v 4))

  (def sql-str1 (lex   "select id, username from places where locality=\"los angeles\";"))
  (second (split-with #(not= (-> % :token) "values") v))

  (split-with #(not= (-> % :token) "from") (rest sql-str1))
  (-> sql-str1 first :token)

  (apply ->Row
         (mapv
          (fn [v] (convert (v :token) (v :type)))
          (filter #(not= (-> % :type) :symbol) -vs))))

(defn insert-fn
  "
    INSERT
    INTO
    $table-name
    VALUES ( $expression [, ...])
  "
  [input]
  (if (expect-token (input 1) "into")
    (if (expect-token (input 2) "db") ; table-name
      (if (expect-token (input 3) "values")
        (insert-row (subvec input 4))
        (prn "Expected Values"))
      (prn "Table doesn't exist"))
    (prn "Expected INTO statement")))

(insert-fn
 (lex "insert into db values (1, user, user@clj.org)"))

(defn select-fn
  "
    SELECT
    $expression [, ...]
    FROM
    $table-name 
  "
  [input]
  (let [[select-expr from-expr]
        (split-with #(not= (-> % :token) "from") (rest input))]
    {:table (second from-expr)
     :expression (vec select-expr)}))

(select-fn (lex "select id, username from customer"))

(defn create-table [input]
  (partition 2 (filter #(not= (-> % :type) :symbol) input)))
(defn create-fn
  "
    CREATE
    $table-name
    (
    [$column-name $column-type [, ...]]
    ) 
  "
  [input]
  (if (expect-token (input 1) "table") 
    (if (expect-token (input 2) "customer") ; table-name
      (create-table (subvec input 3))
      (prn "Expected Values"))
    (prn "Expected table name: ")))

(create-fn (lex "create table customer (id int, name text, email text);"))

(def command
  {:select select-fn
   :insert insert-fn
   :create create-fn})

(defn -main []
  (start-message)
  (loop [input (lex (read-line))]
    (print "sqlite> ")
    (let [statement (first input)]
      (if (or (= statement ".exit") (= statement ".quit")) (print "bye!")
          (do
            (condp contains? (keyword statement)
              command (println (((keyword statement) command) input))
              (println "Unrecognized command " input))
            (recur (lex (read-line))))))))

((command :select) 1)

(comment
; (def tokens
;   (read-string (str \[ "select * from customer where id = 1" \])))
; 
; (read-input "select *  from customer where id = 1;")
; (read-input ".quit")
  )

(-main)
