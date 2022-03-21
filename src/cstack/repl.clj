(ns cstack.repl
  (:require [cstack.lexer :refer [lex]]
            [cstack.db :refer [insert-into create-table]]))

(defn start-message []
  (println "SQLite clone v.0.0.1")
  (println "Enter \".help\" for usage hints")
  (println "Connected to a transient in-memory database")
  (println "Use \".open FILENAME\" to re-open a persistent database."))

(defn convert [value type]
  (case type
    :number (try
              (Integer/parseInt value)
              (catch Exception _ nil))
    value))

(defn expect-token
  [token expected]
  (= (token :token) expected))

(defn parse-insert
  "
    INSERT
    INTO
    $table-name
    VALUES ( $expression [, ...])
  "
  [input]
  (if (expect-token (input 1) "into")
    (if (= ((input 2) :type) :identifier) ; table-name
      (if (expect-token (input 3) "values")
        {:table (-> (input 2) :token keyword)
         :values
         (mapv
          (fn [v] (convert (v :token) (v :type)))
          (filter #(not= (-> % :type) :symbol)
                  (take-while #(not= (-> % :token) ")")
                              (rest
                               (drop-while #(not= (-> % :token) "(")
                                           input)))))}
        (prn "Expected Values"))
      (prn "Table doesn't exist"))
    (prn "Expected INTO statement")))

(parse-insert (lex "insert into db values (1, 'user', 'user@clj.org');"))

(defn parse-select
  "
    SELECT
    $expression [, ...]
    FROM
    $table-name 
  "
  [input]
  ; "TODO: validation"
  (let [[select-expr from-expr]
        (split-with #(not= (-> % :token) "from") (rest input))]
    {:table (-> from-expr second :token keyword)
     :expression (mapv :token
                       (filter #(not= (-> % :token) ",") select-expr))}))

(parse-select (lex "select id, *; username from customer"))

(defn coldef [token]
  {:name (first token)
   :datatype (second token)})

(defn parse-create
  "
    CREATE
    $table-name
    (
    [$column-name $column-type [, ...]]
    ) 
  "
  [input]
  ; "TODO: Validation"
  (if (expect-token (input 1) "table")
    (if (=  (-> input (get 2) :type)  :identifier) ; table-name
      {:name (-> (input 2) :token keyword)
       :cols
       (mapv (fn [t]
               {:name (first t) :datatype (second t)})
             (partition 2
                        (mapv :token
                              (filter #(not= (-> % :type) :symbol) (subvec input 3)))))}
      (prn "Expected table name"))
    (prn "Syntax error")))

(parse-create
 (lex "create table customer (id int, name text, email text);"))

(defn meta? [tokens]
  (= (-> tokens first :token) "."))

(defn execute-meta [meta-cmd]
  (print meta-cmd)
  (case meta-cmd
    "exit" (print "bye!")
    "quit" (print "bye!")
    (print "unrecognised command")))

(defn -main []
  (start-message)
  (loop [input (lex (read-line))]
    (print "sqlite> ")
    (let [statement (first input)]
      (if (meta? input)
        (cond
          (nil? (second input)) (do
                                  (println "Unrecognized command " input)
                                  (recur (lex (read-line))))
          (= "exit" (-> input second :token)) (println "bye!")
          :else (do
                  (println "Unrecognized command " input)
                  (recur (lex (read-line)))))

        (do
          (case (statement :token)
            "select" (println (parse-select input))
            "insert" (println (parse-insert input))
            "create" (println (parse-create input))
            (println "Unrecognized command " input))
          (recur (lex (read-line))))))))

(-main)

(comment

  (def v (lex "insert into * db values (1.03, 'user', 'user@clj.org');"))

  (def query (lex "create table customer (id int, name text, email text);"))

  (def sql-str1 (lex   "select id, username from places where locality=\"los angeles\";"))

  (second (split-with #(not= (-> % :token) "values") v))
  (split-with #(not= (-> % :token) "from") (rest sql-str1))
  (-> sql-str1 first :token))
