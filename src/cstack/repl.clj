(ns cstack.repl
  (:require [cstack.lexer :refer [lex]]
            [cstack.db :refer [insert-into create-table select]]))

(defn start-message []
  (println "SQLite clone v.0.0.1")
  (println "Enter \".help\" for usage hints")
  (println "Connected to a transient in-memory database")
  (println "Use \".open FILENAME\" to re-open a persistent database."))

(defn convert [value type]
  ; TODO: parse-expr {:literal token :kind literalKind}
  ; TODO: parse-expr {:fn token :kind fnKind}
  (case type
    :number (try
              (Integer/parseInt value)
              (catch Exception _ nil))
    value))

(defn expect-token
  [token expected]
  (= (token :token) expected))

(defn parse-exprs
   "Looks for tokens separated by a comma until delimeter is found"
  ^{:post [(every? #(not= :symbol (-> % :type)) %)]}
  [tokens delim]
  (filter #(not= (-> % :token) ",")
          (take-while #(not= (-> % :token) delim)
                      tokens)))

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
          (parse-exprs (rest
                        (drop-while #(not= (-> % :token) "(")
                                    input))
                       ")"))}
        (println "Expected values keyword"))
      (println "Expected table names"))
    (println "Expected into"))
  {})

(parse-insert (lex "insert into db values (1, 'user', 'user@clj.org');"))
(parse-insert (lex "insert into db value (1, 'user', 'user@clj.org');"))

(defn parse-select
  "
    SELECT
    $expression [, ...]
    FROM
    $table-name 
  "
  [input]
  ; "TODO: validation"
  ; some validation in parse exprs
  (let [[_ from-expr]
        (split-with #(not= (-> % :token) "from") (rest input))]
    (if (not= '() from-expr)
      {:table (-> from-expr second :token keyword)
       :expression (mapv #(-> % :token keyword)
                         (parse-exprs (rest input) "from"))}
      (println  "Expected FROM clause"))))

(parse-exprs (lex "select id, username customer") "from")
(parse-select (lex "select id, username from customer"))

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
                              (parse-exprs (subvec input 4) ")"))))}
      (println "Expected table name"))
    (println  "Syntax error")))

(parse-exprs (lex "id int, name text, email text);") ")")
(parse-create
 (lex "create table customer (id int, name text, email tex);"))

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
        (if (or (= "exit" (-> input second :token))
                (= "quit" (-> input second :token))) (println "bye!")
           (do
                  (println "Unrecognized command " input)
                  (recur (lex (read-line)))))

        (do
          (case (get statement :token)
            "select" (println (select (parse-select input)))
            "insert" (println (insert-into (parse-insert input)))
            "create" (println (create-table (parse-create input)))
            (println "Unrecognized command " input))
          (recur (lex (read-line))))))))

(-main)

(comment

  (def v (lex "insert into * db values (1.03, 'user', 'user@clj.org');"))
  (def query (lex "create table customer (id int, name text, email text);"))
  (def sql-str1 (lex   "select id, username from places where locality=\"los angeles\";"))

  (second (split-with #(not= (-> % :token) "values") v))
  (split-with #(not= (-> % :token) "from") (rest sql-str1))
  (-> sql-str1 first :token)
  (parse-exprs (rest
                (drop-while #(not= (-> % :token) "(")
                            (lex "insert into db values (1, 'user', 'user@clj.org');")))
               ")"))

