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

(def -vs (subvec v 4))

(apply ->Row
       (mapv
        (fn [v] (convert (v :token) (v :type)))
        (filter #(not= (-> % :type) :symbol) -vs)))

(def v (lex "insert into db values (1, 'user', 'user@clj.org')"))
(insert-row (subvec v 4))

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

(defn select-fn [input]
  input
  "This is where we would do a create")

(defn create-fn [input]
  input
  "This is where we would do a create")

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
