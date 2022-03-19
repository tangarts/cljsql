(ns cstack.repl
  (:require [clojure.string :as str]))

(def db (atom []))

(defn read-input [input]
  (str/split input #"\s+|\n|\t|\n|;"))

(defn start-message []
  (println "SQLite clone v.0.0.1")
  (println "Enter \".help\" for usage hints")
  (println "Connected to a transient in-memory database")
  (println "Use \".open FILENAME\" to re-open a persistent database."))

(defrecord Row [^Integer id ^String username ^String email])
(def one (->Row 1 "user1" "user1@clj.org"))
(-> one :id)

(defn insert-row [values]
  (map->Row
    (zipmap [:id :username :email]
          (rest (str/split
                 (str/join values) #",|\)|\(")))))

(def v ["insert" "into" "db" "values" "(1," "user," "user@clj.org)"])
(insert-row (subvec v 4))

(defn insert-fn
  "
    INSERT
    INTO
    $table-name
    VALUES ( $expression [, ...])
  "
  [input]
  (if (= (input 1) "into")
    (if (= (input 2) "db")
      (if (= (input 3) "values")
        (insert-row (subvec input 4))
        (prn "Expected Values"))
      (prn "Table doesn't exist"))
    (prn "Expected INTO statement")))

(insert-fn
  (read-input "insert into db values (1, user, user@clj.org)"))

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
  (loop [input (read-input (read-line))]
    (print "sqlite> ")
    (let [statement (first input)]
      (if (or (= statement ".exit") (= statement ".quit")) (print "bye!")
          (do
            (condp contains? (keyword statement)
              command (println (((keyword statement) command) input))
              (println "Unrecognized command " input))
            (recur (read-input (read-line))))))))

((command :select) 1)

(comment
; (def tokens
;   (read-string (str \[ "select * from customer where id = 1" \])))
; 
; (read-input "select *  from customer where id = 1;")
; (read-input ".quit")
  )

(-main)
