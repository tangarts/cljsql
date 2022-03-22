(ns cstack.repl
  (:require [cstack.lexer :refer [lex]]
            [cstack.db :refer [insert-into create-table select]]
            [cstack.parser :refer [parse-insert parse-create parse-select]]))

(defn start-message []
  (println "SQLite clone v.0.0.1")
  (println "Enter \".help\" for usage hints")
  (println "Connected to a transient in-memory database")
  (println "Use \".open FILENAME\" to re-open a persistent database."))


(defn -main []
  (start-message)
  (loop [tokens (lex (read-line))]
    (print "sqlite> ")
    (let [statement (first tokens)]
      (if ((-> tokens first :token) ".")
        (if (or (= "exit" (-> tokens second :token))
                (= "quit" (-> tokens second :token))) (println "bye!")
           (do
                  (println "Unrecognized command" (-> tokens second :token))
                  (recur (lex (read-line)))))

        (do
          (case (get statement :token)
            "select" (println (select (parse-select tokens)))
            "insert" (println (insert-into (parse-insert tokens)))
            "create" (println (create-table (parse-create tokens)))
            (println "Unrecognized command " tokens))
          (recur (lex (read-line))))))))

(-main)



