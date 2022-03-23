(ns cstack.repl
  (:require [cstack.lexer :refer [lex]]
            [cstack.db :refer [execute]]
            [cstack.parser :refer [parse-statement]]))

(defn start-message []
  (println "SQLite clone v.0.0.1")
  (println "Enter \".help\" for usage hints")
  (println "Connected to a transient in-memory database")
  (println "Use \".open FILENAME\" to re-open a persistent database."))

(defn -main []
  (start-message)
  (loop []
    (print "sqlite> ")
    (flush)
    (let [tokens (lex (read-line))]
      (if (= (-> tokens first :token) ".")
        (if (or (= "exit" (-> tokens second :token))
                (= "quit" (-> tokens second :token))) (println "bye!")
            (do
              (println "Unrecognized command" (-> tokens second :token))
              (recur)))
        (do
          (execute (parse-statement tokens))
          (recur))))))

(-main)
