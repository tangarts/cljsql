(ns cstack.repl
  (:require [cstack.lexer :refer [lex]]
            [cstack.db :refer [execute db]]
            [cstack.parser :refer [parse-statement]]))

(defn start-message []
  (println "SQLite clone v.0.0.1")
  (println "Enter \".help\" for usage hints")
  (println "Connected to a transient in-memory database")
  (println "Use \".open FILENAME\" to re-open a persistent database."))

(defn print-help []
  (println ".exit\t\t Exit this program")
 ; (println ".open ?FILE?\t\t Close existing database and reopen FILE")
  (println ".quit\t\t Exit this program")
 ; (println ".read FILE\t\t Read input from FILE")
 ; (println ".save FILE\t\t Write in-memory database into FILE")
  (println ".tables\t\t List available tables"))

(defn execute-meta [token]
  (case token
    "tables" (apply println (map name (keys @db)))
    "help" (print-help)
    "save" (println "To implement: .save")
    (println "Unrecognized command" token)))

(defn -main []
  (start-message)
  (loop []
    (print "cljsql> ")
    (flush)
    (let [tokens (lex (read-line))]
      (if (= (-> tokens first :token) ".")
        (if (#{"exit" "quit"} (-> tokens second :token)) (print "bye!")
            (do
              (execute-meta (-> tokens second :token))
              (recur)))
        (do
          (execute (parse-statement tokens))
          (recur))))))

(-main)
