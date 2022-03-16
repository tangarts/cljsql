(ns cstack)

(defn start-message []
  (println "SQLite clone v.0.0.1")
  (println "Enter \".help\" for usage hints")
  (println "Connected to a transient in-memory database")
  (println "Use \".open FILENAME\" to re-open a persistent database."))

(defn -main []
  (start-message)
  (loop [input (read-line)]
    (print "sqlite> ")
    (cond
      (or (= input ".exit")
          (= input ".quit")) (print "bye!")
      :else (do
              (println input)
              (recur (read-line))))))


