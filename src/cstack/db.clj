(ns cstack.db)

; (def db {:customer {:cols [] :types [] :rows [][]}
;          :tbls2 {:cols [] :types [] :rows [][]}}})
(def db (atom {}))

(defn insert-into
  "adds a new record"
  [statement]
  (println statement)
  (if (get @db (statement :table))
    (swap! db update-in [(->> statement :table) :rows] 
           conj (zipmap ((get @db (statement :table)) :cols) 
                        (statement :values)))
      (println "Table does not exist")))

(defn create-table
  ; TODO add column type validation, for now a table is a vector
  [statement]
  (let [dtypes (->> statement :cols (map :datatype) (mapv keyword))
        cols (->> statement :cols (map :name) (mapv keyword))]
  (swap! db assoc (statement :name) {:cols cols :types dtypes :rows []})))

(defn select
  [statement]
  (->> (get @db (statement :from))
       :rows
       (map #(select-keys % (statement :item)))))

(comment
  
  @db
  (do
  (reset! db {})
  (create-table {:name :customer,
                 :cols
                 [{:name "id", :datatype "int"}
                  {:name "name", :datatype "text"}
                 {:name "email", :datatype "text"}]})
  (create-table {:name :t
                 :cols
                 [{:name "i", :datatype "int"}
                  {:name "a", :datatype "text"}
                 ]})
  @db
  (insert-into {:table :customer, :values [1 "'user1'" "'user1@clj.org'"]})  
  (insert-into {:table :customer, :values [2 "'user2'" "'user2@clj.org'"]})  
  (insert-into {:table :customer, :values [3 "'user3'" "'user3@clj.org'"]})  
  (insert-into {:table :t :values [1 "'3'"]})
  @db
  ))
