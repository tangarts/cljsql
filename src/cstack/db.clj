(ns cstack.db)

; (def db {:customer {:cols [] :types [] :rows [][]}
;          :tbls2 {:cols [] :types [] :rows [][]}}})
(defonce db (atom {}))

(defn insert-into
  "adds a new record"
  [statement]
  (if (@db (statement :table))
    (swap! db update-in [(->> statement :table) :rows] 
           conj (zipmap ((get @db (statement :table)) :cols) 
                        (statement :values)))
      (throw (Exception. "Table does not exist"))))

(defn create-table
  ; TODO add column type validation, for now a table is a vector
  [statement]
  (let [dtypes (->> statement :cols (map :datatype) (mapv keyword))
        cols (->> statement :cols (map :name) (mapv keyword))]
  (swap! db assoc (statement :name) {:cols cols :types dtypes :rows []})))

(defn select
  [statement]
  (->> (get @db (statement :table))
       :rows
       (map #(select-keys % (statement :expression)))))

(comment
  
  @db
  (do
  (reset! db {})
  (create-table {:name :customer,
                 :cols
                 [{:name "id", :datatype "int"}
                  {:name "name", :datatype "text"}
                 {:name "email", :datatype "text"}]})
  @db
  (insert-into {:table :customer, :values [1 "'user1'" "'user1@clj.org'"]})  
  (insert-into {:table :customer, :values [2 "'user2'" "'user2@clj.org'"]})  
  (insert-into {:table :customer, :values [3 "'user3'" "'user3@clj.org'"]})  
  @db
  ))
