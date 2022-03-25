(ns cstack.db)

; (def db {:customer {:cols [] :types [] :rows [][]}
;          :tbls2 {:cols [] :types [] :rows [][]}}})
(def db (atom {}))

(defn insert-into
  "Adds a new record"
  [statement]
  (println statement)
  (if (get @db (:table statement))
    (swap! db update-in [(:table statement) :rows]
           conj (zipmap ((get @db (:table statement)) :cols)
                        (:values statement)))
    (println "Table does not exist")))

(defn create-table
  ; TODO add column type validation, for now a table is a vector
  [statement]
  (let [dtypes (->> statement :cols (map :datatype) (mapv keyword))
        cols (->> statement :cols (map :name) (mapv keyword))]
    (swap! db assoc (:name statement) {:cols cols :types dtypes :rows []})))

(defn select
  [statement]
  (let [out (->> (get @db (:from statement))
       :rows
       
       )]
    (if out
      (prn out)
      (println "Error"))))

(defn execute [statement]
  (when (:statement statement)
    (case (:kind statement)
      :select (select (:statement statement))
      :insert (insert-into (:statement statement))
      :create (create-table (:statement statement))
      (println "Unrecognized statement" (name (:kind statement))))))

(comment

  ; (map name (keys @db))
  (do
    (reset! db {})
    (create-table {:name :customer,
                   :cols
                   [{:name "id", :datatype "int"}
                    {:name "name", :datatype "text"}
                    {:name "email", :datatype "text"}]})
    (create-table {:name :t
                   :cols
                   [{:name "id", :datatype "int"}
                    {:name "name", :datatype "text"}]})
    (create-table {:name :a
                   :cols
                   [{:name "id", :datatype "int"}]})
    @db
    (insert-into {:table :customer, :values [1 "'user1'" "'user1@clj.org'"]})
    (insert-into {:table :customer, :values [2 "'user2'" "'user2@clj.org'"]})
    (insert-into {:table :customer, :values [3 "'user3'" "'user3@clj.org'"]})
    (insert-into {:table :customer, :values [4 "'user4'" "'user4@clj.org'"]})
    (insert-into {:table :customer, :values [5 "'user5'" "'user5@clj.org'"]})
    (insert-into {:table :t :values [1 "'3'"]})
    (insert-into {:table :t :values [2 "'name'"]})
    (insert-into {:table :a :values [1]})
    (select {:from :customer :item [:id]}))

  (def res [{:id 1, :name "'user1'", :email "'user1@clj.org'"} {:id 2, :name "'user2'", :email "'user2@clj.org'"} {:id 3, :name "'user3'", :email "'user3@clj.org'"} {:id 4, :name "'user4'", :email "'user4@clj.org'"} {:id 5, :name "'user5'", :email "'user5@clj.org'"}])
  (apply merge (mapv (fn [k]
                          {k (mapv k res)}) [:id :name]))

 (->> '({:i 1} {:i 2})
      (partition-by :i)
     (map (partial apply merge)))

       (reduce (partial merge-with +) '({:i 1} {:i 2}))

(cons 1 '())

  [])

