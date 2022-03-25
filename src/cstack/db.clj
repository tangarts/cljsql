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
  ; (let [out]
  ;   (if out
  ;     (reduce (merge-with list*) out)
  ;     (println "Error")))
  (->> (get @db (:from statement))
       :rows
       (mapv #(select-keys % (:item statement)))
       ))


(defn execute [statement]
  (when (:statement statement)
    (case (:kind statement)
      :select (select (:statement statement))
      :insert (insert-into (:statement statement))
      :create (create-table (:statement statement))
      (println "Unrecognized statement" (name (:kind statement))))))


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
    (select {:from :customer :item [:id :name]}))

(comment
  (->> [{:i 1 :name "ben"} {:i 2 :name "candice"}]
       (reduce (partial merge-with vector)))

  (->> (get @db :t)
       :rows
       (mapv #(select-keys % [:id]))
       (reduce (partial merge-with vector)))

  [])

