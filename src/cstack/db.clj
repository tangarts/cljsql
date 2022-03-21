(ns cstack.db)

(defonce db (atom {}))

(defn insert-into
  "adds a new record"
  [table-name record]
  (if (@db table-name) (swap! db update-in [table-name] conj record)
      (throw (Exception. "Table does not exist"))))

(defn create-table
  [table-name]
  (swap! db assoc (keyword table-name) []))

(comment
  (@db :h)

  (reset! db {})
  (->> (@db :songs)
       (map #(select-keys % [:title :artist])))
  @db
  (create-table "songs")

  (do
    (insert-into :songs {:title "Roses" :artist "Kathy Mattea" :rating 7})
    (insert-into :songs {:title "Fly" :artist "Dixie Chicks" :rating 8})
    (insert-into :songs {:title "Home" :artist "Dixie Chicks" :rating 9})
    (insert-into :songs {:title "Home" :artist "Dixie Chicks" :rating 9})
    @db
    (insert-into :hus {:title "Home" :artist "Dixie Chicks" :rating 9})))

(defprotocol Cell
  (as-text [p])
  (as-int [p]))

(defn results []
  {:cols {:type nil :name nil}
   :rows nil})

(defprotocol Backend
  (create-table [statement])
  (insert [statement])
  (select [statement]))


(defrecord Row [^Integer id ^String username ^String email])

(defn insert-row
  [expr]
  (apply ->Row
         (mapv
          (fn [v] (convert (v :token) (v :type)))
          (filter #(not= (-> % :type) :symbol) expr))))
