(ns cstack.db
  (:require [clojure.pprint :as p]))

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
  (let [cols (if (= (:item statement) [:*])
               (get-in @db [(:from statement) :cols])
               (:item statement))
        out (->> (get @db (:from statement))
                 :rows
                 (mapv #(select-keys % cols)))]
    (if out
      (p/print-table out)
      (println "Error"))
    out))

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
  (select {:from :customer :item [:*]}))

(comment [])
  (->> [{:i 1 :name "ben"} {:i 2 :name "candice"}]
       (reduce (partial merge-with vector)))

  (->> (get @db :t)
       :rows
       (mapv #(select-keys % [:id]))
       (reduce (partial merge-with vector)))
(->> [{:id 1, :name "'user1'", :email "'user1@clj.org'"} {:id 2, :name "'user2'", :email "'user2@clj.org'"} {:id 3, :name "'user3'", :email "'user3@clj.org'"} {:id 4, :name "'user4'", :email "'user4@clj.org'"} {:id 5, :name "'user5'", :email "'user5@clj.org'"}]
     (filter (fn [x] (and (> (:id x) 2 )
                          (<= (:id x) 4)))))

{:from :t, 
 :where 
 {:kind :binary, 
  :expr {:op {:token "=", :type :symbol}, 
         :a {:token "id", :type :identifier}, 
         :b {:token "1", :type :number}}}, 
 :item [:*]}

{:from :t, 
 :where 
 '({:token "=", :type :symbol} {:token "id", :type :identifier} {:token "1", :type :number}), 
 :item [:*]}

(def wh {:from :t, :where 
  '({:token ">", :type :symbol} {:token "id", :type :identifier} {:token "1", :type :number}) 
 :item [:*]})

(def sexp 
  '({:token "and", :type :symbol} 
    ({:token ">", :type :symbol} {:token "id", :type :identifier} {:token "1", :type :number}) 
    ({:token "<", :type :symbol} {:token "id", :type :identifier} {:token "5", :type :number})))

(map :token sexp)
'(and (> id 1) (< id 5))

(->> wh :where 
     (map :token))

; => (filter #(= :id 1) coll)

(def op {'and 'or
         '= '=
         '<> 'not=
         '<  '<
         '> '>
         '<= '<= '>= '>=
         '|| str '+ '+})
((op '||) "first " "name")

