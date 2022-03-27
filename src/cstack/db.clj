(ns cstack.db
  (:require [clojure.pprint :as p]))

; (def db {:customer {:cols [] :types [] :rows [][]}
;          :tbls2 {:cols [] :types [] :rows [][]}}})

(defn- logical-or [a b] (or a b))
(defn- logical-and [a b] (and a b))

(def oper {'or logical-or
           'and logical-and
           :or logical-or
           :and logical-and
           '|| str
           :|| str})

(defonce db (atom {}))

(defn insert-into
  "Adds a new record"
  [statement]
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

(defn condfn
  ([t] t)

  ([t k1 op1 k2 link & other]
   ((oper link link)
    (apply condfn t other)
    (condfn t k1 op1 k2)))

  ([t k1 op k2]
   (let [v1 (if (keyword? k1) (k1 t) k1)
         v2 (if (keyword? k2) (k2 t) k2)]
     ((resolve op) v1 v2)))

  ([t exprs]
   (apply condfn t exprs)))

(defn select
  [statement]
  (let [cols (if (= (:item statement) [:*])
               (get-in @db [(:from statement) :cols])
               (:item statement))

        out (->> (get @db (:from statement))
                 :rows
                 (filter (fn [t] (condfn t (:where statement))))
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

; ;;;;;;;;;;;;;;;;;;;;;;; SCRATCH ;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (do
    @db
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

    (insert-into {:table :customer, :values [1 "'user1'" "'user1@clj.org'"]})
    (insert-into {:table :customer, :values [2 "'user2'" "'user2@clj.org'"]})
    (insert-into {:table :customer, :values [3 "'user3'" "'user3@clj.org'"]})
    (insert-into {:table :customer, :values [4 "'user4'" "'user4@clj.org'"]})
    (insert-into {:table :customer, :values [5 "'user5'" "'user5@clj.org'"]})
    (insert-into {:table :t :values [1 "'3'"]})
    (insert-into {:table :t :values [2 "'Jerry'"]})
    (insert-into {:table :t :values [3 "'Ben'"]})
    (insert-into {:table :t :values [4 "'Sandra'"]})
    (insert-into {:table :a :values [1]})
    ;(select {:from :t, :where '[:id >= 3 and :id < 4], :item [:*]})
    )

  comment)
