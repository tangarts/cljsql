(ns cstack.db
  (:require [clojure.pprint :as p]))

; (def db {:customer {:cols [] :types [] :rows [][]}
;          :tbls2 {:cols [] :types [] :rows [][]}}})

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

(defmacro condition
  ; http://blog.alex-turok.com/2014/05/how-to-make-sql-from-clojure-or-my.html
  ([_]
   `true)
  ([t complex]
   `(condition ~t ~@complex))
  ([t k1 op k2]
   (if (seq? k1)
     `(~op
       (condition ~t ~k1)
       (condition ~t ~k2))
     (let [v1 (if (keyword? k1) `(~k1 ~t) k1)
           v2 (if (keyword? k2) `(~k2 ~t) k2)]
       `(~op ~v1 ~v2))))
  ([t k1 op1 k2 link & other]
   (if (seq? k1)
     `(~op1
       (condition ~t ~k1)
       (condition ~t ~k2 ~link ~@other))
     `(~link
       (condition ~t ~k1 ~op1 ~k2)
       (condition ~t ~@other)))))

(defmacro condm
  [& conditions]
  `(fn [t#] (condition t# ~@conditions)))

(defn condfn
  ([t] t)

  ([t k1 op k2]
   (let [v1 (if (keyword? k1) (k1 t) k1)
         v2 (if (keyword? k2) (k2 t) k2)]
     ((resolve op) v1 v2)))
  ([t k1 op1 k2 link & other]
   (println "t: " t)
   (println "k1: " k1)
   (println "op1: " op1)
   (println "k2: " k2)
   (println "link: "  link)
   (println "other: " other)

   (link
    (condfn t k1 op1 k2)
    (apply condfn t other))
   ))

(def table [{:a 1 :b 100 :c "100" :d 4}
            {:a 2 :b 200 :c "200" :d 3}
            {:a 3 :b 300 :c "300" :d 2}
            {:a 4 :b 400 :c "400" :d 1}])

(comment
  (->> table
       (filter 
           (fn [t] (condition t [:a '> 2 'and :b '<= 300]))
           )))

(defn select
  [statement]
  (let [cols (if (= (:item statement) [:*])
               (get-in @db [(:from statement) :cols])
               (:item statement))

        out (->> (get @db (:from statement))
                 :rows
                 ; where
                 ; (filter #(op (-> % expr2) expr1)
                 (filter (fn [t] (apply condfn t (:where statement))))
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

  (get-in @db [:t :rows])

  (insert-into {:table :customer, :values [1 "'user1'" "'user1@clj.org'"]})
  (insert-into {:table :customer, :values [2 "'user2'" "'user2@clj.org'"]})
  (insert-into {:table :customer, :values [3 "'user3'" "'user3@clj.org'"]})
  (insert-into {:table :customer, :values [4 "'user4'" "'user4@clj.org'"]})
  (insert-into {:table :customer, :values [5 "'user5'" "'user5@clj.org'"]})
  (insert-into {:table :t :values [1 "'3'"]})
  (insert-into {:table :t :values [2 "'name'"]})
  (insert-into {:table :t :values [3 "'name'"]})
  (insert-into {:table :t :values [4 "'name'"]})
  (insert-into {:table :a :values [1]})
  (select '{:from :t, :where [:id >= 3 and :id < 5], :item [:*]}))


