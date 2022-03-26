(ns cstack.db
  (:require [clojure.pprint :as p]
            [cstack.parser :refer [convert]]))

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

(defn where-expr [field pred & _]
  (filter (fn [e]
            (-> e field pred))))

(defn where-pred [field operation]
  (fn [x] (operation x field)))

(defmacro mselect [& what]
  (let [fields (set
                (take-while #(not= 'from %) what))
        source (fnext
                (drop-while #(not= 'from %) what))
        conditions (next (drop-while #(not= 'where %) what))]
    `(map (fn [record#]
            (into {} (filter #(~fields (first %)) record#)))
          (filter #(condition % ~@conditions) ~source))))

(defmacro mcondition
  ([t]
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



(defn condition [t op k1 k2]
  (let [v1 (if (keyword? k1) (k1 t) k1)
        v2 (if (keyword? k2) (k2 t) k2)]
    ((resolve op) v1 v2)))

(def table [{:a 1 :b 100 :c "100" :d 4}
            {:a 2 :b 200 :c "200" :d 3}
            {:a 3 :b 300 :c "300" :d 2}
            {:a 4 :b 400 :c "400" :d 1}])

(->> table
     (filter #(condition % '>= :a 3))
     (map #(select-keys % [:b :d])))

(defn select
  [statement]
  (let [cols (if (= (:item statement) [:*])
               (get-in @db [(:from statement) :cols])
               (:item statement))

        where (if-let (:where statement)
                (fn [e]
                  (condtion (:from statement)
                            ~@(:where statement)))
                identity)

        out (->> (get @db (:from statement))
                 :rows
                 ; where
                 ; (filter #(op (-> % expr2) expr1)
                 (filter where)
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

(->> (get @db :customer)
     :rows
              ; where
              ; (filter #(op (-> % expr2) expr1)
              ; (
     (filter (fn [e] (or (= (:name e)  "'user1'")
                         (= (:name e) "'user3'"))))
     (mapv #(select-keys % [:id :name])))

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

(->> [{:id 1, :name "'user1'", :email "'user1@clj.org'"}
      {:id 2, :name "'user2'", :email "'user2@clj.org'"}
      {:id 3, :name "'user3'", :email "'user3@clj.org'"}
      {:id 4, :name "'user4'", :email "'user4@clj.org'"}
      {:id 5, :name "'user5'", :email "'user5@clj.org'"}]

     (filter (fn [x] (and (> (:id x) 2)
                          (<= (:id x) 4)))))

(def wh {:from :t, :where
         '({:token ">", :type :symbol} {:token "id", :type :identifier} {:token "1", :type :number})
         :item [:*]})

(def sexp
  '({:token "and", :type :symbol}
    ({:token ">", :type :symbol} {:token "id", :type :identifier} {:token "1", :type :number})
    ({:token "<", :type :symbol} {:token "id", :type :identifier} {:token "5", :type :number})))

(->> wh :where
     (map (fn [t]
            (convert (-> t :type) (-> t :token)))))

; => (filter #(= :id 1) coll)

(def op {'and 'or
         '= '=
         '<> 'not=
         '<  '<
         '> '>
         '<= '<= '>= '>=
         '|| str '+ '+})
((op '||) "first " "name")

