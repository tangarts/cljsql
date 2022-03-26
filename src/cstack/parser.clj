(ns cstack.parser)

(defn convert [type value]
  ; TODO: parse-expr {:literal token :kind literalKind}
  ; TODO: parse-expr {:fn token :kind fnKind}
  (case type
    :identifier (keyword value)
    :symbol (symbol value)
    :number (try
              (Integer/parseInt value)
              (catch Exception _ nil))
    value))

(->>  [{:token "id", :type :identifier}
                 {:token ">", :type :symbol}
                 {:token "1", :type :number}
                 {:token "and", :type :symbol}
                 {:token "id", :type :identifier}
                 {:token "<", :type :symbol}
                 {:token "5", :type :number}
                 {:token ";", :type :symbol}]
     (map #(convert (-> % :type) (-> % :token)))
     parse-binary
     )


(def op {"and" 1 "or" 1
         "=" 2 "<>" 2
         "<" 3 ">" 3
         "<=" 4 ">=" 4
         "||" 5 "+" 5})

(def ops
  (merge op
         (zipmap (map keyword (keys op)) (vals op))
         (zipmap (map symbol (keys op)) (vals op))))

(defn parse-binary
  "Parse Binary expression"
  [tokens]
  (if (sequential? tokens)
    (loop [[a op & xs] tokens]
      ; f becoms accumulator
      (if (nil? op) a
          (let [[b next-op & _] xs]
            (cond
              (ops (get op :token op))
              (if (and next-op (< (get ops (get op :token op) 0)
                             (get ops (get next-op :token next-op) 0)))
                ;{:kind :binary
                ; :expr {:op s :a f :b (parse-binary r)}}
                (list op a (parse-binary xs))
                (recur (list*; {:kind :binary :expr 
                             ;  {:op s :a f :b t}}
                        (list op a b)
                        (rest xs))))

              :else
              ; (node s f (calc r))
              a))))
    tokens))

(defn parse-exprs [exprs delim]
  (if (sequential? exprs)
    (loop [[x c & xs] exprs
           acc [x]]
      (if  (nil? x) (println "Expected " delim)
           (let [[nx & _] xs]
             (cond
               (= delim (:token c)) acc

               (= "," (:token c))
               (recur xs (conj acc (parse-binary nx)))

               :else (println "expected comma")))))
    exprs))

(defn column-def [exprs delim]
  (if (sequential? exprs)
    (loop [[cn tn c & xs] exprs
           acc [cn tn]]
      (if  (nil? cn) (println "Expected " delim)
           (let [[cnn tnn & _] xs]
             (cond
               (= delim (:token c)) acc
               (= "," (:token c)) (recur xs (conj acc cnn tnn))

               :else (println "expected comma")))))
    exprs))

(defn parse-insert
  " INSERT INTO $table-name VALUES ( $expression [, ...] ) "
  [tokens]
  (if (= (get-in tokens [1 :token]) "into")
    (if (= (get-in tokens [2 :type]) :identifier) ; table-name
      (if (= (get-in tokens [3 :token]) "values")
        (if (= (get-in tokens [4 :token]) "(")
          {:table (-> (tokens 2) :token keyword)
           :values (parse-exprs (subvec tokens 5) ")") ; convert to literal / binary expressions?
          ; (mapv
          ;  (fn [v] (convert (v :token) (v :type)))
          ;  )
           }
          (println "Expected left parenthesis"))
        (println "Expected values keyword"))
      (println "Expected table names"))
    (println "Expected into")))

(parse-insert [{:token "insert", :type :keyword}
               {:token "into", :type :keyword}
               {:token "db", :type :identifier}
               {:token "values", :type :keyword}
               {:token "(", :type :symbol}
               {:token "1", :type :number}
               {:token ",", :type :symbol}
               {:token "'user'", :type :string}
               {:token ")", :type :symbol}
               {:token ";", :type :symbol}])

(defn parse-select
  " SELECT $expression [, ...] FROM $table-name "
  [tokens]
  ; "TODO: validation"
  ; some validation in parse exprs
  (let [[select-expr from-expr]
        (split-with #(not= (-> % :token) "from") (rest tokens))
        where-expr (drop-while #(not= (-> % :token) "where") from-expr)
        from (-> from-expr second :token keyword)]
    (println select-expr)
    (println from-expr)
    (println where-expr)
    (if (not= '() from-expr)
      (if (= '() where-expr)
        {:from from
         :item (mapv #(-> % :token keyword)
                     (filter #(not= (-> % :token) ",")
                             select-expr))}
        {:from from
         :where (parse-binary (rest where-expr))
         :item (mapv #(-> % :token keyword)
                     (filter #(not= (-> % :token) ",")
                             select-expr))})
      {:from from
       :item (parse-binary select-expr)})))

(comment

  (parse-exprs [{:token "select", :type :keyword}
                 {:token "id", :type :identifier}
                 {:token ",", :type :symbol}
                 {:token "name", :type :identifier}
                 {:token "from", :type :string}
                 {:token "t", :type :symbol}
                 {:token ";", :type :symbol}])

  (parse-select [{:token "select", :type :keyword}
                 {:token "*", :type :symbol}
                 {:token "from", :type :string}
                 {:token "t", :type :identifier}
                 {:token "where" :type :keyword}
                 {:token "id", :type :identifier}
                 {:token ">", :type :symbol}
                 {:token "1", :type :number}
                 {:token "and", :type :symbol}
                 {:token "id", :type :identifier}
                 {:token "<", :type :symbol}
                 {:token "5", :type :number}
                 {:token ";", :type :symbol}]))

(defn parse-create
  " CREATE $table-name ( [$column-name $column-type [, ...]] ) "
  [input]
  (if (= (get-in input [1 :token]) "table")
    (if (=  (get-in input [2 :type])  :identifier) ; table-name
      (if (= (get-in input [3 :token]) "(")
        {:name (-> (input 2) :token keyword)
         :cols
         (mapv (fn [[t ts] & _]
                 {:name t :datatype ts})
               (partition 2 (map :token
                                 (column-def (subvec input 4) ")"))))}
        (println "Expected left parenthesis"))
      (println "Expected table name"))
    (println  "Syntax error")))

(defn parse-statement
  [tokens]
  (case (:token (first tokens))
    "select" {:kind :select :statement (parse-select tokens)}
    "insert" {:kind :insert :statement (parse-insert tokens)}
    "create" {:kind :create :statement (parse-create tokens)}
    {:kind (-> tokens first :token keyword) :statement nil}))
