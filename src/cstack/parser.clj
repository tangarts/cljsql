(ns cstack.parser)

(defn convert [value type]
  ; TODO: parse-expr {:literal token :kind literalKind}
  ; TODO: parse-expr {:fn token :kind fnKind}
  (case type
    :number (try
              (Integer/parseInt value)
              (catch Exception _ nil))
    value))

(def op {"and" 1 "or" 1
         "=" 2 "<>" 2
         "<" 3 ">" 3
         "<=" 4 ">=" 4
         "||" 5 "+" 5})

(def ops
  (merge op
         (zipmap (map keyword (keys op)) (vals op))
         (zipmap (map symbol (keys op)) (vals op))))

(defn expect-token
  [token expected]
  (= (token :token) expected))


(defn parse-binary
  "Parse Binary expression"
  [tokens]
  (if (sequential? tokens)
    (loop [[f s & r] tokens]
      ; f becoms accumulator
      (if (nil? s) f
          (let [[t ft & _] r]
            (cond
              (ops (get s :token s))
              (if (and ft (< (get ops (get s :token s) 0)
                             (get ops (get ft :token ft) 0)))
                ;{:kind :binary
                ; :expr {:op s :a f :b (parse-binary r)}}
                (list s f (parse-binary r))
                (recur (list*; {:kind :binary :expr 
                             ;  {:op s :a f :b t}}
                            (list s f t) 
                              (rest r))))

              :else
              ; (node s f (calc r))
              f))))
    tokens))

(defn parse-exprs [exprs delim]
  (if (sequential? exprs)
    (loop [[x c & xs] exprs
           acc [x]]
    (if  (nil? x) (println "Expected " delim)
     (let [[nx & _] xs]
       (cond 
        (= delim (:token c)) acc 
        (= "," (:token c)) (recur xs (conj acc (parse-binary nx)))

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

(column-def [
      {:token "id", :type :identifier}
      {:token "int", :type :keyword}
      {:token ",", :type :symbol}
      {:token "age", :type :identifier}
      {:token "int", :type :keyword}
      {:token ")", :type :symbol}
      {:token ";", :type :symbol}] ")")

 

(parse-exprs [{:token "id", :type :identifier}
                {:token ",", :type :symbol}
                {:token "name", :type :identifier}
                {:token ",", :type :symbol}
                {:token "lname", :type :identifier}
                {:token ")", :type :symbol}
                ] ")")

(parse-binary '(1))
(parse-binary '(1 = 2 + 3))

(defn parse-insert
  " INSERT INTO $table-name VALUES ( $expression [, ...] ) "
  [tokens]
  (if (expect-token (tokens 1) "into")
    (if (= ((tokens 2) :type) :identifier) ; table-name
      (if (expect-token (tokens 3) "values")
        (if (expect-token (tokens 4) "(")
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

(def tokens [{:token "select", :type :keyword}
             {:token "*", :type :symbol}
             {:token "t", :type :symbol}
             {:token ";", :type :symbol}])

(parse-select tokens)

(comment
  (parse-select [{:token "select", :type :keyword}
                 {:token "1", :type :number}
                 {:token "+", :type :symbol}
                 {:token "1", :type :number}
                 {:token ";", :type :symbol}])

  (parse-select [{:token "select", :type :keyword}
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
  ; "TODO: Validation"
  (if (expect-token (input 1) "table")
    (if (=  (-> input (get 2) :type)  :identifier) ; table-name
      (if (expect-token (input 3) "(")
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
