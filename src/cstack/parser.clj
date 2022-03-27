(ns cstack.parser)

(defn convert [{type :type token :token}]
  (case type
    :identifier (keyword token)
    :keyword (symbol token)
    :symbol (symbol token)
    :number (try
              (Integer/parseInt token)
              (catch Exception _ nil))
    token))

(def op {"and"  1 "or"  1
         "="    2 "<>"  2
         "<"    3 ">"   3
         "<="   4 ">="  4
         "||"   5 "+"   5})

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

(defn parse-exprs
  [exprs delim]
  (if (sequential? exprs)
    (loop [[x c & xs] exprs
           acc [x]]
      (if  (nil? x) (println "Expected " delim)
           (let [[nx & _] xs]
             (cond
               (= delim (:token c)) acc

               (= "," (:token c))
               (recur xs (conj acc nx))

               :else (println "expected comma")))))
    exprs))

(def column-type #{"integer" "int" "numeric" "num" "text"})

(defn column-def
  ; Doesn't support constraints only [column name, type name ..]
  [exprs delim]
  (if (sequential? exprs)
    (loop [[cn tn c & xs] exprs
           acc [cn tn]]
      (if (nil? cn) (println "Expected " delim)
          (let [[cnn tnn & _] xs]
            (cond
              (nil? (column-type (:token tn)))
              (println "Unrecognized column type, must be one of " column-type)

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
           :values (mapv convert (parse-exprs (subvec tokens 5) ")"))}
          (println "Expected left parenthesis"))
        (println "Expected values keyword"))
      (println "Expected table names"))
    (println "Expected into")))

(defn parse-select
  " SELECT $expression [, ...] FROM $table-name "
  [tokens]
  (let [tokens
        (take-while #(not= (-> % :token) ";") tokens)

        [select-expr from-expr]
        (split-with #(not= (-> % :token) "from") (rest tokens))

        where-expr (drop-while #(not= (-> % :token) "where") from-expr)
        from (-> from-expr second :token keyword)]
    (if (not= '() from-expr)
      {:from from
       :where (mapv convert (rest where-expr))
       :item (mapv #(-> % :token keyword)
                   (parse-exprs select-expr nil))}
      {:from from
       :item (mapv convert select-expr)})))

(defn parse-create
  " CREATE $table-name ( [$column-name $column-type [, ...]] ) "
  [input]
  (if (= (get-in input [1 :token]) "table")
    (if (=  (get-in input [2 :type])  :identifier) ; table-name
      (if (= (get-in input [3 :token]) "(")
        {:name (-> (input 2) :token keyword)
         :cols
         (mapv (fn [[t ts] & _] {:name t :datatype ts})
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

;;;;;;;;;;;;;;;;; SCRATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

  (def tkn [{:token "select", :type :keyword}
            {:token "id", :type :identifier}
            {:token ",", :type :symbol}
            {:token "name", :type :identifier}
            {:token "from", :type :string}
            {:token "t", :type :symbol}
            {:token ";", :type :symbol}] ":")
  comment)
