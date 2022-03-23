(ns cstack.parser)

(defn convert [value type]
  ; TODO: parse-expr {:literal token :kind literalKind}
  ; TODO: parse-expr {:fn token :kind fnKind}
  (case type
    :number (try
              (Integer/parseInt value)
              (catch Exception _ nil))
    value))

(defn expect-token
  [token expected]
  (= (token :token) expected))

(defn parse-exprs
  "Looks for tokens separated by a comma until delimeter is found"
;  ^{:post [(every? #(not= :symbol (-> % :type)) %)]}
  [tokens delim]
  (filter #(not= (-> % :token) ",")
          (take-while #(not= (-> % :token) delim)
                      tokens)))
(defn parse-insert
  "
    INSERT
    INTO
    $table-name
    VALUES ( $expression [, ...])
  "
  [input]
  (if (expect-token (input 1) "into")
    (if (= ((input 2) :type) :identifier) ; table-name
      (if (expect-token (input 3) "values")
        {:table (-> (input 2) :token keyword)
         :values
         (mapv
          (fn [v] (convert (v :token) (v :type)))
          (parse-exprs (rest
                        (drop-while #(not= (-> % :token) "(")
                                    input))
                       ")"))}
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
  "
    SELECT
    $expression [, ...]
    FROM
    $table-name 
  "
  [tokens]
  ; "TODO: validation"
  ; some validation in parse exprs
  (let [[select-expr from-expr]
        (split-with #(not= (-> % :token) "from") (rest tokens))
        where-expr (drop-while #(not= (-> % :token) "where") from-expr)]
    (println select-expr)
    (println from-expr)
    (println where-expr)
    (if (not= '() from-expr)
      (if (= '() where-expr)
        {:from (-> from-expr second :token keyword)
         :item (mapv #(-> % :token keyword)
                     (filter #(not= (-> % :token) ",")
                             select-expr))}
        {:from (-> from-expr second :token keyword)
         :where where-expr
         :item (mapv #(-> % :token keyword)
                     (filter #(not= (-> % :token) ",")
                             select-expr))})

      {:from (-> from-expr second :token keyword)
       :item (mapv #(-> % :token)
                   (filter #(not= (-> % :type) :symbol)
                           select-expr))})))

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
               {:token "id", :type :identifier}
               {:token ",", :type :symbol}
               {:token "name", :type :identifier}
               {:token ",", :type :symbol}
               {:token "from", :type :string}
               {:token "t", :type :symbol}
               {:token "where" :type :keyword}
               {:token "id", :type :identifier}
               {:token "=", :type :symbol}
               {:token "1", :type :number}
               {:token ";", :type :symbol}])

; {:kind :binary
;  :expr {:a {:token "id" :type :identifier}
;         :b {:token "1" :type :number} 
;         :op {:token :type :symbol}}}

(partition 2 (rest '({:token "id", :type :identifier}
                     {:token "=", :type :symbol}
                     {:token "1", :type :number}
                     {:token ";", :type :symbol})))
(defn parse-create
  "
    CREATE
    $table-name
    (
    [$column-name $column-type [, ...]]
    ) 
  "
  [input]
  ; "TODO: Validation"
  (if (expect-token (input 1) "table")
    (if (=  (-> input (get 2) :type)  :identifier) ; table-name
      {:name (-> (input 2) :token keyword)
       :cols
       (mapv (fn [t]
               {:name (first t) :datatype (second t)})
             (partition 2
                        (mapv :token
                              (parse-exprs (subvec input 4) ")"))))}
      (println "Expected table name"))
    (println  "Syntax error")))

(defn parse-statement
  [tokens]
  (case (get (first tokens) :token)
    "select" {:kind :select :statement (parse-select tokens)}
    "insert" {:kind :insert :statement (parse-insert tokens)}
    "create" {:kind :create :statement (parse-create tokens)}
    nil))
