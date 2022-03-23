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
  [input]
  ; "TODO: validation"
  ; some validation in parse exprs
  (let [[_ from-expr]
        (split-with #(not= (-> % :token) "from") (rest input))]
    (if (not= '() from-expr)
      {:table (-> from-expr second :token keyword)
       :expression (mapv #(-> % :token keyword)
                         (parse-exprs (rest input) "from"))}
      (println  "Expected FROM clause"))))

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
