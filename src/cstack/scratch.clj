(ns cljsexp-simple.core
  (:require [clojure.pprint :as p]))

(def funcs {"prn" println
            "+" +})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn currentLine [state]
  (get (:code state) (:line state)))

(defn currentChar [state]
  (get (currentLine state) (:col state)))

(defn moveRight [state by]
  (let [updated (assoc state :col (+ (:col state) by))
        line (currentLine state)]
    (if (<= (:col updated) (count line))

        ;Still space on current line, return it
      updated

        ;Move to next line
      (assoc
       state
       :col 0
       :line (inc (:line state))))))

(defn parseRegex [state, typeName, token, re]
  (let [s (subs (currentLine state) (:col state))
        val (re-find re s)]
    ;Does the remainder of the line match the regex - it should!
    (if val

      (assoc
       (moveRight state (count val))
       :token token
       :val val)

      (throw (Exception. (str "Failed to parse " typeName))))))

(defn parseName [state]
  (parseRegex state "name" :name #"[a-zA-Z\+\-\*\\\/\?_\$\<\>=]+"))

(defn parseComment [state]
  (parseRegex state "comment" :comment #";.*"))

(defn parseString [state]
  (parseRegex state "string" :string #"'[^']+'"))

(defn parseNumber [state]
  (parseRegex state "number" :number #"\d+"))

;TODO replace with a case + cond in the next method.
(def tokenMap {:byChar {\( :lparen
                        \) :rparen,
                        \[ :lbracket,
                        \] :rbracket}
               :byRegex {#"'" parseString
                         #"\d+" parseNumber
                         #"[a-zA-Z\+\-\*\\\/\?_\$\<\>=]" parseName
                         #";" parseComment}})

(defn clearToken [state]
  (assoc state
         :token :none
         :val :none))

(defn- nextToken [state]
  (let [c (currentChar state)]
    (cond

      (nil? c) (clearToken state)

     ;Ignore white space
      (Character/isSpaceChar c) (recur (moveRight state 1))

     ;Check if a token can be found in the token map by character
      :else  (if-let [token ((:byChar tokenMap) c)]
               (assoc (moveRight state 1) :token token :val c)

              ;Nothing found so now search by regex
              ; Get the function associated with the first regex that matches and call that
               (if-let [r (first (filter #(re-matches (% 0) (str c)) (:byRegex tokenMap)))]
                 ((r 1) state)
                 (throw (Exception. (str "dont understand next token - " c state))))))))

(defn- tokenise [state]
  (loop [nextState (nextToken state), tokens []]
    (if (= :none (:token nextState))
      tokens
      (recur
       (nextToken nextState)
       (conj tokens {:line (:line nextState),
                     :col (:col nextState),
                     :val (:val nextState),:type (:token nextState)})))))

(defn- parseExpression [token tokens]
  (case (:type token)
    (nil '()) [nil tokens]

    :name {:expr {:type :name,
                  :val (:val token),
                  :line (:line token),
                  :col (:col token),
                  :expressions []}
           :tokens tokens}

    :string {:expr {:type :string,
                    :val (:val token),
                    :line (:line token),
                    :col (:col token),
                    :expressions []}
             :tokens tokens}

    :number {:expr {:type :number,
                    :val (read-string (:val token)),
                    :line (:line token),
                    :col (:col token),
                    :expressions []}
             :tokens tokens}

    (:lparen :lbracket) (let [grp (if (= :lparen (:type token))
                                    {:start :lparen, :end :rparen, :type :list}
                                    {:start :lbracket, :end :rbracket, :type :vector})]
                          (loop [expressions []
                                 [loopToken & loopTokens] tokens]

                            (let [type (:type loopToken)]
                              (cond
                                (or (nil? token) (= '() token)) (throw (Exception. (str "EOF waiting for :rparen")))

                                (= (:end grp) type) {:expr {:type (:type grp)
                                                            :val (:type grp)
                                                            :line (:line token)
                                                            :col (:col token)
                                                            :expressions expressions}
                                                     :tokens loopTokens}

                                :else (let [r (parseExpression loopToken loopTokens)]
                                        (recur (conj expressions (:expr r)) (:tokens r)))))))))

(defn- parseAll [allTokens]
  (loop [expressions [], tokens allTokens]
    (let [r (parseExpression (first tokens) (rest tokens))]
      (if (= 0 (count (:expr r)))
        expressions
        (recur (conj expressions (:expr r)) (:tokens r))))))

(defn parse [code]
  (let [tokens (tokenise {:code code, :line 0, :col 0, :val :none, :token :none})
        result (parseAll tokens)]
    (p/pprint result)
    result))

(def code ["(id > 10 and id < 3)"])
(def t (tokenise {:code code, :line 0, :col 0, :val :none, :token :none}))
(p/pprint t)
(p/pprint
 (parseExpression (first t) (rest t)))

(defn binding-power [token]
  (case (token :type)
    :keyword (case (token :token)
               ("and" "or") 1
               0)
    :symbol (case (token :token)
              ("=" "<>" "||" "+") 3
              0)
    0))

(binding-power {:token "where" :type :keyword})

(def tokens [{:token "id", :type :identifier}
             {:token "=", :type :symbol}
             {:token "1", :type :number}

             {:token "and" :type :symbol}

             {:token "2", :type :identifier}
             {:token "+", :type :symbol}
             {:token "2", :type :number}])

(defn expr-bp [token tokens]
  (if (= (token :type) :symbol) (println "Bad token")
      (loop [expressions []
             [loopToken & loopTokens] tokens
             min-bp 0]
        (cond
          (or (nil? token) (= '() token))
          (throw (Exception. (str "EOF waiting for :rparen")))

          (< (binding-power token) min-bp) 
          {:expr {:val :symbol
                  :expressions expressions}
           :tokens loopTokens}

          ; (= :rparen (loopToken :token))
          ; {:expr {:val :symbol
          ;         :expressions expressions}
          ;  :tokens loopTokens}

          :else (let [r (parseExpression loopToken loopTokens)]
                  (recur (conj expressions (:expr r))
                         (:tokens r)
                         (binding-power token)))))))

(expr-bp {:token "2", :type :number} [])
