(ns cstack.pratt)

;;; function parse(tokens):
;    firstToken = tokens.consume()
;
;    if firstToken is a number
;        leftNode = NumberNode(firstToken)
;    otherwise
;        Error("Expected a number")
;
;    nextToken = tokens.consume()
;
;    if nextToken is an operator token
;        rightNode = parse(tokens)
;
;        return OperatorNode(
;            operator: nextToken,
;            leftNode,
;            rightNode
;        )
;
;    otherwise
;        return leftNode

; [1, "/", 2, "+", 3.4].consume() -> 1, ["/", 2, "+", 3.4]
; [1, "/", 2, "+", 3.4].peek() -> 1, [1, "/", 2, "+", 3.4]
; 
; [].consume() -> empty token, []
; [].peek() -> empty token, []

(def op #{'+ '- '* '/})
(op '+)
(if (op '-) "in" "out")

(def precedence '{* 0, / 0 + 1, - 1})

(declare parse)
(defn- -parse [& tokens]
  (let
   [[t tn] tokens]
    (if
      (nil? tn) (first t)
      (loop [[t tn & ts] tokens
             acc {:kind :binary
                          :expression []}]
        (println "t:" t)
        (println "tn:" tn)
        (println "ts:" ts)
        (println "acc: " acc)
        (cond
          (nil? tn) acc
          (number? t) (recur ts (update acc :expression 
                                        conj {:op tn
                                           :left t
                                           :right (-parse ts)})))))))

(parse [3 '* 2 '+ 1])

(defn parse [tokens]
  ;(reduce #(assoc %1 :right %2) )
  (apply -parse tokens))

(def maps [{:op '*, :left 4, :right 1} 
           {:op '+, :left 1, :right 2} 
           {:op '*, :left 2, :right 3}])

(-> (first maps)
    (assoc-in (into [] (repeat 1 :right)) (get maps 1))
    (assoc-in (into [] (repeat 2 :right)) (get maps 2)))


(defn infix [& exprs]
  (loop [[x & xs] exprs
         acc 0
         oper +]
    (cond
      (nil? x) acc
      (number? x) (recur xs (oper acc x) nil)
      :else (recur xs acc x))))

