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

(defn- -parse [& tokens]
  (let
   [[t tn] tokens]
    (cond
      (nil? tn) (first t)
      :else
      (loop [[t tn & ts] tokens
             acc []]
        (println "t:" t)
        (println "tn:" tn)
        (println "ts:" ts)
        (println "acc: " acc)
        (cond
          (nil? tn) acc
          (number? t) (recur ts (conj acc {:op tn
                                           :left t
                                           :right (-parse ts)})))))))
(defn parse [tokens]
  (reduce #(assoc %1 :right %2) (apply -parse tokens)))

(parse [1 '+ 2 '* 3])

(second '(1))

(defn infix [& exprs]
  (loop [[x & xs] exprs
         acc 0
         oper +]
    (cond
      (nil? x) acc
      (number? x) (recur xs (oper acc x) nil)
      :else (recur xs acc x))))


