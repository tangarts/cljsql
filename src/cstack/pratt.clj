(ns cstack.pratt)

(def ^:dynamic *verbose* false)

(defmacro printfv
  [fmt & args]
  `(when *verbose*
     (printf ~fmt ~@args)))

(defmacro with-verbose
  [& body]
  `(binding [*verbose* true] ~@body))

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

(def precedence '{* 3, / 3 + 1, - 1})

(declare parse)
(defn- -parse [& tokens]
  (let
   [[t tn] tokens]
    (if
     (nil? tn) (first t)
     (loop [[t tn & ts] tokens
            acc t
            minbp 0]
       (println "t:" t)
       (println "tn:" tn)
       (println "ts:" ts)
       (println "acc: " acc)
       (cond
         (nil? tn) {:kind :binary
                    :expressions acc}
         ;  (number? t) (recur ts (update acc :expression 
         ;                                conj {:op tn
         ;                                   :left t
         ;                                   :right (-parse ts)}))
         (<= (precedence tn) minbp)
         {:kind :binary :expressions acc}

         (op tn)
         (let [r (-parse ts)]
           (println "r:" r)
           (recur ts (cons tn ; {:op tn :left t :right (-parse ts)})
                           (list acc r))
                  (inc (precedence tn)))))))))

(defn multi [& tokens]
  (let [[t tn] tokens]
    (list t tn)))

(defn parse [tokens]
  (let [t (first tokens) 
        tn (rest tokens)]
    (if
     (nil? tn) t
     (loop [[t tn & ts] tokens
            acc t
            minbp 0]
       (println "t:" t)
       (println "tn:" tn)
       (println "ts:" ts)
       (println "acc: " acc)
       (cond
         (nil? tn) {:kind :binary
                    :expressions acc}

        (<= (precedence tn) minbp)
         {:kind :binary :expressions acc}

         (op tn)
         (let [r (:expressions (parse ts))]
           (println "r:" r)
           (recur ts (cons tn ; {:op tn :left t :right (-parse ts)})
                           (list acc r))
                  (inc (precedence tn)))))))))

(parse '(1))
(parse '(1 * 2))
(parse '(3 * 2 + 1))
(parse [1 '+ 2 '* 3])
(parse '(a + b * c * d + e))
(parse '(2 * 3))

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

