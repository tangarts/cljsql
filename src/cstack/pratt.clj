(ns cstack.pratt)

(def ^:dynamic *verbose* false)

(defmacro printfv
  [fmt & args]
  `(when *verbose*
     (printf ~fmt ~@args)))

(defmacro with-verbose
  [& body]
  `(binding [*verbose* true] ~@body))

(def op #{'+ '- '* '/})

(defn prec [op]
  (case op
    (+ -) [1 2]
    (* /) [3 4]
    (println "Not an op")))

(second [1 2])

(defn multi [tokens]
  (let [[t tn & ts] tokens]
    (list t tn ts)))

(multi '(1))

(defn parse [tokens minbp]
  ; (let [t (first tokens) 
  ;       tn (rest tokens)]
  ;   (if (nil? tn) t)
  ;   )
  (loop [[t tn & ts] tokens
         acc t]
    (println "t:" t)
    (println "tn:" tn)
    (println "ts:" ts)
    (println "acc: " acc)
    (cond
      (nil? tn) {:kind :binary
                 :expressions acc}

      (op tn)
      (let [[lbp rbp] (prec tn)]
        (if (< lbp minbp) {:kind :binary :expressions acc}

            (let [r (:expressions (parse ts rbp))]
              (println "r:" r)
              (println "count ts " (count ts))
              (recur ts (cons tn ; {:op tn :left t :right (-parse ts)})
                              (list acc r))))))
      :else
      acc)))

(parse '(1) 0)
(parse '(1 * 2) 0)
(parse '(3 * 2 + 1) 0) ; {:kind :binary, :expressions (+ (* 3 2) 1)}
(parse '(1 + 2 * 3) 0)
(parse '(a + b * c * d + e) 0)

(defn infix [& exprs]
  (loop [[x & xs] exprs
         acc 0
         oper +]
    (cond
      (nil? x) acc
      (number? x) (recur xs (oper acc x) nil)
      :else (recur xs acc x))))

(defn reorder-equation [arg]
  (if (seq? arg)
    (let [[f s & r] arg
          f (reorder-equation f)]
      (cond
        (#{"*" "/"} (str s))
        (let [[t ft & r2] r
              t (reorder-equation t)]
          (if ft
            (list ft (list s f t) (reorder-equation r2))
            (list s f t)))
        (nil? s) f
        :else (list s f (reorder-equation r))))
    arg))

(reorder-equation '(a + b * c * d + e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; https://github.com/fogus/unfix/blob/master/src/joy/unfix/infix.clj

(def && #(and   %1 %2))
(def || #(or    %1 %2))
(def != #(not=  %1 %2))

(def ^:dynamic *ops* '[- + * / < > && || = !=])
(def rank (zipmap *ops* (iterate inc 1)))
; (def ^:dynamic _ clojure.core/resolve)

(defn- infix* 
  [[a b & [c d e & more] :as _]]
  (cond
   (vector? a) (recur (list* (infix* a) b c d e more))
   (vector? c) (recur (list* a b (infix* c) d e more))
   (rank b)    (if (and d (< (rank b 0) (rank d 0)))
                 (recur (list a b (infix* (list* c d e more))))
                 (recur (list* (list b a c) d e more)))
   :else a))

(infix* '[a + b * c * d + e])
