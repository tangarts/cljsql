(ns cstack.pratt
  (:require [clojure.test :refer :all]
            [clojure.pprint :as p]))

(def op #{'+ '- '* '/})

(defn prec [op]
  (case op
    (+ -) [1 2]
    (* /) [3 4]
    (println "Not an op")))

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

; (parse '(1) 0)
; (parse '(1 * 2) 0)
; (parse '(3 * 2 + 1) 0) ; {:kind :binary, :expressions (+ (* 3 2) 1)}
; (parse '(1 + 2 * 3) 0)
; (parse '(a + b * c * d + e) 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; https://github.com/fogus/unfix/blob/master/src/joy/unfix/infix.clj

(def ^:dynamic *ops* '[- + * / < > and or = <>])
(def rank (zipmap *ops* (iterate inc 1)))

(defn binding-power [op]
  (case op
    (and or) 1
    (= <>) 2
    (< >) 3
    (<= >=) 4
    (|| +) 5
    0))

(defn- infix*
  [[a b & [c d e & more] :as _]]
  (cond
    (vector? a) (recur (list* (infix* a) b c d e more))
    (vector? c) (recur (list* a b (infix* c) d e more))
    (rank b)    (if (and d (< (rank b 0) (rank d 0)))
                  (recur (list a b (infix* (list* c d e more))))
                  (recur (list* (list b a c) d e more)))
    :else a))

(infix* '[a + b * c + d * e])


(defn calc [arg]
  (if (seq? arg)
    (loop [[f s & r] arg]
      (if (nil? s)
        f
        (let [[t ft & _] r]
          (cond

            (rank s)
            (if (and ft (< (binding-power s) (binding-power ft)))

              (list s f (calc r))
              (recur (list* (list s f t) (rest r)) ))
;                  (recur (list a b (infix* (list* c d e more))))
;                  (recur (list* (list b a c) d e more)))


            :else
              (list s f (calc r))))))
    arg))

(calc '(1))
(calc '(1 + 2))
(calc '(1 + 2 and 3 > 4))
(calc '(id = 1 and c > d + e)) 
(infix* '[a + b * c = d * e])
(calc '(a + b and c + d * e))

(deftest pratt-parse
  (are [x y] (= (calc x) y)
       '(1) 1
       '(1 + 2) '(+ 1 2)
       '(1 + 2 * 3) '(+ 1 (* 2 3))
       ))

(run-tests 'cstack.pratt)
