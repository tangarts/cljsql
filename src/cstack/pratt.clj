(ns cstack.pratt
  (:require [clojure.test :refer :all]
            [clojure.pprint :as p]))

(def op '#{and or = <> < > <= >= || +})

(defn binding-power [op]
  (case op
    (and or) 1
    (= <>) 2
    (< >) 3
    (<= >=) 4
    (|| +) 5
    0))

(defn node [op x y]
  {:kind :binary
   :expr {:a x :op op :b y}})

(defn parse-binary
  "Parse Binary expression"
  [arg]
  (if (seq? arg)
    (loop [[f s & r] arg]
      (if (nil? s) f
        (let [[t ft & _] r]
          (cond
            
            (op s)
            (if (and ft (< (binding-power s) (binding-power ft)))
              (node s f (parse-binary r))
              (recur (list* (node s f t) (rest r))))

            :else
              ; (node s f (calc r))
            (println "Binary expression expected")))))
    arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(parse-binary '(1))
(parse-binary '(1 + 2))
(p/pprint (parse-binary '(1 + 2 and 3 > 4)))
(p/pprint
 (parse-binary '(select * from t where id = 1 and c > d + e)))

(p/pprint
 (parse-binary '(a + b and c + d <> e)))

(deftest pratt-parse
  (are [x y] (= (parse-binary x) y)
    '(1) 1
    '(1 + 2) '(+ 1 2)
    '(1 + 2 * 3) '(+ 1 (* 2 3))))

(run-tests 'cstack.pratt)

(comment 
; https://github.com/fogus/unfix/blob/master/src/joy/unfix/infix.clj

(def ^:dynamic *ops* '[- + * / < > and or = <>])
(def rank (zipmap *ops* (iterate inc 1)))

(defn- infix*
  [[a b & [c d e & more] :as _]]
  (cond
    (vector? a) (recur (list* (infix* a) b c d e more))
    (vector? c) (recur (list* a b (infix* c) d e more))
    (rank b)    (if (and d (< (rank b 0) (rank d 0)))
                  (recur (list a b (infix* (list* c d e more))))
                  (recur (list* (list b a c) d e more)))
    :else a))
)
