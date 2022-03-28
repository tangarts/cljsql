(ns cstack.scratch
  (:require [clojure.pprint :as p]
            [clojure.walk :refer [postwalk]]
            [clojure.test :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; {:kind :binary
;  :expr {:a {:token "id" :type :identifier}
;         :b {:token "1" :type :number} 
;         :op {:token :type :symbol}}}

(def tok [{:token "id", :type :identifier}
          {:token "=", :type :symbol}
          {:token "1", :type :number}
          {:token "and" :type :symbol}
          {:token "2", :type :identifier}
          {:token "+", :type :symbol}
          {:token "2", :type :number}])

(def precedence '{* 0, / 0 + 1, - 1})

(defn order-ops
  "((A x B) y C) or (A x (B y C)) depending on precedence of x and y"
  [[A x B y C & more]]
  (let [ret (if (<=  (precedence x) (precedence y))
              (list (list A x B) y C)
              (list A x (list B y C)))]
    (if more
      (recur (concat ret more))
      ret)))

(defn add-parens
  "Tree walk to add parens.  All lists are length 3 afterwards."
  [s]
  (postwalk
   #(if (seq? %)
      (let [c (count %)]
        (cond (even? c) (throw (Exception. "Must be an odd number of forms"))
              (= c 1) (first %)
              (= c 3) %
              (>= c 5) (order-ops %)))
      %)
   s))

(add-parens (subvec tok 0 5))

(p/pprint
 (-> '(3 * 2 + 1) add-parens))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  [tokens]
  (if (seq? tokens)
    (loop [[f s & r] tokens]
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
    tokens))

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
      :else a)))
