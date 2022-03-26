(ns cljsexp-simple.core
  (:require [clojure.pprint :as p]
            [clojure.walk :refer [postwalk]]))


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

(defn node [x op y]
  {:kind :binary
   :expr {:a x :op op :b y}})

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
