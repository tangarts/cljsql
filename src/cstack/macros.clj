(ns cstack.macros)

(defmacro condition
  ; http://blog.alex-turok.com/2014/05/how-to-make-sql-from-clojure-or-my.html
  ([_]
   `true)
  ([t complex]
   `(condition ~t ~@complex))
  ([t k1 op k2]
   (if (seq? k1)
     `(~op
       (condition ~t ~k1)
       (condition ~t ~k2))
     (let [v1 (if (keyword? k1) `(~k1 ~t) k1)
           v2 (if (keyword? k2) `(~k2 ~t) k2)]
       `(~op ~v1 ~v2))))
  ([t k1 op1 k2 link & other]
   (if (seq? k1)
     `(~op1
       (condition ~t ~k1)
       (condition ~t ~k2 ~link ~@other))
     `(~link
       (condition ~t ~k1 ~op1 ~k2)
       (condition ~t ~@other)))))

(defmacro condm
  [& conditions]
  `(fn [t#] (condition t# ~@conditions)))
