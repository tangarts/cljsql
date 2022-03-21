(ns cstack.ast)

;; AST

(deftype Statement
  {:select select-statement
   :create create-statement
   :insert insert-statement
   :kind nil})

(deftype Ast [^Statement statement])

 
;; INSERT
(defn insert-statement []
  {:table nil ; token
   :values nil ; []expression
   })

(defn expression
  []
  {:literal nil ;token
   :kind nil ; expressionkind
   })

;; CREATE
(defn column-def []
  {:name nil ;token
   :datatype nil ;token
   })

(defn select-statement []
  {:item nil ; [expression]
   :from nil ;token
   })

(defn create-statement []
  {:name nil ;token
   :cols nil ; [column-def]
   })


