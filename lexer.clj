(ns cstack)

(def meta-command
  {:exit ".exit" :help ".help" :open ".open" :quit ".quit"})

(def keywords
  {:select  "select"
   :from    "from"
   :as      "as"
   :table   "table"
   :create  "create"
   :insert  "insert"
   :into    "into"
   :values  "values"
   :int     "int"
   :text    "text"})

(def symbols
  {:semicolon  ";"
   :asterisk   "*"
   :comma      ","
   :leftparen  "("
   :rightparen ")"})


(:exit meta-command)
