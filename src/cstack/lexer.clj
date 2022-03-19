(ns cstack.lexer)

(def meta-command
  {:exit ".exit" :help ".help" :open ".open" :quit ".quit"})

(def keywords
  #{"select" "from" "as" "table" "create" "insert" "into" "values" "int" "text"})

(def symbols
  {:semicolon  ";" :asterisk   "*" :comma      "," :leftparen  "(" :rightparen ")"})

(def token-kind
  #{:keyword :symbol :identifier :string :number})

(defrecord location [line col])
(defrecord cursor [pointer loc])

;;; ==========================================================

;; http://regexadvice.com/forums/thread/29185.aspx
(def REGEX
  #"(?:(['\"])(?:\\\\|\\\1|(?!\1).|\1\1)*\1|(?:(?<!\d)-)?\d+(?:\.\d+)?(?:[eE]-?\d+)?|\.\.|(?:\w+\.)*\w+|[<>=|]{2}|\S)")

(defn digit? [c]
  (re-find #"[0-9]" c))

(defn letter? [c]
  (re-find #"[a-zA-Z]" c))

(defn asymbol? [c]
  (re-find #"[\(\)\[\]!\.+-><=\?*]" c))

(defn astring? [c]
  (re-find #"[\"']" c))

(defn what-type [token]
  (let [c (subs token 0 1)]
    (cond
      (keywords token) :keyword
      (letter? c)  :word
      (digit? c)   :number
      (asymbol? c) :symbol
      (astring? c) :string
      :else (throw (IllegalArgumentException. (str "Don't know type of token:" token))))))

(keywords "select")
(what-type "select")
(defn typer [token]
  {:token token
   :type (what-type token)})

(asymbol? "1- >= 3")

(defn lex [sql-str]
  (map typer (map first (re-seq REGEX sql-str))))

(lex "select * from places where locality=\"los angeles\";")
(def sql-str  "insert into db values (1, \"username\", \"user@clj.org\"")
(def sql-str1  "select * from places where locality=\"los angeles\";")
