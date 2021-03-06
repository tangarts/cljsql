(ns cstack.lexer
  (:require [clojure.string :as str]))

(def meta-command
  #{".exit" ".help" ".open" ".quit"})

(def keywords
  #{"select" "from" "as" "table" "create" "insert" "into" "values" "int" "text"})

(def symbols
  {:semicolon  ";"
   :asterisk   "*"
   :comma      ","
   :leftparen  "("
   :rightparen ")"})

(def token-kind #{:keyword :symbol :identifier :string :number})

;;; ==========================================================

;; http://regexadvice.com/forums/thread/29185.aspx
(def REGEX
  #"(?:(['\"])(?:\\\\|\\\1|(?!\1).|\1\1)*\1|(?:(?<!\d)-)?\d+(?:\.\d+)?(?:[eE]-?\d+)?|\.\.|(?:\w+\.)*\w+|[<>=|]{2}|\S)")

(defn digit? [c]
  (re-find #"[0-9]" c))

(defn letter? [c]
  (re-find #"[a-zA-Z]" c))

(defn asymbol? [c]
  (re-find #"[||\(\)\[\]!\.+-><=\?*]" c))

(defn astring? [c]
  (re-find #"[\"']" c))

(defn what-type [token]
  (let [c (subs token 0 1)]
    (cond
      (keywords token) :keyword
      (letter? c)  :identifier
      (digit? c)   :number
      (asymbol? c) :symbol
      (astring? c) :string
      :else (throw (IllegalArgumentException. (str "Don't know type of token:" token))))))

(defn typer [token]
  {:token (str/lower-case token)
   :col (count token)
   :type (what-type token)})

(defn lex [sql-str]
  (let [tokens (map typer (map first (re-seq REGEX sql-str)))]
    (mapv #(assoc %1 :col %2)
          tokens
          (reductions + 0 (map #(-> % :token count) tokens)))))
