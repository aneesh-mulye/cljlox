(ns cljlox.token)

(defrecord Token [token-type lexeme literal line])
