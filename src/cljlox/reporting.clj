(ns cljlox.reporting)

(defn report [linenum where message]
  (binding [*out* *err*]
    (println (str "[line " linenum "] Error" where ": " message))))

(defn error [linenum message]
  (report linenum "" message))

