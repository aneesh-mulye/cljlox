(ns cljlox.scanner
  (:require [cljlox.token :refer [->Token]]
            [cljlox.reporting :refer [error]]))

(def one-char-lexemes-unambig
  {\( :left-paren
   \) :right-paren
   \{ :left-brace
   \} :right-brace
   \, :comma
   \. :dot
   \- :minus
   \+ :plus
   \; :semicolon
   \* :star})

(def two-char-lexemes-ambig
  {"!=" :bang-equal
   "==" :equal-equal
   "<=" :less-equal
   ">=" :greater-equal})

(def one-char-lexemes-ambig
  {\! :bang
   \= :equal
   \< :less
   \> :greater})

(def whitespace-lexemes
  #{\space \tab \return})

(def digit-lexemes #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(def keyword-lexemes
  {"and"    :and
   "class"  :class
   "else"   :else
   "false"  :false
   "for"    :for
   "fun"    :fun
   "if"     :if
   "nil"    :nil
   "or"     :or
   "print"  :print
   "return" :return
   "super"  :super
   "this"   :this
   "true"   :true
   "var"    :var
   "while"  :while})

(defn alpha? [c]
  (let [cv (int c)]
    (or (<= (int \a) cv (int \z))
        (<= (int \A) cv (int \Z))
        (= c \_))))

(defn alphanumeric? [c]
  (or (alpha? c)
      (digit-lexemes c)))

;; Assumes that s starts at the beginning of the string, *after* the beginning
;; string opening quote has been consumed.
(defn scan-for-string [s]
  (loop [remaining s
         num-newlines 0
         text []]
    (if (not (seq remaining)) [nil num-newlines nil]
      (let [f (first remaining)]
        (if (= f \") [(apply str text) num-newlines (rest remaining)]
          (recur (rest remaining)
                 (+ num-newlines (if (= f \newline) 1 0))
                 (conj text f)))))))

(defn scan-for-number [s]
  (loop [remaining s
         text []]
    (let [f (first remaining)
          s (second remaining)]
      (cond
        (digit-lexemes f) (recur (rest remaining) (conj text f))

        (and (= f \.) (digit-lexemes s))
        (recur (drop 2 remaining) (conj text f s))

        :else [(apply str text) remaining]))))

(defn scan-for-identifier [s]
  (loop [remaining s
         text []]
    (let [f (first remaining)]
      (if ((complement alphanumeric?) f) [(apply str text) remaining]
        (recur (rest remaining) (conj text f))))))

(defn scan-tokens [src]
  (loop [remaining src
         status nil
         tokens []
         linenum 1]
    (if (not (seq remaining))
      [(or status :ok) (conj tokens (->Token :eof "" nil linenum))]
      (let [f (first remaining)
            s (second remaining)]
        (cond
          ;; Unambiguous single-character lexemes.
          (one-char-lexemes-unambig f)
          (recur (rest remaining)
                 status
                 (conj tokens (->Token (one-char-lexemes-unambig f)
                                       (str f)
                                       nil
                                       linenum))
                 linenum)

          ;; Two-character lexemes whose first char is independently a lexeme.
          (two-char-lexemes-ambig (str f s))
          (recur (drop 2 remaining)
                 status
                 (conj tokens (->Token (two-char-lexemes-ambig (str f s))
                                       (str f s)
                                       nil
                                       linenum))
                 linenum)

          ;; One-char lexemes that could have been two-char, but here weren't.
          (one-char-lexemes-ambig f)
          (recur (rest remaining)
                 status
                 (conj tokens (->Token (one-char-lexemes-ambig f)
                                       (str f)
                                       nil
                                       linenum))
                 linenum)

          ;; Comment(s).
          (= "//" (str f s))
          (recur (drop-while #(not= \newline %) remaining)
                 status
                 tokens
                 linenum)

          ;; Slash
          (= f \/)
          (recur (rest remaining)
                 status
                 (conj tokens (->Token :slash
                                       (str f)
                                       nil
                                       linenum))
                 linenum)

          ;; Whitespace (ignore).
          (whitespace-lexemes f)
          (recur (rest remaining)
                 status
                 tokens
                 linenum)

          ;; Newline
          (= f \newline)
          (recur (rest remaining)
                 status
                 tokens
                 (inc linenum))

          ;; String literals.
          (= f \")
          (let [[literal-string-text num-newlines remaining-after-string]
                (scan-for-string (rest remaining))]
            (if (not literal-string-text)
              (do
                (error linenum "Unterminated string.")
                (recur nil
                       :error
                       tokens
                       (+ linenum num-newlines)))
              (recur remaining-after-string
                     status
                     (conj tokens (->Token :string
                                           (str \" literal-string-text \")
                                           literal-string-text
                                           linenum))
                     (+ linenum num-newlines))))

          ;; Number literals.
          (digit-lexemes f)
          (let [[literal-number-text remaining-after-number]
                (scan-for-number remaining)]
            (recur remaining-after-number
                   status
                   (conj tokens (->Token :number
                                         literal-number-text
                                         (parse-double literal-number-text)
                                         linenum))
                   linenum))

          ;; Identifiers or keywords.
          (alpha? f)
          (let [[literal-identifier-text remaining-after-identifier]
                (scan-for-identifier remaining)]
            (recur remaining-after-identifier
                   status
                   (conj tokens (->Token
                                  (or (keyword-lexemes literal-identifier-text)
                                      :identifier)
                                  literal-identifier-text
                                  nil
                                  linenum))
                   linenum))

          ;; Final case: something we don't recognise as a valid character.
          :else
          (do
              (error linenum "Unexpected character.")
              (recur (rest remaining)
                     :error
                     tokens
                     linenum)))))))
