(ns cljlox.expr
  (:require [cljlox.token :refer [->Token]]
            [clojure.string :as str]))

;; Example expression
(def expr-doc
  {;; The key on which multimethods dispatch; :expr-type is one of the members
   ;; of the set below. This set is also the reference for which expression
   ;; types have been implemented, and the dispatch values for which
   ;; multimethods need to be defined for the multimethod implementation to be
   ;; "complete".
   :expr-type   #{:binary
                  :unary
                  :literal
                  :grouping}

   ;; For binary operators
   :left        "The left operand; an expression"
   ;; The following two also constitute the keys of a unary operation.
   :operator    "The actual binary operator; a Token record"
   :right       "The right operand; an expression"

   ;; For literals
   :value       "The value of the literal; raw (ie, not textual)"

   ;; For grouping expressions
   :expression  "The expression being grouped; an expression"})

;; Example AST construction below, through a series of defs.

(def literal-expr
  {:expr-type   :literal
   :value       123})

(def unary-expr
  {:expr-type   :unary
   :operator    (->Token :minus "-" nil 1)
   :right       literal-expr})

(def grouping-expr
  {:expr-type   :grouping
   :expression  {:expr-type :literal, :value 45.67}})

(def binary-expr
  {:expr-type   :binary
   :left        unary-expr
   :operator    (->Token :star "*" nil 1)
   :right grouping-expr})

(defmulti expr->str :expr-type)

(defn parenthesize [nm & exprs]
    (let [expr-strs (map expr->str exprs)]
      (str "(" nm " " (str/join " " expr-strs) ")")))

(defmethod expr->str :binary [expr]
  (parenthesize (get-in expr [:operator :lexeme])
                (:left expr)
                (:right expr)))

(defmethod expr->str :grouping [expr]
  (parenthesize "group" (:expression expr)))

(defmethod expr->str :literal [expr]
  (if (nil? (:value expr)) "nil"
    (str (:value expr))))

(defmethod expr->str :unary [expr]
  (parenthesize (get-in expr [:operator :lexeme])
                (:right expr)))

(comment
  (expr->str binary-expr))
