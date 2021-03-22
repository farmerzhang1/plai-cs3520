#lang plait

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol]))
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))] ;; number
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]  ;; identifier
    [(s-exp-match? `{a SYMBOL ...} s) (idE (s-exp->symbol (first (rest (s-exp->list s)))))]))
;; SYMBOL ... 是一個東西（listof s-expression)

; no currying!
(define (curry? [a : Number] [b : Number])
  (* a b))