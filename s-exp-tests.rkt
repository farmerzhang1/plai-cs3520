#lang plai-typed
(require plai-typed/s-exp-match)
(define-type Exp
  (numE [n : number])
  (idE [s : symbol]))
(define (parse [s : s-expression]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))] ;; number
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]  ;; identifier
    [(s-exp-match? `{a SYMBOL ...} s) (idE (s-exp->symbol (first (rest (s-exp->list s)))))]))
;; SYMBOL ... 是一個東西（listof s-expression)

; no currying!
(define (curry? [a : number] [b : number])
  (* a b))