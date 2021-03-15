#lang plai-typed
(require plai-typed/s-exp-match)

(define-type Exp
  (numE [n : number])
  (idE [s : symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (appE [s : symbol]
        [args : (listof Exp)])
  (maxE [l : Exp]
        [r : Exp]))

(define-type Func-Defn
  (fd [name : symbol]
      [args : (listof symbol)]
      [body : Exp]))

(module+ test
  (print-only-errors #t))

;; An EXP is either
;; - `NUMBER
;; - `SYMBOL
;; - `{+ EXP EXP}
;; - `{* EXP EXP}
;; - `{SYMBOL EXP)

;; A FUNC-DEFN is
;; - `{define {symbol symbol} EXP}

;; parse ----------------------------------------
(define (parse [s : s-expression]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))] ;; number
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]  ;; identifier
    [(s-exp-match? `{+ ANY ANY} s)                      ;; addition
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)                      ;; multiplication
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{max ANY ANY} s)                    ;; max
     (maxE (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ...} s)                     ;; function call
     (appE (s-exp->symbol (first (s-exp->list s)))
           (map parse (rest (s-exp->list s))))]
    
    [else (error 'parse "invalid input")]))

;; parse list of symbol (any size)
;; should use map!!!(真香
(define (listof-s-exp->listof-symbol [s : (listof s-expression)]) : (listof symbol)
  (cond [(empty? s) empty]
        [else (cons (s-exp->symbol (first s)) (listof-s-exp->listof-symbol (rest s)))]))

   
(define (parse-fundef [s : s-expression]) : Func-Defn
  (cond
    [(s-exp-match? `{define {SYMBOL SYMBOL ...} ANY} s)
     (fd (s-exp->symbol (first (s-exp->list (second (s-exp->list s))))) ;; function name
         (check (map s-exp->symbol (rest (s-exp->list (second (s-exp->list s))))));; argument list
         (parse (third (s-exp->list s))))]
    [else (error 'parse-fundef "invalid input")]))

(define (check [symbols : (listof symbol)]) : (listof symbol)
  symbols)
  
(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `x)
        (idE 'x))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input")

  (test (parse-fundef `{define {double x} {+ x x}})
        (fd 'double (list 'x) (plusE (idE 'x) (idE 'x)))))


;; interp ----------------------------------------
(define (interp [a : Exp] [defs : (listof Func-Defn)]) : number
  (type-case Exp a
    [numE (n) n]
    [idE (s) (error 'interp "free variable")]
    [plusE (l r) (+ (interp l defs) (interp r defs))]
    [multE (l r) (* (interp l defs) (interp r defs))]
    [maxE (l r) (let ([left (interp l defs)]
                      [right (interp r defs)])
                  (if (> left right) left right))]
    [appE (s args) (local [(define fd (get-fundef s defs))]
                    (interp (subst-appE (map (lambda ([e : Exp]) (numE (interp e defs))) args)
                                   (fd-args fd)
                                   (fd-body fd))
                            defs))]))




(module+ test
  (test (interp (parse `2) empty)
        2)
  (test/exn (interp (parse `x) empty)
            "free variable")
  (test (interp (parse `{+ 2 1}) empty)
        3)
  (test (interp (parse `{* 2 1}) empty)
        2)
  (test (interp (parse `{+ {* 2 3}
                           {+ 5 8}})
                empty)
        19)
  (test (interp (parse `{max 1 2})
                (list))
        2)
  (test (interp (parse `{max {+ 4 5} {+ 2 3}})
                (list))
        9)
  (test (interp (parse `{f 1 2})
                (list (parse-fundef `{define {f x y} {+ x y}})))
        3)
  (test (interp (parse `{+ {f} {f}})
                (list (parse-fundef `{define {f} 5})))
        10)
  (test (interp (parse `{f 2 3})
                (list (parse-fundef `{define {f x y} {+ y {+ x x}}})))
        7)
  (test/exn (interp (parse `{f 1})
                    (list (parse-fundef `{define {f x y} {+ x y}})))
            "wrong arity"))


        

;; get-fundef ----------------------------------------
(define (get-fundef [s : symbol] [defs : (listof Func-Defn)]) : Func-Defn
  (cond [(empty? defs) (error 'get-fundef "undefined function")]
        [else (if (eq? s (fd-name (first defs)))
                  (first defs)
                  (get-fundef s (rest defs)))]))



;; subst ----------------------------------------
(define (subst [what : Exp] [for : symbol] [in : Exp])
  (type-case Exp in
    [numE (n) in]
    [idE (s) (if (eq? for s)
                 what
                 in)]
    [plusE (l r) (plusE (subst what for l)
                        (subst what for r))]
    [multE (l r) (multE (subst what for l)
                        (subst what for r))]
    [appE (s args) (appE s (map (lambda ([arg : Exp]) (subst what for arg)) args))]
    [maxE (l r) (maxE (subst what for l)
                      (subst what for r))]))
(define (subst-appE [whats : (listof Exp)] [fors : (listof symbol)] [in : Exp])
  (cond [(or (and (empty? whats) (not (empty? fors)))
             (and (not (empty? whats)) (empty? fors))) (error 'subst-appE "wrong arity")] ;; when two lists are not of the same size
        [(and (empty? whats) (empty? fors)) in]
        [else (subst-appE (rest whats) (rest fors) (subst (first whats) (first fors) in))]))

(module+ test
  (test (subst (parse `8) 'x (parse `9))
        (numE 9))
  (test (subst (parse `8) 'x (parse `x))
        (numE 8))
  (test (subst (parse `8) 'x (parse `y))
        (idE 'y))
  (test (subst (parse `8) 'x (parse `{+ x y}))
        (parse `{+ 8 y}))
  (test (subst (parse `8) 'x (parse `{* y x}))
        (parse `{* y 8}))
  (test (subst (parse `8) 'x (parse `{double x}))
        (parse `{double 8})))