#lang plait

(define-type Value
  (numV [n : Number])
  (closV [arg : Symbol]
         [body : Exp]
         [env : Env]))

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (lamE [n : Symbol]
        [body : Exp])
  (appE [fun : Exp]
        [arg : Exp])
  (if0E [tst : Exp]
        [thn : Exp]
        [els : Exp]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors #t))

(define mk-rec-fun
  `{lambda {body-proc}
     {let {[fX {lambda {fX}
                 {let {[f {lambda {x}
                            {{fX fX} x}}]}
                   {body-proc f}}}]}
       {fX fX}}})

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first ;;let's take it as the SYMBOL
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appE (lamE (s-exp->symbol (first bs))
                   (parse (third (s-exp->list s))))
             (parse (second bs))))];; wow!
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol (first (s-exp->list
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{if0 ANY ANY ANY} s)
     (if0E (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [(s-exp-match? `{letrec {[SYMBOL ANY]} ANY} s)
     (let* ([binding (s-exp->list (first (s-exp->list (second (s-exp->list s)))))]
            [name (first binding)]
            [rhs (second binding)]
            ;[rec (appE (parse mk-rec-fun) (lamE (s-exp->symbol (first binding)) (parse (second binding))))]
            [body (third (s-exp->list s))])
       (parse `{let {[,name {,mk-rec-fun {lambda {,name} ,rhs}}]} ,body}))]
    [else (error 'parse "invalid input")]))
;; (letrec ([fac (lambda (n) (if (zero? n) 1 (* n (fac (- n 1)))))]) (fac 10))
(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x) ; note: backquote instead of normal quote
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{+ {+ 3 4} 8})
        (plusE (plusE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{let {[x {+ 1 2}]}
                  y})
        (appE (lamE 'x (idE 'y))
              (plusE (numE 1) (numE 2))))
  (test (parse `{lambda {x} 9})
        (lamE 'x (numE 9)))
  (test (parse `{if0 1 2 3})
        (if0E (numE 1) (numE 2) (numE 3)))
  (test (parse `{double 9})
        (appE (idE 'double) (numE 9)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input"))

;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env]) : Value
  (type-case Exp a
    [(numE n) (numV n)]
    [(idE s) (lookup s env)]
    [(plusE l r) (num+ (interp l env) (interp r env))]
    [(lamE n body)
     (closV n body env)]
    [(appE fun arg) (type-case Value (interp fun env)
                      [(closV n body c-env)
                       (interp body
                               (extend-env
                                (bind n
                                      (interp arg env))
                                c-env))]
                      [else (error 'interp "not a function")])]
    [(if0E tst thn els)
     (interp (if (num-zero? (interp tst env))
                 thn
                 els)
             env)]))

(module+ test
  (test (interp (parse `2) mt-env)
        (numV 2))
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env))
        (numV 9))
  (test (interp (parse `{+ 2 1}) mt-env)
        (numV 3))
  (test (interp (parse `{+ {+ 2 3} {+ 5 8}})
                mt-env)
        (numV 18))
  (test (interp (parse `{lambda {x} {+ x x}})
                mt-env)
        (closV 'x (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        (numV 12))
  (test (interp (parse `{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env)
        (numV 5))
  (test (interp (parse `{{lambda {x} {+ x x}} 8})
                mt-env)
        (numV 16))
  
  (test (interp (parse `{if0 0 2 3})
                mt-env)
        (numV 2))
  (test (interp (parse `{if0 1 2 3})
                mt-env)
        (numV 3))

  (test/exn (interp (parse `{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse `{if0 {lambda {x} x} 2 3})
                    mt-env)
            "not a number")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env)
            "free variable")
    (test (interp (parse `{letrec {[f {lambda {n}
                                      {if0 n
                                           0
                                           {+ {f {+ n -1}} -1}}}]}
                          {f 10}})
                 mt-env)
          (numV -10)))

;; num+ ----------------------------------------
(define (num-op [op : (Number Number -> Number)] [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r))
    (numV (op (numV-n l) (numV-n r)))]
   [else
    (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num-zero? [v : Value]) : Boolean
  (type-case Value v
    [(numV n) (zero? n)]
    [else (error 'interp "not a number")]))

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num-zero? (numV 0))
        #t)
  (test (num-zero? (numV 1))
        #f))

;; lookup ----------------------------------------
(define (lookup [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
   [empty (error 'lookup "free variable")]
   [(cons b rst-env) (cond
                       [(symbol=? n (bind-name b))
                        (bind-val b)]
                       [else (lookup n rst-env)])]))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (numV 8)) mt-env))
        (numV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'x (numV 8)) mt-env)))
        (numV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'y (numV 8)) mt-env)))
        (numV 8)))