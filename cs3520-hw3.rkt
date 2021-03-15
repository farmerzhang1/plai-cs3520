#lang plai-typed
(require plai-typed/s-exp-match)
(define-type Value
  (boolV [b : boolean])
  (numV [n : number])
  (closV [arg : symbol]
         [body : Exp]
         [env : Env]))

(define-type Exp
  (trueE)
  (falseE)
  (equalE [l : Exp]
     [r : Exp])
  (ifE [predicate : Exp]
      [then : Exp]
      [else : Exp])
  (numE [n : number])
  (idE [s : symbol])
  (plusE [l : Exp]
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (letE [n : symbol]
        [rhs : Exp]
        [body : Exp])
  (unletE [n : symbol]
          [body : Exp])
  (lamE [n : symbol]
        [body : Exp])
  (appE [fun : Exp]
        [arg : Exp]))

(define-type Binding
  (bind [name : symbol]
        [val : Value]))

(define-type-alias Env (listof Binding))

(define mt-env empty)
(define extend-env cons)

(define (delete [to/delete : symbol] [from-env : Env])
  (cond [(empty? from-env) empty]
        [else
         (if (eq? to/delete (bind-name (first from-env)))
             (rest from-env)
             (cons (first from-env) (delete to/delete (rest from-env))))]))

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse [s : s-expression]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s)
     (cond [(eq? 'true (s-exp->symbol s)) (trueE)] ;; use parenthesis !
            [(eq? 'false (s-exp->symbol s)) (falseE)]
            [else (idE (s-exp->symbol s))])]
    ;[(s-exp-match? `{true} s) trueE]
    ;[(s-exp-match? `{false} s) falseE]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{= ANY ANY} s)
     (equalE (parse (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letE (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? `{unlet SYMBOL ANY} s) (unletE (s-exp->symbol (second (s-exp->list s)))
                                                  (parse (third (s-exp->list s))))]
    [(s-exp-match? `{if ANY ANY ANY} s) (ifE (parse (second (s-exp->list s)))
                                             (parse (third (s-exp->list s)))
                                             (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol (first (s-exp->list
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x)
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{let {[x {+ 1 2}]}
                  y})
        (letE 'x (plusE (numE 1) (numE 2))
              (idE 'y)))
  (test (parse `{lambda {x} 9})
        (lamE 'x (numE 9)))
  (test (parse `{double 9})
        (appE (idE 'double) (numE 9)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input"))

;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env]) : Value
  (type-case Exp a
    [trueE () (boolV #t)]
    [falseE () (boolV #f)]
    [equalE (l r)
            (let ([left (interp l env)]
                  [right (interp r env)])
              (if (and (numV? left) (numV? right))
                  (boolV (= (numV-n left) (numV-n right)))
                  (error 'equalE "not a number")))]
    [ifE (p t e)
         (let ([predicate (interp p env)])
               ;[then (interp t env)]
               ;[otherwise (interp e env)])
           (if (boolV? predicate)
               (if (boolV-b predicate) (interp t env) (interp e env))
               (error 'ifE "not a boolean")))]
    [numE (n) (numV n)]
    [idE (s) (lookup s env)]
    [plusE (l r) (num+ (interp l env) (interp r env))]
    [multE (l r) (num* (interp l env) (interp r env))]
    [letE (n rhs body) (interp body
                               (extend-env
                                (bind n (interp rhs env))
                                env))]
    [unletE (n body) (interp body (delete n env))]
    [lamE (n body) (closV n body env)]
    [appE (fun arg) (type-case Value (interp fun env)
                      [closV (n body c-env)
                       (interp body
                               (extend-env
                                (bind n
                                      (interp arg env))
                                c-env))]
                      [else (error 'interp "not a function")])]))

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
  (test (interp (parse `{* 2 1}) mt-env)
        (numV 2))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env)
        (numV 19))
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

  (test/exn (interp (parse `{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env)
            "free variable")

  #;
  (time (interp (parse `{let {[x2 {lambda {n} {+ n n}}]}
                          {let {[x4 {lambda {n} {x2 {x2 n}}}]}
                            {let {[x16 {lambda {n} {x4 {x4 n}}}]}
                              {let {[x256 {lambda {n} {x16 {x16 n}}}]}
                                {let {[x65536 {lambda {n} {x256 {x256 n}}}]}
                                  {x65536 1}}}}}})
                mt-env))
    (test (interp (parse `{if {= 2 {+ 1 1}} 7 8})
                mt-env)
        (interp (parse `7)
                mt-env))
  (test (interp (parse `{if false {+ 1 {lambda {x} x}} 9})
                mt-env)
        (interp (parse `9)
                mt-env))
  (test (interp (parse `{if true 10 {+ 1 {lambda {x} x}}})
                mt-env)
        (interp (parse `10)
                mt-env))
  (test/exn (interp (parse `{if 1 2 3})
                    mt-env)
            "not a boolean")
    (test/exn (interp (parse `{let {[x 1]}
                             {unlet x
                              x}})
                    mt-env)
            "free variable")
  (test (interp (parse `{let {[x 1]}
                          {+ x {unlet x 1}}})
                mt-env)
        (interp (parse `2) mt-env))
  (test (interp (parse `{let {[x 1]}
                          {let {[x 2]}
                            {+ x {unlet x x}}}})
                mt-env)
        (interp (parse `3) mt-env))
  (test (interp (parse `{let {[x 1]}
                          {let {[x 2]}
                            {let {[z 3]}
                              {+ x {unlet x {+ x z}}}}}})
                mt-env)
        (interp (parse `6) mt-env))
  (test (interp (parse `{let {[f {lambda {z}
                                   {let {[z 8]}
                                     {unlet z
                                       z}}}]}
                          {f 2}})
                mt-env)
        (interp (parse `2) mt-env)))

;; num+ and num* ----------------------------------------
(define (num-op [op : (number number -> number)] [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r))
    (numV (op (numV-n l) (numV-n r)))]
   [else
    (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num* [l : Value] [r : Value]) : Value
  (num-op * l r))

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6)))

;; lookup ----------------------------------------
(define (lookup [n : symbol] [env : Env]) : Value
  (cond
   [(empty? env) (error 'lookup "free variable")]
   [else (cond
                       [(symbol=? n (bind-name (first env)))
                        (bind-val (first env))]
                       [else (lookup n (rest env))])]))

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
