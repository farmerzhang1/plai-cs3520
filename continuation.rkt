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
  (multE [l : Exp]
         [r : Exp])
  (lamE [n : Symbol]
        [body : Exp])
  (appE [fun : Exp]
        [arg : Exp]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)

(define-type Cont
  (doneK)
  (plusSecondK [r : Exp]
               [e : Env]
               [k : Cont])
  (doPlusK [v : Value]
           [k : Cont])
  (multSecondK [r : Exp]
               [e : Env]
               [k : Cont])
  (doMultK [v : Value]
           [k : Cont])
  (appArgK [a : Exp]
           [env : Env]
           [k : Cont])
  (doAppK [f : Value]
          [k : Cont]))

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appE (lamE (s-exp->symbol (first bs))
                   (parse (third (s-exp->list s))))
             (parse (second bs))))]
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
        (appE (lamE 'x (idE 'y))
              (plusE (numE 1) (numE 2))))
  (test (parse `{lambda {x} 9})
        (lamE 'x (numE 9)))
  (test (parse `{double 9})
        (appE (idE 'double) (numE 9)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input"))

;; interp & continue ----------------------------------------
(define (interp [a : Exp] [env : Env] [k : Cont]) : Value
  (type-case Exp a
    [(numE n) (continue k (numV n))]
    [(idE s) (continue k (lookup s env))]
    [(plusE l r) (interp l env
                         (plusSecondK r env k))]
    [(multE l r) (interp l env
                         (multSecondK r env k))]
    [(lamE n body)
     (continue k (closV n body env))]
    [(appE fun arg) (interp fun env
                            (appArgK arg env k))]))
;; 我來用lambda（函數調用啦）解釋一下continuation到底是個什麼東西。
;; 首先呢，我們一次只處理一個東西，在這裡就是先interp fun，然後我們還剩下參數沒動啊，那他就是一個續體, with continuation (appArgK ...)
;; 這個續體就叫appArgK，我們再給他一些參數（其中的這個K，就是比較高層的函數的續體，我們調用完函數之後就會返回去計算）讓他構成一個Cont
;; 解釋lamE的時候我們會有continue K (closure ...)這裡把他想做繼續我們該算的東西，因為lamE已經解釋完了剩下的就是這個continuation
;; 那麼我們continue 這個東西，進來continue這個函數
;; 首先得解釋函數參數，那麼arg這個續體就算完了，還有一個continuation是函數調用
;; 所以又有doAppK (do apply K吧)
;; doAppK解釋之後就是繼續解釋別的東西了，（指next-k，與appE裡面的k相同）
(define (continue [k : Cont] [v : Value]) : Value
  (type-case Cont k
    [(doneK) v]
    [(plusSecondK r env next-k)
     (interp r env
             (doPlusK v next-k))]
    [(doPlusK v-l next-k)
     (continue next-k (num+ v-l v))]
    [(multSecondK r env next-k)
     (interp r env
             (doMultK v next-k))]
    [(doMultK v-l next-k)
     (continue next-k (num* v-l v))]
    [(appArgK a env next-k)
     (interp a env
             (doAppK v next-k))]
    [(doAppK v-f next-k)
     (type-case Value v-f
       [(closV n body c-env)
        (interp body
                (extend-env
                 (bind n v)
                 c-env)
                next-k)]
       [else (error 'interp "not a function")])]))

(module+ test
  (test (interp (parse `2) mt-env (doneK))
        (numV 2))
  (test/exn (interp (parse `x) mt-env (doneK))
            "free variable")
  (test (interp (parse `x)
                (extend-env (bind 'x (numV 9)) mt-env)
                (doneK))
        (numV 9))
  (test (interp (parse `{+ 2 1}) mt-env (doneK))
        (numV 3))
  (test (interp (parse `{* 2 1}) mt-env (doneK))
        (numV 2))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env
                (doneK))
        (numV 19))
  (test (interp (parse `{lambda {x} {+ x x}})
                mt-env
                (doneK))
        (closV 'x (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env
                (doneK))
        (numV 10))
  (test (interp (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env
                (doneK))
        (numV 12))
  (test (interp (parse `{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env
                (doneK))
        (numV 5))
  (test (interp (parse `{{lambda {x} {+ x x}} 8})
                mt-env
                (doneK))
        (numV 16))

  (test/exn (interp (parse `{1 2}) mt-env (doneK))
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env (doneK))
            "not a number")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env
                    (doneK))
            "free variable")
  ;; Eager:
  (test/exn (interp (parse `{{lambda {x} 0} {1 2}}) mt-env (doneK))
            "not a function")

  (test (continue (doneK) (numV 5))
        (numV 5))
  (test (continue (plusSecondK (numE 6) mt-env (doneK)) (numV 5))
        (numV 11))
  (test (continue (doPlusK (numV 7) (doneK)) (numV 5))
        (numV 12))
  (test (continue (multSecondK (numE 6) mt-env (doneK)) (numV 5))
        (numV 30))
  (test (continue (doMultK (numV 7) (doneK)) (numV 5))
        (numV 35))
  (test (continue (appArgK (numE 5) mt-env (doneK)) (closV 'x (idE 'x) mt-env))
        (numV 5))
  (test (continue (doAppK (closV 'x (idE 'x) mt-env) (doneK)) (numV 8))
        (numV 8)))

;; num+ and num* ----------------------------------------
(define (num-op [op : (Number Number -> Number)] [l : Value] [r : Value]) : Value
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
