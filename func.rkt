#lang plait

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]  ;标识符的定义
  [appC (fun : symbol) (arg : ExprC)]  ;调用的定义
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define (findit [fun-defs : (listof FunDefC)] [fun-name : symbol])
  (cond [(empty? fun-defs) (error 'findit "undefined function")]
        [else
         (cond [(equal? fun-name (fdC-name (first fun-defs))) (first fun-defs)]
               [else (findit (rest fun-defs) fun-name)])]))

(define (subst [to-replace : symbol] [with : ExprC] [in-this : ExprC])
  (type-case ExprC in-this
    [(numC n) in-this]
    [plusC (l r) (plusC (subst to-replace with l) (subst to-replace with r))]
    [multC (l r) (multC (subst to-replace with l) (subst to-replace with r))]
    [idC (s) (cond [(equal? s to-replace) with]
                   [else in-this])]
    [appC (fun arg) (appC fun (subst to-replace with arg))]))

(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]
    [appC (fun arg)
          (let ([myFunc (findit fds fun)])
            (interp (subst (fdC-arg myFunc)
                           (numC (interp arg fds))
                           (fdC-body myFunc)) fds))]
    [idC (s) (error 'interp "shouldn't get here")]))