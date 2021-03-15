#lang plai-typed
;; https://my.eng.utah.edu/~cs3520/hw1.html
(define-type Tree
  [leaf (val : number)]
  [node (val : number)
        (left : Tree)
        (right : Tree)])

(define (sum [tree : Tree]) : number
  (type-case Tree tree
    [leaf (n) n]
    [node (n l r) (+(+ n (sum l)) (sum r))]))

(define (negate [tree : Tree]) : Tree
  (type-case Tree tree
    [leaf (n) (leaf (- 0 n))]
    [node (n l r) (node (- 0 n) (negate l) (negate r))]))

(define (contains? [tree : Tree] [k : number]) : boolean
  (type-case Tree tree
    [leaf (n) (= k n)]
    [node (n l r) (or (or (= k n) (contains? l k)) (contains? r k))]))

(define (big-leaves? [tree : Tree]) : boolean
  (letrec ([bigger-leaves?
         (lambda ([tree : Tree] [acc : number]) : boolean
           (type-case Tree tree
             [leaf (n) (> n acc)]
             [node (n l r) (and (bigger-leaves? l (+ acc n))
                                (bigger-leaves? r (+ acc n)))]))])
    (bigger-leaves? tree 0)))

(define (positive-trees? [trees : (listof Tree)]) : boolean
  (cond [(empty? trees) #t]
        [else (and (> (sum (first trees)) 0) (positive-trees? (rest trees)))]))
                          
    