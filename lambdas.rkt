#lang racket

(define test (lambda (f)
               (lambda (x)
                 (f 10))))

(test (lambda (y) (+ x y)))