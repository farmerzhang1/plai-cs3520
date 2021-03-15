#lang plai-typed
(print-only-errors #t)

;robot can either turn left 90 deg or right 90 deg or go forward some # of feet

(module+ test
  (test (distance-moved empty)
        0)
  (test (distance-moved (cons (forward 10) empty))
        10)
  (test (distance-moved (cons (forward 15) (cons (forward 10) empty)))
        25)
  )

(define-type Robot-Instruction
  [left]
  [right]
  [forward [feet : Number]])

;; Template for a function that takes a Robot-Instruction:

;(define (forward-distance [inst : Robot-Instruction]) : Number
;  (type-case Robot-Instruction inst
;    [(left) ...]
;    [(right) ...]
;    [(forward ft) ...ft...]
;    ))

(define (forward-distance [inst : Robot-Instruction]) : Number
  (type-case Robot-Instruction inst
    [left ()0]
    [right ()0]
    [forward (ft) ft]
    ))

(module+ test
  (test (forward-distance (left))
        0)
  (test (forward-distance (right))
        0)
  (test (forward-distance (forward 10))
        10)
   )


;; Representation for a robot program:

#;
(define-type (Listof Robot-Instruction)
  empty
  [cons (first :  Robot-Instruction)
        (rest : Listof Robot-Instruction)]
  )

;Template for a function that takes a robot program
#;
(define (distance-moved [program : (Listof Robot-Instruction)])
  (type-case (Listof Robot-Instruction) program
    [empty ...]
    [(cons first rst) ... (forward-distance first) ... (distance-moved rst) ...]))


(define (distance-moved [program : (Listof Robot-Instruction)])
  (type-case (Listof Robot-Instruction) program
    [empty 0]
    [(cons first rst) (+ (forward-distance first) (distance-moved rst))]))


(define (isturn? [inst : Robot-Instruction])
  (type-case Robot-Instruction inst
    [(left) #t]
    [(right) #t]
    [(forward ft) #f]
    )
  )

(module+ test
  (test (isturn? (left))
        #t)
  (test (isturn? (right))
        #t)
  (test (isturn? (forward 10))
        #f)
  )


(define (turns? [program : (Listof Robot-Instruction)]) : Boolean
  (type-case (Listof Robot-Instruction) program
    [empty #f]
    [(cons first rst) (or (isturn? first) (turns? rst))]))


(module+ test
  (test (turns? empty)
        #f)
  (test (turns? (cons (left) empty))
        #t)
  (test (turns? (cons (forward 10) empty))
        #f)
  (test (turns? (cons (left) (cons (forward 10) empty)))
        #t)
  )