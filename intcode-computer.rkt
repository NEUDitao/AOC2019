#lang racket

(provide intcode-computer)


;; An OpCode is one of:
;; 1 (adds the first 2 numbers and sets the index in the third number to that value)
;; 2 (does the same as 1 but multiplies it)
;; 3 (takes a single integer as input and saves it to address given by parameter)
;; 4 (outputs value of its only parameter at parameter)
;; 99 (ends the program)
;; And represents an instruction used for an IntCode

(define OP-CODE-ADD 1)
(define OP-CODE-MULTIPLY 2)
(define OP-CODE-TERMINATE 99)


;; An IntCode is a (list OpCode Number ...)
;; Where the first number is an OpCode
;; And the other three numbers dictate what the OpCode receives
;; and represents an instruction and parameters passed to it

(define INT-CODE-1 '(1 0 0 3))
(define INT-CODE-2 '(2 0 0 3))
(define INT-CODE-3 '(99 3 4 12))

;; intcode-computer: [List-of Number] -> Number
;; Executes IntCodes seen, producing the number at index 0

(define (intcode-computer lon)

  ;; update-value-in-lon: IntCode [List-of Number] [Number Number -> Number] -> Number
  (define (update-value-in-lon ic lon combiner)
    (list-set lon (fourth ic)
              (combiner (list-ref lon (second ic))
                        (list-ref lon (third ic)))))

  ;; singular-intcode: IntCode [List-of Number] Number -> Number
  (define (singular-intcode ic lon idx)
    (cond
      [(= (first ic) 1) (execute-intcode/idx (update-value-in-lon ic lon +) (add1 idx))]
      [(= (first ic) 2) (execute-intcode/idx (update-value-in-lon ic lon *) (add1 idx))]
      [(= (first ic) 99) (first lon)]))

  ;; execute-intcode/idx [List-of Number] Number -> Number
  (define (execute-intcode/idx lon idx)
    (singular-intcode (list-tail lon (* idx 4)) lon idx))
  
  (execute-intcode/idx lon 0))

