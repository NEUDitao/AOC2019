#lang racket

(require 2htdp/batch-io)

;; An OpCode is one of:
;; 1
;; 2
;; 99


;; An IntCode is a (list OpCode Number Number Number)
;; Where the first number is an OpCode
;; And the other three numbers dictate what the OpCode receives

(define INT-CODE-1 '(1 0 0 3))
(define INT-CODE-2 '(2 0 0 3))
(define INT-CODE-3 '(99 3 4 12))

;; I'm too lazy to learn actual CSV parsing, so here we are
(define INPUT (first (read-csv-file "input2.txt")))

(define INPUT-FOR-PART-1 (list-set (list-set (map string->number INPUT) 1 12) 2 2))

;; execute-intcode: [List-of Number] -> Number
;; Executes IntCodes seen, producing the number at index 0

(define (intcode-computer lon)

  (define (update-value-in-lon ic lon combiner)
    (list-set lon (fourth ic)
              (combiner (list-ref lon (second ic))
                        (list-ref lon (third ic)))))

  (define (singular-intcode ic lon idx)
    (cond
      [(= (first ic) 1) (execute-intcode/idx (update-value-in-lon ic lon +) (add1 idx))]
      [(= (first ic) 2) (execute-intcode/idx (update-value-in-lon ic lon *) (add1 idx))]
      [(= (first ic) 99) (first lon)]))
  
  (define (execute-intcode/idx lon idx)
    (singular-intcode (list-tail lon (* idx 4)) lon idx))
  
  (execute-intcode/idx lon 0))

(intcode-computer INPUT-FOR-PART-1)