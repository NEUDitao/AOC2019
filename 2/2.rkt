#lang racket

(require 2htdp/batch-io
         "../intcode-computer.rkt")


;; I'm too lazy to learn actual CSV parsing, so here we are
(define INPUT (first (read-csv-file "input2.txt")))

(define INPUT-FOR-PART-1 (list-set (list-set (map string->number INPUT) 1 12) 2 2))


;; find-inputs-that-give-19690720: Number Number -> Void
;; Finds the values that, when they replace the first and second values of the input to
;; intcode-computer produce 19690720
(define (find-inputs-that-give-19690720 x y)
  (cond
    [(and (> x 99) (> y 99)) (error "could not find valid inputs")]
    [(> x 99) (find-inputs-that-give-19690720 0 (add1 y))]
    [else (if (= 19690720 (intcode-computer (list-set (list-set (map string->number INPUT) 1 x) 2 y)))
              (fprintf (current-output-port) "~a, ~a" x y)
              (find-inputs-that-give-19690720 (add1 x) y))]))
              