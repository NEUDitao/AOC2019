#lang racket


(require 2htdp/batch-io
         "../intcode-computer.rkt")


;; I'm too lazy to learn actual CSV parsing, so here we are
(define INPUT (map string->number (first (read-csv-file "input.txt"))))

(define (do-5 params)
  (define OUTPUT (apply intcode-computer-with-index params))
  (cond
    [(output-pair? OUTPUT) (cons (output-pair-output OUTPUT) (do-5 (output-pair-fields OUTPUT)))]
    [else OUTPUT]))

(displayln (cadddr (cdddr (cdddr (do-5 `(,INPUT (1) 0))))))
(displayln (car (do-5 `(,INPUT (5) 0))))