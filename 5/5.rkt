#lang racket


(require 2htdp/batch-io
         "../intcode-computer.rkt")


;; I'm too lazy to learn actual CSV parsing, so here we are
(define INPUT (map string->number (first (read-csv-file "input.txt"))))

(displayln (cadddr (cdddr (cdddr (do-intcode-to-end `(,INPUT (1) 0))))))
(displayln (car (do-intcode-to-end `(,INPUT (5) 0))))