#lang racket


(require 2htdp/batch-io
         "../intcode-computer.rkt")


;; I'm too lazy to learn actual CSV parsing, so here we are
(define INPUT (map string->number (first (read-csv-file "input.txt"))))

(define ALL-COMBINATIONS (map (lambda (x) (cons 0 x)) (permutations '(4 3 2 1 0))))

