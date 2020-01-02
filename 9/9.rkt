#lang racket

(require 2htdp/batch-io
         "../intcode-computer.rkt")


;; I'm too lazy to learn actual CSV parsing, so here we are
(define INPUT (map string->number (first (read-csv-file "input.txt"))))

(intcode-computer `(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99) '())
;;(intcode-computer INPUT '(1))