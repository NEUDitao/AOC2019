#lang racket


(require 2htdp/batch-io
         "../intcode-computer.rkt")


;; I'm too lazy to learn actual CSV parsing, so here we are
(define INPUT (map string->number (first (read-csv-file "input.txt"))))

(define ALL-COMBINATIONS (map (lambda (x) (cons 0 x))
                              (permutations '(4 3 2 1 0))))

(define ALL-COMBINATIONS-FEEDBACK (map (lambda (x) (cons 0 x))
                                       (permutations '(9 8 7 6 5))))


;; perform-singular-permutation: [NEList-of Number] -> Number
;; performs a single set of permutations for an amplifier's program

(module+ test
  (require rackunit)
  (test-case "part 1 tests"
             (check-equal?
              (perform-singular-permutation `(0,4,3,2,1,0)
                                            `(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0))
                           43210)
             (check-equal?
              (perform-singular-permutation `(0,0,1,2,3,4)
                                            `(3,23,3,24,1002,24,10,24,1002,23,-1,23,
                                               101,5,23,23,1,24,23,23,4,23,99,0,0))
              54321)))

;; 
(define (perform-singular-permutation perm ip)
  ;; (displayln perm)
  (cond
    [(empty? (rest perm)) (first perm)]
    [else
     (define INPUTS (list (second perm) (first perm)))
     (perform-singular-permutation (cons (car (intcode-computer ip INPUTS))
                                              (rest (rest perm))) ip)]))


(displayln (perform-singular-permutation
            (argmax (lambda (x) (perform-singular-permutation x INPUT)) ALL-COMBINATIONS)
            INPUT))