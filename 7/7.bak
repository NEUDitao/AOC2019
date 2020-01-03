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

#;(module+ test
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


(define (perform-singular-permutation perm ip)
  ;; (displayln perm)
  (cond
    [(empty? (rest perm)) (first perm)]
    [else
     (define INPUTS (list (second perm) (first perm)))
     (perform-singular-permutation (cons (output-pair-output (intcode-computer ip INPUTS))
                                         (rest (rest perm))) ip)]))


#;(displayln (perform-singular-permutation
            (argmax (lambda (x) (perform-singular-permutation x INPUT)) ALL-COMBINATIONS)
            INPUT))

(define INIT-INPUT (build-list 5 (lambda (x) (output-pair 0 `(,INPUT (0) 0)))))


(define (five-thrusters amps idx input-queue)
  (define curr-idx (modulo idx 5))
  (define curr-amp (list-ref amps curr-idx))

  (define (do-one-op-and-move-on)
    (define apply-input (list-set (output-pair-fields curr-amp)
                                  1 (list (first input-queue))))
    (define new-comp (apply intcode-computer-with-index apply-input))

    (cond
      [(number? new-comp) (first input-queue)]
      [else (five-thrusters (list-set amps curr-idx new-comp) (add1 idx) (cons (output-pair-output new-comp) (rest input-queue)))]))

  (define (do-beginning-op)
    (define apply-input (list-set (output-pair-fields curr-amp)
                                  1 (list (second input-queue) (first input-queue))))
    (define new-comp (apply intcode-computer-with-index apply-input))
    (five-thrusters (list-set amps curr-idx new-comp) (add1 idx) (cons (output-pair-output new-comp) (rest (rest input-queue)))))
  
  (cond
    [(= (length input-queue) 1) (do-one-op-and-move-on)]
    [else (do-beginning-op)])

  
  
  #;(define prev-computation (list-ref amps (modulo (sub1 idx) 5)))
  #;(define apply-input (list-set (output-pair-fields prev-computation)
                                1 (list (output-pair-output prev-computation))))
  #;(define new-comp (apply intcode-computer-with-index apply-input))
  #;(cond
    [(pair? new-comp) 'e]
    [else (five-thrusters (list-set amps idx new-comp) (add1 idx))]
    )
  )

(five-thrusters (build-list 5 (lambda (x) (output-pair 0 `((3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5) () 0)))) 0 '(0 9 8 7 6 5))

(five-thrusters (build-list 5 (lambda (x) (output-pair 0 `((3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10) () 0)))) 0 '(0 9 7 8 5 6))
