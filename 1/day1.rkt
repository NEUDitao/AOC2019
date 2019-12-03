#lang racket

(require rackunit)

(define INPUT1 (open-input-file "input1.txt"))



;; input->mass/module-sum: InputPort -> Number
;; reads every line from the input port, and sums up their mass->module

#;(module+ test
  (test-case "input->mass/module-sum"
             (check-equal? (input->mass/module-sum INPUT1) 3367126)))

(define (input->mass/module-sum ip)
  (define MASS-ON-LINE (read ip))
  (cond
    [(eof-object? MASS-ON-LINE) 0]
    [else (+ (mass->module MASS-ON-LINE) (input->mass/module-sum ip))]))



;; mass->module: Number -> Number
;; produces the fuel requirement for a module as described https://adventofcode.com/2019/day/1

(module+ test
  (test-case "mass->module test"
             (check-equal? (mass->module 14) 2)
             (check-equal? (mass->module 1969) 654)
             (check-equal? (mass->module 100756) 33583)))

(define (mass->module mass)
  (- (floor (/ mass 3)) 2))


;; input->mass/module-sum-with-fuel: InputPort -> Number
;; reads every line from the input port, and sums up mass->module, accounting for the fuel required
;; for fuel

(define (input->mass/module-sum-with-fuel ip)
  (define MASS-ON-LINE (read ip))
  (cond
    [(eof-object? MASS-ON-LINE) 0]
    [else (+ (mass->module/acc MASS-ON-LINE) (input->mass/module-sum-with-fuel ip))]))

;; mass->module/acc: Number -> Number
;; calculates the mass->module, taking into account fuel for fuel

(module+ test
  (test-case "mass->module/acc"
             (check-equal? (mass->module/acc 14) 2)
             (check-equal? (mass->module/acc 1969) 966)
             (check-equal? (mass->module/acc 100756) 50346)))

(define (mass->module/acc mass)
  (define FUEL-FROM-MASS (mass->module mass))
  (cond
    [(< FUEL-FROM-MASS 0) 0]
    [else (+ FUEL-FROM-MASS (mass->module/acc FUEL-FROM-MASS))]))