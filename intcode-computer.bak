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
(define OP-CODE-INPUT 3)
(define OP-CODE-OUTPUT 4)
(define OP-CODE-JUMP-IF-TRUE 5)
(define OP-CODE-JUMP-IF-FALSE 6)
(define OP-CODE-LESS-THAN 7)
(define OP-CODE-GREATER-THAN 8)
(define OP-CODE-TERMINATE 99)




(struct op-code-param-val (param code) #:transparent)
;; An OpCodeParamVal is a (op-code-param-val [List-of Param] OpCode)


;; An IntCode is a (list OpCode Number ...)
;; Where the first number is an OpCoed
;; And the other three numbers dictate what the OpCode receives
;; and represents an instruction and parameters passed to it

(define INT-CODE-1 '(1 0 0 3))
(define INT-CODE-2 '(2 0 0 3))
(define INT-CODE-3 '(99 3 4 12))

;; A ParameterMode is one of:
;; 0 (positional mode)
;; 1 (value mode)
;; and represent how parameteres are handled in the IntCode computer

(define POSITIONAL 0)
(define VALUE 1)


(struct parameter-value (mode value) #:transparent)
;; A ParameterValue is a (parameter-value ParameterMode Number)
;; and represents the prametermode of an opcode and the value at the parameter it correspodns to

(define PV1 (parameter-value 1 33))
(define PV2 (parameter-value 0 14))


(struct parameter-value-triplet (param-1 param-2 stored-at) #:transparent)
;; a ParameterValueTriplet is a (parameter-value-triplet ParameterValue ParameterValue Number)
;; and represents the values that will be acted on the ParameterValue in stored-at

;; intcode-computer: [List-of Number] -> Number
;; Executes IntCodes seen, producing the number at index 0

(define (intcode-computer lon)

  ;; update-value-in-lon: ParameterValueTriplet [List-of Number] [Number Number -> Number] -> Number
  ;; Updates the value in the original-list based on the pvt
  (define (update-value-in-lon pvt og-lon combiner)
    (list-set og-lon (parameter-value-triplet-stored-at pvt)
              (combiner (get-value-of-pv (parameter-value-triplet-param-1 pvt) og-lon)
                        (get-value-of-pv (parameter-value-triplet-param-2 pvt) og-lon))))

  ;; update-value-off-question ParameterValueTriplet [List-of Number] [Number Number -> Boolean] -> Number
  (define (update-value-off-question pvt og-lon question)
    (define UPDATED-VALUE (if (question (get-value-of-pv (parameter-value-triplet-param-1 pvt) og-lon)
                                        (get-value-of-pv (parameter-value-triplet-param-2 pvt) og-lon))
                              1 0))
    (list-set og-lon (parameter-value-triplet-stored-at pvt) UPDATED-VALUE))

  ;; get-value-of-pv: ParameterValue -> Number
  ;; Gets the value of a ParameterValue, based on what mode it's in 
  (define (get-value-of-pv pv og-lon)
    (cond
      [(= (parameter-value-mode pv) POSITIONAL) (list-ref og-lon (parameter-value-value pv))]
      [(= (parameter-value-mode pv) VALUE) (parameter-value-value pv)]))


  ;; create-pvt: IntCode -> ParameterValueTriplet
  ;; creates a pvt based on the IntCode
  (define (create-pvt ic)
    (define OP-CODE (first ic))
    (parameter-value-triplet (create-pv ic 1)
                             (create-pv ic 2)
                             (fourth ic)))

  ;; create-pv: IntCode Number -> ParameterValue
  (define (create-pv ic param-index)
    (define OP-CODE (first ic))
    (define MODULO-AMT (add1 param-index))
    (parameter-value (modulo (floor (/ OP-CODE (expt 10 MODULO-AMT))) 10) (list-ref ic param-index)))

  ;; jump-if-true: IntCode [List-of Number] Number -> Number
  (define (jump-if-true ic lon idx)
    (if (not (zero? (get-value-of-pv (create-pv ic 1) lon))) ;; HAVE TO GET PROPER VALUE
        (execute-intcode/idx lon (get-value-of-pv (create-pv ic 2) lon))
        (execute-intcode/idx lon (+ 3 idx))))

  ;; jump-if-false: IntCode [List-of Number] Number -> Number
  (define (jump-if-false ic lon idx)
    #;(printf "nex idx ~a~a\nzero?~a\n" (+ idx 2) (create-pv ic (+ idx 2)) (zero? (get-value-of-pv (create-pv ic 1) lon)))
      (if (zero? (get-value-of-pv (create-pv ic 1) lon))
        (execute-intcode/idx lon (get-value-of-pv (create-pv ic 2) lon))
        (execute-intcode/idx lon (+ 3 idx))))
      

  ;; singular-intcode: IntCode [List-of Number] Number -> Number
  (define (singular-intcode ic og-lon idx)
    (define OPERATION (modulo (first ic) 100))
    (define INCREMENT-THREE-PARAM (+ 4 idx))
    (define INCREMENT-ONE-PARAM (+ 2 idx))
    #;(printf "op ~a\n" OPERATION)
    #;(printf "whole first ~a\n" (first ic))
    #;(printf "idx ~a\n" idx)
    (cond
      [(= OPERATION OP-CODE-ADD) (execute-intcode/idx (update-value-in-lon (create-pvt ic) og-lon +) INCREMENT-THREE-PARAM)]
      [(= OPERATION OP-CODE-MULTIPLY) (execute-intcode/idx (update-value-in-lon (create-pvt ic) og-lon *) INCREMENT-THREE-PARAM)]
      [(= OPERATION OP-CODE-INPUT) (execute-intcode/idx (intcode-input ic og-lon) INCREMENT-ONE-PARAM)]
      [(= OPERATION OP-CODE-OUTPUT) (execute-intcode/idx (intcode-output ic og-lon) INCREMENT-ONE-PARAM)]
      [(= OPERATION OP-CODE-JUMP-IF-TRUE) (jump-if-true ic og-lon idx)] 
      [(= OPERATION OP-CODE-JUMP-IF-FALSE) (jump-if-false ic og-lon idx)]
      [(= OPERATION OP-CODE-LESS-THAN) (execute-intcode/idx (update-value-off-question (create-pvt ic) og-lon <) INCREMENT-THREE-PARAM)]
      [(= OPERATION OP-CODE-GREATER-THAN) (execute-intcode/idx (update-value-off-question (create-pvt ic) og-lon =) INCREMENT-THREE-PARAM)]
      [(= OPERATION OP-CODE-TERMINATE) (first og-lon)]))


  ;; intcode-input: IntCode [List-of Number] -> [List-of Number]
  (define (intcode-input ic lon)
    (list-set lon (second ic) (string->number (read-line (current-input-port) 'any))))

  ;; intcode-output: IntCode [List-of Number] -> [List-of Number]
  (define (intcode-output ic lon)
    (displayln (list-ref lon (second ic)))
    lon)

  ;; execute-intcode/idx [List-of Number] Number -> Number
  (define (execute-intcode/idx og-lon idx)
    #;(displayln "here")
    (singular-intcode (list-tail og-lon idx) og-lon idx))
  
  (execute-intcode/idx lon 0))

