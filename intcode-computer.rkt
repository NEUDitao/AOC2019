#lang racket

;; and IntCode Computer as specified at https://adventofcode.com/2019/day/9

(provide intcode-computer
         intcode-computer-with-index/base
         (struct-out output-pair)
         do-intcode-to-end)


;; An OpCode is one of:
;; 1 (adds the first 2 numbers and sets the index in the third number to that value)
;; 2 (does the same as 1 but multiplies it)
;; 3 (takes a single integer as input and saves it to address given by parameter)
;; 4 (outputs value of its only parameter at parameter)
;; 5 (jumps to parameter if true)
;; 6 (jumps to parameter if false)
;; 7 (less than produces 1, else 0)
;; 8 (equal produces 1, else 0)
;; 99 (ends the program)
;; And represents an instruction used for an IntCode

(define OP-CODE-ADD 1)
(define OP-CODE-MULTIPLY 2)
(define OP-CODE-INPUT 3)
(define OP-CODE-OUTPUT 4)
(define OP-CODE-JUMP-IF-TRUE 5)
(define OP-CODE-JUMP-IF-FALSE 6)
(define OP-CODE-LESS-THAN 7)
(define OP-CODE-EQUAL-TO 8)
(define OP-CODE-RELATIVE-BASE 9)
(define OP-CODE-TERMINATE 99)

;; An IntCode is a (list OpCode Number ...)
;; Where the first number is an OpCode
;; And the other three numbers are the parameters passed in

(define INT-CODE-1 '(1 0 0 3))
(define INT-CODE-2 '(2 0 0 3))
(define INT-CODE-3 '(99 3 4 12))

;; A ParameterMode is one of:
;; 0 (positional mode)
;; 1 (value mode)
;; and represent how parameteres are handled in the IntCode computer

(define POSITIONAL 0)
(define VALUE 1)
(define RELATIVE 2)


(struct parameter-value (mode value) #:transparent)
;; A ParameterValue is a (parameter-value ParameterMode Number)
;; and represents the parametermode of an opcode and the value at the parameter it corresponds to

(define PV1 (parameter-value 1 33))
(define PV2 (parameter-value 0 14))


(struct parameter-value-triplet (param-1 param-2 stored-at) #:transparent)
;; a ParameterValueTriplet is a (parameter-value-triplet ParameterValue ParameterValue Number)
;; and represents the values that will be acted on the ParameterValue in stored-at

(struct output-pair (output fields) #:transparent)
;; An Output Pair is a (output-pair Number [List [List-of Number] [List-of Number] Number])
;; and represents the output from an out put instruction, and the fields that can be sent back
;; to the intcode computer

#; {[List-of Number] [List-of Number] Number -> Number}
;; Executes IntCodes seen, producing the number at index 0, using the inputs given

(define (intcode-computer-with-index/base lon inputs index base)
  
  ;; update-value-in-lon: ParameterValueTriplet [List-of Number] [Number Number -> Number] -> Number
  ;; Updates the value in the original-list based on the pvt
  (define (update-value-in-lon pvt updater)
    (define updated-value (updater (get-value-of-pv (parameter-value-triplet-param-1 pvt))
                                 (get-value-of-pv (parameter-value-triplet-param-2 pvt))))
    (define location (parameter-value-triplet-stored-at pvt))
    (ensure-big-enough location)
    (set! lon (list-set lon location updated-value)))
  
  #; {ParameterValueTriplet [List-of Number] [Number Number -> Boolean] -> Number}
  ;; Updates the value pointed at by the PVT to be 1 if ? is true, else 0
  (define (update-value-off-question pvt ?)
    (define updated-value
      (if (? (get-value-of-pv (parameter-value-triplet-param-1 pvt))
             (get-value-of-pv (parameter-value-triplet-param-2 pvt))) 1 0))
    (define location (parameter-value-triplet-stored-at pvt))
    (ensure-big-enough location)
    (set! lon (list-set lon location updated-value)))

  #; {ParameterValueTriplet -> Number}
  ;; Ensures lon is big enough to handle size, else makes it bigger
  ;; gives back what size was referring to in lon
  (define (ensure-big-enough size)
      (if (> size (length lon))
          (and (set! lon (append lon (build-0-list size))) (list-ref lon size))
          (list-ref lon size)))


  ;; get-value-of-pv: ParameterValue -> Number
  ;; Gets the value of a ParameterValue, based on what mode it's in 
  (define (get-value-of-pv pv)
    (define pv-value (parameter-value-value pv))
    (define mode (parameter-value-mode pv))
    (cond
      [(= mode POSITIONAL) (ensure-big-enough pv-value)]
      [(= mode VALUE) pv-value]
      [(= mode RELATIVE) (ensure-big-enough (+ pv-value base))]))


  ;; create-pvt: IntCode -> ParameterValueTriplet
  ;; creates a pvt based on the IntCode
  (define (create-pvt ic)
    (define OP-CODE (first ic))
    (define third-pv (create-pv ic 3))
    (define position (if (= (parameter-value-mode third-pv) 2) (+ base (fourth ic)) (fourth ic)))
    (parameter-value-triplet (create-pv ic 1) (create-pv ic 2) position))

  ;; create-pv: IntCode Number -> ParameterValue
  (define (create-pv ic param-index)
    (define OP-CODE (first ic))
    (define MODULO-AMT (add1 param-index))
    (parameter-value (modulo (floor (/ OP-CODE (expt 10 MODULO-AMT))) 10) (list-ref ic param-index)))

  ;; jump-if-true: IntCode [List-of Number] Number -> Number
  (define (jump-if-true ic idx)
    (jump-on-conditional ic idx (not (zero? (get-value-of-pv (create-pv ic 1))))))

  ;; jump-if-false: IntCode [List-of Number] Number -> Number
  (define (jump-if-false ic idx)
    (jump-on-conditional ic idx (zero? (get-value-of-pv (create-pv ic 1)))))

  #; {IntCode Number Boolean -> Number}
  (define (jump-on-conditional ic idx ?)
    (if ? (execute-intcode/idx (get-value-of-pv (create-pv ic 2)))
        (execute-intcode/idx (+ 3 idx))))

  
  #; { IntCode -> Void}
  ;; SIDE EFFECT! changes the lon passed around
  (define (set-intcode-input! ic)
    (define FIRST (first inputs))
    (define REST (set! inputs (rest inputs)))
    (set! lon (list-set lon (offset-for-ic ic) FIRST)))

  #; {IntCode Number -> OutputPair}
  ;; outputs specified parameter & gives fields needed to re-run
  (define (intcode-output ic idx)
    (output-pair (list-ref lon (offset-for-ic ic))
                 `(,lon ,inputs ,(+ 2 idx) ,base)))

  
  #; {IntCode -> Number}
  (define (offset-for-ic ic)
    (define parameter-mode (parameter-value-mode (create-pv ic 1)))
    (define offset (if (= parameter-mode 2) (+ base (second ic)) (second ic)))
    (ensure-big-enough offset)
    offset)
      

  #; {IntCode Number -> Number}
  (define (singular-intcode ic idx)
    (define OPERATION (modulo (first ic) 100))
    (define INCREMENT-THREE-PARAM (+ 4 idx))
    (define INCREMENT-ONE-PARAM (+ 2 idx))
    (cond
      [(= OPERATION OP-CODE-ADD) (update-value-in-lon (create-pvt ic) +)
                                 (execute-intcode/idx INCREMENT-THREE-PARAM)]
      [(= OPERATION OP-CODE-MULTIPLY) (update-value-in-lon (create-pvt ic) *)
                                      (execute-intcode/idx INCREMENT-THREE-PARAM)]
      [(= OPERATION OP-CODE-INPUT) (set-intcode-input! ic) (execute-intcode/idx  INCREMENT-ONE-PARAM)]
      [(= OPERATION OP-CODE-OUTPUT) (intcode-output ic idx)]
      [(= OPERATION OP-CODE-JUMP-IF-TRUE) (jump-if-true ic idx)] 
      [(= OPERATION OP-CODE-JUMP-IF-FALSE) (jump-if-false ic idx)]
      [(= OPERATION OP-CODE-LESS-THAN) (update-value-off-question (create-pvt ic) <)
                                       (execute-intcode/idx INCREMENT-THREE-PARAM)]
      [(= OPERATION OP-CODE-EQUAL-TO) (update-value-off-question (create-pvt ic) =)
                                      (execute-intcode/idx INCREMENT-THREE-PARAM)]
      [(= OPERATION OP-CODE-RELATIVE-BASE)
       (set! base (+ base (get-value-of-pv (create-pv ic 1))))
       (execute-intcode/idx INCREMENT-ONE-PARAM)]
      [(= OPERATION OP-CODE-TERMINATE) (first lon)]))



  #; {Number -> Number}
  (define (execute-intcode/idx idx)
    (singular-intcode (list-tail lon idx) idx))

  (execute-intcode/idx index))


;; intcode-computer: [List-of Number] [List-of Number] -> [Pair Number]
(define (intcode-computer lon inputs)
  (intcode-computer-with-index/base lon inputs 0 0))

#; {Number -> [List-of Number]}
;; builds a list of length len with all 0's
(define (build-0-list len)
  (build-list len (lambda (x) 0)))

#; {[List [List-of Number] [List-of Number] Number] -> Number}
;; Takes the program and input list, goes until the end of a program
(define (do-intcode-to-end params)
  (define (do-intcode-to-end-with-all-fields params)
    (define OUTPUT (apply intcode-computer-with-index/base params))
    (cond
      [(output-pair? OUTPUT) (cons (output-pair-output OUTPUT)
                                   (do-intcode-to-end-with-all-fields (output-pair-fields OUTPUT)))]
      [else OUTPUT]))
  (do-intcode-to-end-with-all-fields (append params '(0 0))))

