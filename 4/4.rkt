#lang racket

(define RANGE-LOWER 248345)
(define RANGE-HIGHER 746315)

;; all-digits-increasing-for-six-digit-num?: Number -> Boolean
(define (all-digits-increasing-for-six-digit-num? n)

  ;; all-digits-increasing?/a: Number -> Boolean
  ;; ACCUMULATOR acc is the value of the last digit
  ;; ACCUMULATOR idx is the index that you're at in the number
  (define (all-digits-increasing?/a num acc idx)
    (cond
      [(= idx 0) #t]
      [else (and (<= (modulo num 10) acc)
                 (all-digits-increasing?/a (floor (/ num 10)) (modulo num 10) (sub1 idx)))]))

  (all-digits-increasing?/a n 9 6))



;; find-digits-that-satisfy-mass-double: Number Number -> [List-of Number]
(define (find-digits-that-satisfy-mass-double curr highest)
  (cond
    [(> curr highest) '()]
    [else (if (and (all-digits-increasing-for-six-digit-num? curr) (contains-double? curr))
              (cons curr (find-digits-that-satisfy-mass-double (add1 curr) highest))
              (find-digits-that-satisfy-mass-double (add1 curr) highest))]))

(length (find-digits-that-satisfy-mass-double RANGE-LOWER RANGE-HIGHER))


;; no-sequence-longer-than-2: Number -> Boolean
(define (contains-sequence-only-2 n)
  (define NUMBER-AS-LIST (string->list (number->string n)))
  (ormap (lambda (x) (= (count (lambda (y) (char=? y x)) NUMBER-AS-LIST) 2)) NUMBER-AS-LIST))


    

(length (filter contains-sequence-only-2 (find-digits-that-satisfy-mass-double RANGE-LOWER RANGE-HIGHER)))
