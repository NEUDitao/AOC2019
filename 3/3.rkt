#lang racket

(require 2htdp/batch-io
         rackunit
         racket/set)

;; A Direction is one of:
;; U
;; D
;; L
;; R
;; and represents either Up, Down, Left, or Right

(struct relative-posn (dir amount))
;; A RelativePosn is a (relative-posn Direction Number)
;; and represents the direction the relative position makes, and how far it will go


(define-struct posn (x y) #:transparent)
;; A Posn is a (posn Number Number)
;; and represents a point on a 2d grid

;; los->lorp: [List-of String] -> [List-of RelativePosn]

(define (los->lorp los)
  (map
   (λ (str)
     (relative-posn (substring str 0 1)
                         (string->number (string-trim (substring str 1 (string-length str))))))
     los))

;; read-line-and-split-commas: -> [List-of String]
(define (read-line-and-split-commas)
  (string-split (read-line INPUT) ","))


(define INPUT (open-input-file "3.txt"))

(define WIRE-1 (los->lorp (read-line-and-split-commas)))
(define WIRE-2 (los->lorp (read-line-and-split-commas)))



;; manhattan-distance: Posn Posn -> Number
(define (manhattan-distance p1 p2)
   (+ (abs (- (posn-x p1) (posn-x p2))) (abs (- (posn-y p1) (posn-y p2)))))


;; lorp->lop [List-of RelativePosn] -> [List-of Posn]

(module+ test
  (test-case "lorp->lop test"
             (check-equal? `(,(posn 0 0)) (lorp->lop '()))
             (check-equal? `(,(posn 0 0) ,(posn 1 0) ,(posn 2 0))
                           (lorp->lop `(,(relative-posn "R" 2))))))

(define (lorp->lop lorp)

  ;; RelativePosn Posn -> [List-of Posn]
  ;; ACCUMULATOR: prev-posn keeps track of where you were before
  (define (create-singular-direction-lop rel-posn prev-posn)
    (cond
      [(= (relative-posn-amount rel-posn) 0) '()]
      [else
       (define NEXT-POSN (make-singular-posn (relative-posn-dir rel-posn) prev-posn))
       (cons NEXT-POSN (create-singular-direction-lop (relative-posn (relative-posn-dir rel-posn)
                                                                          (sub1 (relative-posn-amount rel-posn))) NEXT-POSN))]))

  ;; Direction Posn -> Posn
  (define (make-singular-posn dir prev-posn)
    (cond
      [(string=? dir "U") (posn (posn-x prev-posn) (add1 (posn-y prev-posn)))]
      [(string=? dir "D") (posn (posn-x prev-posn) (sub1 (posn-y prev-posn)))]
      [(string=? dir "L") (posn (sub1 (posn-x prev-posn)) (posn-y prev-posn))]
      [(string=? dir "R") (posn (add1 (posn-x prev-posn)) (posn-y prev-posn))]))

  (foldl
   (λ (rp lop) (append lop (create-singular-direction-lop rp (last lop))))
   (list (posn 0 0)) lorp))


(define WIRE-1-AS-POSN (lorp->lop WIRE-1))
(define WIRE-2-AS-POSN (lorp->lop WIRE-2))
(define INTERSECTIONS (remove (posn 0 0)
                              (set->list (set-intersect (list->set WIRE-1-AS-POSN)
                                                        (list->set WIRE-2-AS-POSN)))))

(manhattan-distance (argmin (λ (dis) (manhattan-distance dis (posn 0 0))) INTERSECTIONS) (posn 0 0))

(argmin min (map (lambda (x) (+ (index-of WIRE-1-AS-POSN x) (index-of WIRE-2-AS-POSN x)))
                 INTERSECTIONS))