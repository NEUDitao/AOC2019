#lang racket

(module+ test
  (require rackunit)
  (define test-file (open-input-string #<<TEST
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
TEST
                                       ))
  (check-equal? (part1 test-file) 42))

(define (file->orbits infile)
  (file-position infile 0)
  (for/hash ([line (in-port read-line infile)])  
    (match-let ([(list _ orbitee orbiter) (regexp-match #px"(\\w+)\\)(\\w+)" line)])
               (values orbiter orbitee))))

(define (total-orbits orbit-hash)
  (define memo (make-hash))
  (define (planet-orbits p)
    (when (not (hash-has-key? memo p))
      (if (hash-has-key? orbit-hash p)
          (hash-set! memo p (add1 (planet-orbits (hash-ref orbit-hash p))))
          (hash-set! memo p 0)))
    (hash-ref memo p))
  (for/sum ([planet (in-list (hash-keys orbit-hash))])
    (planet-orbits planet)))

(define (part1 file)
  (total-orbits (file->orbits file)))

(module+ test)

(define (file->graph infile)
  (file-position infile 0)
  (for/fold ([ans (hash)])
            ([line (in-port read-line infile)])
    (match-let ([(list _ orbitee orbiter) (regexp-match #px"(\\w+)\\)(\\w+)" line)])
      (hash-update
       (hash-update ans orbiter (curry cons orbitee) empty)
       orbitee
       (curry cons orbiter)
       empty))))

(define (distance-to-santa graph)
  (define (advance-from point-set)
    (for/fold ([ans point-set])
              ([planet (in-set point-set)])
      (set-union ans (list->set (hash-ref graph planet empty)))))
  
  (define (iter you-reached santa-reached k)
    (cond
      [(for/or ([planet (in-set you-reached)])
         (set-member? santa-reached planet))
        (- k 2)]
      [(even? k) (iter (advance-from you-reached) santa-reached (add1 k))]
      [(odd? k) (iter you-reached (advance-from santa-reached) (add1 k))]))

  (iter (set "YOU") (set "SAN") 0))
      
(define (part2 file)
  (distance-to-santa (file->graph file)))

(module+ main
  (define input (open-input-file "input.txt"))
  (displayln (part1 input))
  (displayln (part2 input))
  (close-input-port input))