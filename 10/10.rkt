#lang racket

(define INPUT (file->lines "input.txt"))


#; {String -> [List-of Number]}
(define (turn-string-to-indices str)

  (define (turn-string-to-index/a str idx)
    (cond
      [(empty? str) '()]
      [else (if (string=? (first str) "#")
                (cons idx (turn-string-to-index/a (rest str) (add1 idx)))
                (turn-string-to-index/a (rest str) (add1 idx)))]))

  (turn-string-to-index/a (string-split str "") -1))

(define MAP (for/list ([i INPUT]) (turn-string-to-indices i)))
