#lang racket

(struct treenode [data children] #:transparent)

;; A TreeNode is a (treenode String [List-of TreeNode])
;; and represents a tree with infinite children

(define INPUT (map (lambda (x) (string-split x ")")) (file->lines "input.txt")))
(define START-POINT "COM")


;; create-tree: String [List-of [List-of String String]] -> TreeNode
(define (create-tree start-point input)
  (define STARTING-TREE (treenode start-point '()))
  (define QUEUE (filter (lambda (x) (string=? start-point (first x))) input))

  ;; create-tree-bfs: TreeNode [List-of String] -> TreeNode
  (define (create-tree-bfs tree queue)
    (cond
      [(empty? queue) tree]
      [else
       (define FIRST (first queue))
       (define CHILDREN (filter (lambda (x) (string=? (second FIRST) (first x))) input))
       (define FUTURE-QUEUE (append (rest queue) CHILDREN))
       (create-tree-bfs (add-node-to-tree (second FIRST) (first FIRST) tree) FUTURE-QUEUE)]))

  (create-tree-bfs STARTING-TREE QUEUE))

;; add-node-to-tree: String String TreeNode -> TreeNode
(define (add-node-to-tree node parent tree)
  (if (string=? (treenode-data tree) parent)
      (treenode (treenode-data tree) (cons (treenode node '()) (treenode-children tree)))
      (treenode (treenode-data tree)
                (map (lambda (x) (add-node-to-tree node parent x)) (treenode-children tree)))))

(define TREE (create-tree START-POINT INPUT))


(define (count-orbits tree)
  (+ 
     (foldr (lambda (x y) (+ 1 (count-orbits x) y)) 0 (treenode-children tree))))


(module+ test
  (require rackunit)
  (define test-file (map (lambda (x) (string-split x ")")) '(
"COM)B"
"B)C"
"C)D"
"D)E"
"E)F"
"B)G"
"G)H"
"D)I"
"E)J"
"J)K"
"K)L")))
  (check-equal? (count-orbits (create-tree START-POINT test-file)) 42))


  
        