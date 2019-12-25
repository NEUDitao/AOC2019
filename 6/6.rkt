#lang racket

(struct treenode [data children] #:transparent)

;; A TreeNode is a (treenode String [List-of TreeNode])
;; and represents a tree with infinite children

(define INPUT (map (lambda (x) (string-split x ")")) (file->lines "input.txt")))
(define START-POINT "COM")
(define TEST-LIST '("COM)B"
                    "B)C"
                    "C)D"
                    "D)E"
                    "E)F"
                    "B)G"
                    "G)H"
                    "D)I"
                    "E)J"
                    "J)K"
                    "K)L"))


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


(module+ test
  (require rackunit)
  (define test-file (map (lambda (x) (string-split x ")")) TEST-LIST))
  (define TEST-TREE (create-tree START-POINT test-file))
  (check-equal? (count-orbits TEST-TREE 1) 42))

;; count-orbits: TreeNode Number -> Number
(define (count-orbits tree depth)
  (foldr (lambda (x y) (+ depth (count-orbits x (add1 depth)) y)) 0 (treenode-children tree)))

(module+ test
  (check-equal? (not (false? (child? "B" TEST-TREE))) #t))

;; child?: String TreeNode -> Boolean
(define (child? node tree)
  (define compare-nodes (lambda (x y) (string=? (treenode-data y) node)))
  (cond
    [(empty? (treenode-children tree)) #false]
    [else (or (member node (treenode-children tree) compare-nodes)
              (ormap (lambda (x) (child? node x)) (treenode-children tree)))]))

(define GOAL1 "YOU")
(define GOAL2 "SAN")

(define (deepest-tree-with-both goal1 goal2 tree)

  ;; deepest-tree-with-both/a: TreeNode TreeNode -> TreeNode
  (define (deepest-tree-with-both/a prev-tree curr-tree)
    (cond
      [(not (and (child? goal1 curr-tree) (child? goal2 curr-tree))) prev-tree]
      [else (iter-through-children (treenode-children curr-tree) curr-tree +inf.0 #f)]))

  ;; iter-through-children: [List-of TreeNode] TreeNode Number TreeNode  -> TreeNode
  ;; NOTE: could probably do with argmin
  (define (iter-through-children children prev-tree size-of-smallest tn)
    (cond
      [(empty? children) tn]
      [else (define NEW-VAL (deepest-tree-with-both/a prev-tree (first children)))
            (define NEW-VAL-ORBITS (count-orbits NEW-VAL 1))
            (if (< NEW-VAL-ORBITS size-of-smallest)
                (iter-through-children (rest children) prev-tree NEW-VAL-ORBITS NEW-VAL)
                (iter-through-children (rest children) prev-tree size-of-smallest tn))]))

  (deepest-tree-with-both/a tree tree))




(define (find-depth-in-tree node tree)

  (define (find-depth-in-tree/a tree depth)
    (cond
      [(string=? (treenode-data tree) node) (sub1 depth)]
      [(empty? (treenode-children tree)) 0]
      [else (apply max (map (lambda (x) (find-depth-in-tree/a x (add1 depth))) (treenode-children tree)))]))

  (find-depth-in-tree/a tree 0))

