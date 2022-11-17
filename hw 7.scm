;Maxwell Jung
;Period C
(require (lib "trace.ss"))

;2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch branch-length branch-structre)
  (list branch-length branch-structre))

(define (left mobile)
  (car mobile))

(define (right mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structre branch)
  (cadr branch))

(define (total-weight mobile)
  (cond ((number? mobile) mobile)
        ((number? (branch-structre (left mobile))) (+ (branch-structre (left mobile)) (total-weight (branch-structre (right mobile)))))
        ((number? (branch-structre (right mobile))) (+ (total-weight (branch-structre (left mobile))) (branch-structre (right mobile))))
        (else (+ (total-weight (branch-structre (left mobile))) (total-weight (branch-structre (right mobile)))))))

(define mobile '((2 ((7 6) (1 9))) (5 ((3 4) (8 1)))))
(define m
  (make-mobile
   (make-branch 6
    (make-mobile
     (make-branch 1 8)
     (make-branch 4 2)))
   (make-branch 5 12)))

(total-weight mobile)

(define (balanced? mobile)
  (if (not (list? mobile)) 
      #t 
      (let ((left-branch (left mobile)) (right-branch (right mobile)))
        (cond ((and (number? (branch-structre left-branch)) (number? (branch-structre right-branch))) (= (* (branch-length left-branch) (total-weight (branch-structre left-branch))) (* (branch-length right-branch) (total-weight (branch-structre right-branch)))))
              (else (and (balanced? (branch-structre (left mobile))) (balanced? (branch-structre (right mobile)))))))))

;(trace balanced?)
(balanced? mobile)
(balanced? m)

;2.30
(define (square x)
  (* x x))

(define (square-tree lyst)
  (cond ((null? lyst) lyst)
        ((number? lyst) (square lyst))
        (else (append (list (square-tree (car lyst))) (square-tree (cdr lyst))))))
  

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;2.31
(define (tree-map procedure tree)
  (cond ((null? tree) tree)
        ((number? tree) (procedure tree))
        (else (append (list (tree-map procedure (car tree))) (tree-map procedure (cdr tree))))))

(tree-map square
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;2.32
(define (subsets s)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets '(1 2 3))
;subset procedure works by appending subsets of a set with an insertion of another element to each element of that subset. This works because all subsets can be created by adding another element to an existing subset: (subsets set) is same as appending subsets of (cdr set) with (map/every insert-another-element (subsets of (cdr set))). There is a clear pattern in the output of the procedure (subsets): the second half of the list is the insertion of the some element to all elements of the first half of the list. For example, (subsets '(1 2 3)) outputs (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). The second half of the list is ((1) (1 3) (1 2) (1 2 3)) which is the insertion of "1" to all elements of the first half of the list: (() (3) (2) (2 3)).

;2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              ()
              sequence))

(define (append seq1 seq2)
  (accumulate cons
              seq2
              seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y))
              0
              sequence))

;2.35
(define (count-leaves t)
  (accumulate (lambda (x y) (+ x y))
              0
              (map length t)))