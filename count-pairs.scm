(define x '(1 2 3 (5 6 7)))
(set-car! (cddr x) (cdr x))
(set-cdr! (cdr x) (cdddr x))
(set-cdr! (cddr x) x)

(define (count-pairs pair)
  (let ((visited '()))
    
    (define (visited? pair visited)
      (member pair visited))
    
    (define (cph pair)
      (cond ((null? pair) 0)
            ((not (pair? pair)) 0)
            ((visited? pair visited) 0)
            (else (set! visited (cons pair visited)) (+ 1 (cph (car pair)) (cph (cdr pair))))))
    
    (cph pair)))