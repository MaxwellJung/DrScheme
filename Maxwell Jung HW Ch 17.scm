(load "/Applications/PLT/scheme/simply.scm")

;17.1
;Rod
;Chris
;(Chris Colin Hugh Paul)
;error
;((Rod Argent) Chris White)
;(Rod Argent Chris White)
;((Rod Argent) (Chris White))
;Chris
;(Colin Blutstone)
;#f

;17.2
(define (f1 list1 list2)
  (list (append (cdr list1) (list (car list2)))))

(define (f2 list1 list2)
  (list (cdr list1) (cadr list2)))

(define (f3 list1 list2)
  (append list1 list1))

(define (f4 list1 list2)
  (list (list (car list1) (car list2)) (append (cdr list1) (cdr list2))))

;17.3
;'(procedure procedure procedure procedure)

;17.8
(define (member1 subset set)
  (cond ((null? set) #f)
        ((equal? subset (car set)) set)
        (else (member1 subset (cdr set)))))

;17.9
(define (list-ref1 set num)
  (cond ((= num 0) (car set))
        (else (list-ref1 (cdr set) (- num 1)))))

;17.10
(define (length1 set)
  (cond ((null? set) 0)
        (else (+ 1 (length1 (cdr set))))))

;17.11
(define (before-in-list? set before after)
  (cond ((equal? before (car set)) #t)
        ((equal? after (car set)) #f)
        (else (before-in-list? (cdr set) before after))))

;17.12
(define (no-sublist? set)
  (cond ((or (not (list? set)) (null? set)) #t)
        ((not (list? (car set))) (and #t (no-sublist? (cdr set))))
        (else #f)))

(define (combine a b)
  (cond ((and (list? a) (list? b)) (append a b))
        ((and (list? a) (not (list? b))) (append a (list b)))
        ((and (not (list? a)) (list? b)) (append (list a) b))
        ((and (not (list? a)) (not (list? b))) (append (list a) (list b)))))

(define (flatten set)
    (cond ((no-sublist? set) set)
          ((and (not (list? (car set))) (not (list? (cadr set)))) (combine (combine (car set) (cadr set)) (flatten (cddr set))))
          (else (flatten (combine (car set) (flatten (cdr set)))))))

;17.14
(define (branch num set)
  (cond ((= (length num) 1) (list-ref set (- (car num) 1)))
        (else (branch (cdr num) (list-ref set (- (car num) 1))))))