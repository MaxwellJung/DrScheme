(load "/Applications/PLT/scheme/simply.scm")

;Maxwell Jung HW Ch 14 & 15

;14.6
(define (member2? subset set)
  (if (empty? set)
      #f
      (or (equal? subset (first set)) (member2? subset (bf set)))))
      
;14.7
(define (differences sent)
  (if (equal? (count sent) 1)
      '()
      (se (- (first (bf sent)) (first sent)) (differences (bf sent)))))

;14.9
(define (location wd sent)
  (cond ((not (member? wd sent)) #f)
        (else (let ((firstequal? (equal? wd (first sent))))
               (cond (firstequal? 1)
                     ((not firstequal?) (+ 1 (location wd (bf sent))))
                     (else #f))))))

;14.15
(define (merge sent1 sent2)
  (if (or (empty? sent1) (empty? sent2)) (se sent1 sent2)
      (let ((firstnum1 (first sent1)) (firstnum2 (first sent2)))
        (cond ((<= firstnum1 firstnum2) (se firstnum1 (merge (bf sent1) sent2)))
              ((<= firstnum2 firstnum1) (se firstnum2 (merge sent1 (bf sent2))))
              (else #f)))))

;15.1
(define (to-binary num)
  (cond ((= num 0) "")
        ((odd? num) (word (to-binary (/ (- num 1) 2)) 1))
        ((even? num) (word (to-binary (/ num 2)) 0))))

;15.2
(define (palindrome? sent)
  (let ((pword (accumulate word sent)))
    (if (<= (count pword) 1)
        #t
        (and (equal? (first pword) (last pword)) (palindrome? (bf (bl pword)))))))

;or without using recursion,
(define (reverse wd)
  (cond ((empty? wd) "")
        (else (word (last wd) (reverse (bl wd))))))

(define (palindrome?? sent)
  (let ((pword (accumulate word sent)))
    (let ((pnumber (count pword)))
      (cond ((odd? pnumber) (equal? (reverse ((repeated bf (/ (+ pnumber 1) 2)) pword)) (bl pword)))
            ((even? pnumber) (equal? (reverse ((repeated bf (/ pnumber 2)) pword)) pword))))))

;15.3
;below are the helper functions created to organize the outcome so that it is ordered from least (count word) to the most (count word).

(define (prepend-every prefix sent)
  (every (lambda (wd) (word prefix wd)) sent))

(define (upword wd)
  (if (empty? wd)
      '()
      (se (upword (bl wd)) wd)))

(define (merge left right)
  (cond ((empty? left) right)
	((empty? right) left)
	((<= (count (first left)) (count (first right))) (se (first left) (merge (bf left) right)))
        ((<= (count (first right)) (count (first left))) (se (first right) (merge (bf right) left)))))

(define (one-half sent)
  (if (<= (count sent) 1)
      sent
      (se (first sent) (one-half (bf (bf sent))))))

(define (other-half sent)
  (if (<= (count sent) 1)
      '()
      (se (first (bf sent)) (other-half (bf (bf sent))))))

(define (in-order sent)
  (if (<= (count sent) 1)
      sent
      (merge (in-order (one-half sent))
             (in-order (other-half sent)))))

;actual function
(define (substrings wd)
  (in-order (if (empty? wd)
                '()
                (se (prepend-every (first wd) (upword (bf wd))) (substrings (bf wd))))))