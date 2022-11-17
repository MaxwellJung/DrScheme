(load "/Applications/PLT/scheme/simply.scm")

;Maxwell Jung HW Ch 9

;9.1
;procedure
;34
;(yan etim ta lal)
;error

;9.3
(define (let-it-be sent) (accumulate (lambda (x y) y) sent)) ;This function takes in a sentence, accumulates the function that takes in two arguments and outputs the second argument. it applies that output with the next element until it runs out of elements. It starts from the first two elements from the left.

;9.4
(define (who sent)
  (every describe '(pete roger john keith)))

(define (describe person)
  (se person sent))
;This function doesn't work because there isn't a variable called sent within the "describe" function. Correction would be
(define (who sent)
  (every (lambda (person) (se person sent)) '(pete roger john keith)))

;9.5
(define (prepend-every prefix sent)
  (every (lambda (wd) (word prefix wd)) sent))

;9.8
(define (hang-letter letter guesses)
  (if (member? letter guesses)
      letter
      '_))

(define (hang answer guesses)
  (accumulate word (every (lambda (letter) (hang-letter letter guesses)) answer)))

;9.10
(define (appearances2 x y)
  (accumulate + (every (lambda (element) (if (equal? element x) 1 0)) y)))

;9.14
(define (substitute wd1 wd2 sent)
  (every (lambda (wd) (if (equal? wd wd2) wd1 wd)) sent))

;9.17
(define (keep2 predicate se)
  (every (lambda (x) (if (predicate x) x '() )) se))