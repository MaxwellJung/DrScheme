(load "/Applications/PLT/scheme/simply.scm")

;Homework ch 3 & 4 Maxwell Jung
;Textbook: Simply Scheme: Introducing Computer Science 2/e Copyright (C) 1999 MIT
;credits to my dad for helping me out on 4.8-hotshots and 4.10-tip calculator.
;3.7
;⅓

;4.1
;32

;4.3
;(define (f x y) (- y x))
;the function f subtracts the second domain from the first domain.

;(define (identity x) x)
;function identity outputs the domain as the range; the domain and the range are same.

;(define (three x) 3)
;function three maps any domain to 3.

;(define (seven) 7)
;function seven maps itself to 7
;(define (magic n) (- (/ (+ (+ (* 3 n) 13) (- n 1)) 4) 3))
;when a number is a domain of function magic, it first multiplies n by 3 add 13 to the result,  subtracts 1 from n, add that to the 3 times n plus 13, divide the whole result by 4, and subtract 3 which is equal to n.

;4.4
;(define (sphere-volume r) (* (/ 4 3) 3.141592654) (* r r r))
;It has two outputs; 4/3 times pi and r cubed. It doesn’t tell what to do with those two numbers.
;Correction
(define (sphere-volume r)
  (*  (* (/ 4 3) 3.141592654) (* r r r)))

;(define (next x) (x + 1))
;the (x + 1) procedure is wrong
;Correction
(define (next x)
  (+ x 1))

;(define (square) (* x x))
;there has to be an input in the procedure of square function. X is not defined as a domain.
;Correction
(define (square x)
  (* x x))

;(define (triangle-area triangle) (* 0.5 base height))
;triangle can not be recognized as a domain.
;Correction
(define (triangle-area x y)
  (* 0.5 x y))

;(define (sum-of-squares (square x) (square y)) (+ (square x) (square y)))
;the function square is not defined yet.
;Correction
(define (sum-of-squares x y)
  (+ (* x x) (* y y)))

;4.5
(define (fahrenheit x)
  (+ (* 9/5 x) 32))

(define (celsius x)
  (* (- x 32) 5/9))

;4.8
(define (scientific x y)
  (* (expt 10 y) x))
;hotshot
(define (sci-coefficient x)
  (/ x (expt 10 (floor (/ (log x) (log 10))))))
(define (sci-exponent x)
  (floor (/ (log x) (log 10))))

;4.10
(define (tip x)
  (/ (ceiling (* 15 x)) 100))