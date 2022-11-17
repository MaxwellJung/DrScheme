(load "C:/Program Files (x86)/PLT/scheme/simply.scm")

(require (lib "trace.ss"))

(define (square x)
  (* x x ))

(define (expt1 b n)
  (expt-iter 1 b n))

(define (expt-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (expt-iter a (square b) (/ n 2)))
        (else (expt-iter (* a b) b (- n 1)))))