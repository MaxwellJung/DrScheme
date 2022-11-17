;Maxwell Jung
;Frederic Wang
;Period C

(require (lib "trace.ss"))

;3.1
(define (make-accumulator value)
  (lambda (amount) (begin (set! value (+ value amount)) value)))

(define A (make-accumulator 5))
(A 10)
(A 10)

;3.2
(define (make-monitored f)
  (let ((count 0))
    (lambda (x) (cond ((equal? x 'how-many-calls?) count)
                      ((equal? x 'reset-count) (set! count 0))
                      (else (set! count (+ count 1)) (f x))))))

(define s (make-monitored sqrt))
(s 100)
(s 4)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)

;3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password . amount)
    "Incorrect-password")
  (define (unknown-request . amount)
    "Unknown request")
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else unknown-request))
        incorrect-password))
  dispatch)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'secret-password 'interest) 10)
((acc 'some-other-password 'deposit) 50)

;3.7
(define (make-joint account1 password1 password2)
  (lambda (p m) (if (eq? p password2)
                    (account1 password1 m)
                    (account1 password2 m))))

(define peter-acc (make-account 100 'open-sesame))
((peter-acc 'open-sesame 'deposit) 10)

(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'rosebud 'withdraw) 15)

(define patrick-acc (make-joint paul-acc 'rosebud 'unlock))
((patrick-acc 'unlock 'withdraw) 90)

;3.8



