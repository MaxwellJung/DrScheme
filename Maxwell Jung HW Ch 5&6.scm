(load "/Applications/PLT/scheme/simply.scm")

;Maxwell Jung HW Chapter 5 & 6
;5.1
;(I me mine)
;(is empty)
;2345
;(23 45)
;""
;()
;6
;("" "")
;2
;5.3
;if there is no parentheses outside the word, it only takes the first letter and if there is a parentheses it recognizes it as a word in a sentence and takes the first word.
;5.4
;The one with a quote would give square because scheme thinks square 7 is a sentence. The one without the quote would give 4 (assuming we already defined square function) because first of 49 is 4.
;5.9
;(matt wright)
;(brian harvey)
;5.16
(define (two-first x y)
  (word (first x) (first y)))
(define (two-first-sent sent)
  (word (first (first sent)) (first (last sent))))
;5.19
(define (insert-and x)
  (se (se (bl x) 'and) (last x)))
;6.1
;(nowhere man)
;3
;goes
;6.3
(define (sign number)
  (cond ((< number 0) 'negative)
        ((= number 0) 'zero)
        (else 'positive)))
;6.7
(define (type-of x)
  (cond ((boolean? x) 'boolean)
        ((word? x) 'word)
        ((sentence? x) 'sentence)
        (else 'Invalid)))
;6.8
(define (indef-article phrase)
  (cond ((member? (first (first phrase)) 'aeiou) (se 'an phrase))
        ((and (not (member? (first (first phrase)) 'aeiou)) (not (number? phrase))) (se 'a phrase))
        (else phrase)))
        
;6.11
(define (between? x y z)
  (and (>= x y) (<= x z)))
(define (31-day-month? m)
  (or (= m 1) (= m 3) (= m 5) (= m 7) (= m 8) (= m 10) (= m 12)))
(define (30-day-month? m)
  (or (= m 4) (= m 6) (= m 9) (= m 11)))
;Valid Date?
(define (valid-date? m d y)
  (cond ((and (number? m) (number? d) (number? y)) (cond ((and (cond ((= (remainder y 400) 0) #t)
                                                                     ((= (remainder y 100) 0) #f)
                                                                     ((= (remainder y 4) 0) #t)
                                                                     (else #f)) (= m 2) (between? d 1 29)) '(Valid Date))
                                                         ((and (between? m 1 12) (cond ((31-day-month? m) (between? d 1 31))
                                                                                       ((30-day-month? m) (between? d 1 30))
                                                                                       ((= m 2) (between? d 1 28))
                                                                                       (else #f))) '(Valid Date))
                                                         (else '(Invalid Date))))
        (else '(Invalid Date))))