(load "/Applications/PLT/scheme/simply.scm")

;Maxwell Jung Homework Ch 11 & 12

;11.2
(define (count-ums sent)
  (if (= (count sent) 0)
      0
      (if (equal? 'um (first sent))
          (+ 1 (count-ums (bf sent)))
          (count-ums (bf sent)))))

;11.3
(define (unspell-letter letter)
  (cond ((member? letter 'abc) 2)
	((member? letter 'def) 3)
	((member? letter 'ghi) 4)
	((member? letter 'jkl) 5)
	((member? letter 'mno) 6)
	((member? letter 'prs) 7)
	((member? letter 'tuv) 8)
	((member? letter 'wxy) 9)
	(else 0)))

(define (phone-unspell wd)
  (if (= (count wd) 0)
      ""
      (word (unspell-letter (first wd)) (phone-unspell (bf wd)))))

;11.5
(define (initials sent)
  (if (= (count sent) 1)
      (first (first sent))
      (se (first (first sent)) (initials (bf sent)))))

;11.6
(define (countdown num)
  (if (<= num 0)
      'blastoff!
      (se num (countdown (- num 1)))))

;12.2
;(define (acronym sent)
;  (if (= (count sent) 1)
;      (first sent)
;      (word (first (first sent))
;	    (acronym (bf sent)))))

(define (acronym sent)                       ;; The first of sentence is a word and we want a letter so first the first of sent
  (if (= (count sent) 1)
      (first (first sent))
      (word (first (first sent))
	    (acronym (bf sent)))))

;12.5
(define (exaggerate sent)
  (if (empty? sent)
      '()
      (let ((exaggerative (first sent)))
        (se (cond ((number? exaggerative) (* 2 exaggerative))
                  ((equal? exaggerative 'good) 'great)
                  ((equal? exaggerative 'bad) 'terrible)
                  (else exaggerative)) (exaggerate (bf sent))))))

;12.7
(define (spell-digit digit)
  (item (+ 1 digit)
	'(zero one two three four five six seven eight nine)))

(define (spell-number num)
  (if (empty? num)
      '()
      (se (spell-digit (first num)) (spell-number (bf num)))))