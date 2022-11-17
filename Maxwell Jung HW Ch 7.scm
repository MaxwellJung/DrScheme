(load "/Applications/PLT/scheme/simply.scm")

;Homework Ch 7 Maxwell Jung
;6.14
(define (describe-time n)
  (let ((seconds 60))
    (let ((minutes (* seconds 60)))
      (let ((hours (* minutes 24)))
        (let ((days (* hours 365.25)))
          (let ((years (* days 100)))
              (cond ((and (>= n 0) (< n seconds)) (se n 'seconds))
                    ((and (>= n seconds) (< n minutes)) (se (/ n seconds) 'minutes))
                    ((and (>= n minutes) (< n hours)) (se (/ n minutes) 'hours))
                    ((and (>= n hours) (< n days)) (se (/ n hours) 'days))
                    ((and (>= n days) (< n years)) (se (/ n days) 'years))
                    ((>= n years) (se (/ n years) 'centuries))
                    (else 'error))))))))
;7.1
(define (gertrude wd)
  (let ((magicword (se (if (member? (first wd) 'aeiou) 'an 'a) wd)))
    (se magicword 'is magicword 'is magicword)))
;7.2
(let ((pi 3.14159)
      (pie '(lemon meringue)))
    (se 'pi 'is pi 'but 'pie 'is pie))
;7.3
(define (superlative adjective word)
  (se (word adjective 'est) word)) ;wrong because it has word as an argument which scheme thinks is a function
;correction
(define (superlative adjective wd)
  (se (word adjective 'est) wd))
;7.4
(define (sum-square a b)
  (let ((+ *)
        (* +))
    (* (+ a a) (+ b b))))
;this function temporarily changes + sign to multiply function and * sign to add function so it multiplies a by a and multiplies b by b and adds them together.