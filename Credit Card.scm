(load "/Applications/PLT/scheme/simply.scm")

;Maxwell Jung Credit Card Project
;https://creditcards.chase.com/credit-cards/sapphire-preferred-card.aspx?iCELL=6215&list=4,2,1
;APR (any APR can be calculated) = 15.99%
;Minimum Payment = 3% * balance
;Purchase (any amount can be calculated) = I chose NVIDIA GEFORCE GTX 980 TI = $649.99
;http://www.amazon.com/dp/B00YDAYOK0/?tag=pcgedit-20&ascsubtag=bestgpu

;Input sentence format = (credit-card '(balance APR% minimum-payment-rate%))
(define (months-pay-off bill)
  (let ((balance (first bill)) (APR (first (bf bill))) (mrate (last bill)))
    (let ((minimum-payment (* (/ mrate 100.) balance)))
      (cond ((or (< balance 25) (<= (- balance minimum-payment) 0)) 1)
            ((>= (* (+ 1 (/ APR 1200.)) (- balance minimum-payment)) balance) 'infinite)
            (else (+ 1 (months-pay-off (se (* (+ 1 (/ APR 1200.)) (- balance minimum-payment)) APR mrate))))))))

(define (actual-cost bill)
  (cond ((equal? 'infinite (months-pay-off bill)) 'infinite)
        (else (let ((balance (first bill)) (APR (first (bf bill))) (mrate (last bill)))
                (let ((minimum-payment (* (/ mrate 100.) balance)))
                  (cond ((or (< balance 25) (<= (- balance minimum-payment) 0)) balance)
                        (else (+ minimum-payment (actual-cost (se (* (+ 1 (/ APR 1200.)) (- balance minimum-payment)) APR mrate))))))))))

(define (credit-card bill)
  (let ((balance (first bill)) (APR (first (bf bill))) (mrate (last bill)))
    (se '(Charging) (word '$ balance) '(on your card with APR of) (word APR '%) '(and paying only the monthly minimums of) (word mrate '%) '(will take) (months-pay-off bill) (word 'month "(s)") '(to pay off and cost a total of) (word '$ (cond ((number? (actual-cost bill)) (/ (round (* (actual-cost bill) 100.)) 100.)) (else (actual-cost bill)))))))