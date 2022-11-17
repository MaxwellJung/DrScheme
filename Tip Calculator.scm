(load "/Applications/PLT/scheme/simply.scm")

;Tip Calculator Project Maxwell Jung
(define (tip bill tippercent ppl)
  (/ (round (* (/ (round (* tippercent bill)) 100) (/ 100 ppl))) 100))
;(* tippercent bill) multiplies the tip in percent value to the bill. I round this number and devide it by 100 because percent value is 100 times the decimal value. then it multiplies by 100 and devides by number of people to make it easier to round. Then, I devided by 100 to get the cent values.
