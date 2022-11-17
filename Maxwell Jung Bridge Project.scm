(load "/Applications/PLT/scheme/simply.scm")

;Maxwell Jung Bridge Project

(define (valid-card? card)
  (let ((suit (first card)) (rank (bf card)))
    (and (member? suit '(s h d c)) ;checks if the card is one of the suits
         (member? rank '(a 1 2 3 4 5 6 7 8 9 10 j q k))))) ;checks if the card value is valid

(define (valid-hand? hand)
  (and (equal? (count hand) 13) ;a standard hand consists of 13 cards
       (equal? (keep valid-card? hand) hand))) ;checks if all cards are valid

(define (card-val card)
  (let ((rank (bf card)) (suit (first card)))
    (cond ((not (valid-card? card)) '(Invalid Card))
          ((equal? rank 'a) 4)
          ((equal? rank 'k) 3)
          ((equal? rank 'q) 2)
          ((equal? rank 'j) 1)
          ((member? rank '(2 3 4 5 6 7 8 9 10)) 0))))

(define (high-card-points hand)
  (accumulate + (every card-val hand)))

(define (count-suit suit hand)
  (accumulate + 
              (every 
               (lambda (card) (if (equal? (first card) suit) 1 0)) ; if a card equals the inputed suit, it marks one, otherwise 0. At the end add all and get how many suit is in the had.
                       hand)))

(define (suit-counts hand)
  (se (count-suit 's hand) (count-suit 'h hand) (count-suit 'c hand) (count-suit 'd hand)))

(define (suit-dist-points num)
  (cond ((equal? num 2) 1) ;2 of same suit, 1 point
        ((equal? num 1) 2) ;1 of same suit, 2 points
        ((equal? num 0) 3) ;0 of same suit, 3 points
        (else 0)))

(define (hand-dist-points hand)
  (accumulate + (every suit-dist-points (suit-counts hand)))) ;calculates point value for suit distribution for each suit and adds all of them.

(define (bridge-val hand)
  (if (valid-hand? hand) (+ (high-card-points hand) (hand-dist-points hand)) 'Cheater!!!)) ;checks for valid hand and if yes, calculate point. If no, say Cheater!!!