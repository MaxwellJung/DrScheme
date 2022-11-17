(load "/Applications/PLT/scheme/simply.scm")

;Maxwell Jung Rock Paper Scissors Classic and Rock Paper Scissors Lizard Spock project

(define (comp) 
     (let ((x (random 3)))
        (cond ((equal? x 0) 'rock) 
              ((equal? x 1) 'paper) 
              ((equal? x 2) 'scissors)
              (else 'noooooo))))

(define (rps human_throw)
  
  (let ((comp_throw (comp)))
    
    (cond ((not (member? human_throw '(rock paper scissors))) '(That was not a valid input....play fair!))
              
          ((equal? human_throw comp_throw) '(You Tied...meh))
              
          ((and (equal? human_throw 'paper) (equal? comp_throw 'rock)) (se '(You won!....) human_throw 'beat comp_throw))
          
          ((and (equal? human_throw 'rock) (equal? comp_throw 'scissors)) (se '(You won!....) human_throw 'beat comp_throw))
          
          ((and (equal? human_throw 'scissors) (equal? comp_throw 'paper)) (se '(you won!....) human_throw 'beat comp_throw))
                 
          (else (se '(You lost...) human_throw '( loses to ) comp_throw)))))

(define (opponent)
  (let ((x (random 5)))
    (cond ((equal? x 0) 'rock) 
            
          ((equal? x 1) 'paper) 
        
          ((equal? x 2) 'scissors)
          
          ((equal? x 3) 'lizard)
          
          ((equal? x 4) 'spock)
          
          (else 'cheater!!!!!))))

(define (rpsls human_throw)
  
  (let ((opp_throw (opponent)))
    
    (cond ((and (not (member? human_throw '(rock paper scissors lizard spock))) (not (member? '$ human_throw))) '(You can not do that!))
              
          ((equal? human_throw opp_throw) '(You Tied...Try Again))
              
          ((and (equal? human_throw 'scissors) (equal? opp_throw 'paper)) (se '(You won!....) human_throw 'cuts opp_throw))
          
          ((and (equal? human_throw 'paper) (equal? opp_throw 'rock)) (se '(You won!....) human_throw 'covers opp_throw))
          
          ((and (equal? human_throw 'rock) (equal? opp_throw 'lizard)) (se '(you won!....) human_throw 'crushes opp_throw))
                 
          ((and (equal? human_throw 'lizard) (equal? opp_throw 'spock)) (se '(you won!....) human_throw 'poisones opp_throw))
          
          ((and (equal? human_throw 'spock) (equal? opp_throw 'scissors)) (se '(you won!....) human_throw 'smashes opp_throw))
          
          ((and (equal? human_throw 'scissors) (equal? opp_throw 'lizard)) (se '(you won!....) human_throw 'decapitates opp_throw))
          
          ((and (equal? human_throw 'lizard) (equal? opp_throw 'paper)) (se '(you won!....) human_throw 'eats opp_throw))
          
          ((and (equal? human_throw 'paper) (equal? opp_throw 'spock)) (se '(you won!....) human_throw 'disproves opp_throw))
          
          ((and (equal? human_throw 'spock) (equal? opp_throw 'rock)) (se '(you won!....) human_throw 'vaporizes opp_throw))
          
          ((and (equal? human_throw 'rock) (equal? opp_throw 'scissors)) (se '(you won!....) human_throw 'crushes opp_throw))
          
          ((member? '$ human_throw) (cond ((> (bf human_throw) 0) '(You Win!!!));Pay to Win
                                          
                                          ((<= (bf human_throw) 0) '(Nope. Keep Playing.))));Some people tries to give negative money
          
          (else (se '(You lost....) human_throw '(loses to) opp_throw)))))
