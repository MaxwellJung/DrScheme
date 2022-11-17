(load "/Applications/PLT/scheme/simply.scm")

;Maxwell Jung HW Ch 8
;8.1
;(a e i o u)
;()
;0
;#f
;(16 144 0)
;ai
;25
;(go d sunshi)

;8.2
;keep
;every
;first
;every last
;accumulate word every last
;every
;accumulate

;8.3
;(define (f a) (keep even? a))
;function f checks the elements of "a" for even numbers. If combines all the even elements into a sentence.
;(define (g b) (every b '(blue jay way)))
;if b is a function that can deal with one inputs, it applies the function to every with '(blue jay way). For example if it's (f first), b would be first so it would give (b j w).
;(define (h c d) (c (c d)))
;c has to be a function of one argument and d can be any number, sentence, or word. Function h repeats function c to d.
;(define (i e) (/ (accumulate + e) (count e)))
;e has to be a sentence or word that only contains numbers. The function adds all numbers in the group and devide by how many numbers there are. It calculates the average.
;accumulate takes in a function and a sentence. It looks at each words of the sentence and applies the function to the first two words and applies the function to that result and the next word and so on until it reaches the last argument.
;sqrt
;It takes any positive number. It square roots the number and gives out the result.
;repeated
;It takes a function and tells it to repeat it however many times you list. It takes the result of the funciton, put that as domain, gets result, put that as domain until it has repeated it as many times as it was listed.
;(repeated sqrt 3)
;Tt square roots a number three times. so It would root the root of the root of a number (triple root).
;(repeated even? 2)
;It checks if the argument is even twice.
;(repeated first 2)
;It takes the first word/letter from a sentence/word and it takes the first letter of the word/letter.
;(repeated (repeated bf 3) 2)
;it (repeated bf 3) applies but first function three times however this is repeated twice. The entire function bf an argument 6 times.

;6.3
(define (letter wd)
  (every first wd)) ;I defined this because I want my function not only a word but also a sentence (group of words). You'll see that I defined this so I can get all the letters of a sentence.
(define (phonetic letter)
  (cond ((equal? 'a letter) 'Alpha)
        ((equal? 'b letter) 'Bravo)
        ((equal? 'c letter) 'Charlie)
        ((equal? 'd letter) 'Delta)
        ((equal? 'e letter) 'Echo)
        ((equal? 'f letter) 'Foxtrot)
        ((equal? 'g letter) 'Golf)
        ((equal? 'h letter) 'Hotel)
        ((equal? 'i letter) 'India)
        ((equal? 'j letter) 'Juliet)
        ((equal? 'k letter) 'Kilo)
        ((equal? 'l letter) 'Lima)
        ((equal? 'm letter) 'Mike)
        ((equal? 'n letter) 'November)
        ((equal? 'o letter) 'Oscar)
        ((equal? 'p letter) 'Papa)
        ((equal? 'q letter) 'Quebec)
        ((equal? 'r letter) 'Romeo)
        ((equal? 's letter) 'Sierra)
        ((equal? 't letter) 'Tango)
        ((equal? 'u letter) 'Uniform)
        ((equal? 'v letter) 'Victor)
        ((equal? 'w letter) 'Whiskey)
        ((equal? 'x letter) 'Xray)
        ((equal? 'y letter) 'Yankee)
        ((equal? 'z letter) 'Zulu)
        ((equal? 0 letter) 'Zero)
        ((equal? 1 letter) 'Wun)
        ((equal? 2 letter) 'Too)
        ((equal? 3 letter) 'Tree)
        ((equal? 4 letter) 'Fower)
        ((equal? 5 letter) 'Fife)
        ((equal? 6 letter) 'Six)
        ((equal? 7 letter) 'Seven)
        ((equal? 8 letter) 'Ait)
        ((equal? 9 letter) 'Niner)
        (else '(What is this?))))
(define (words x)
  (every phonetic (every letter x)))

;8.7
(define (letter-count argument)
  (accumulate + (every count argument)))

;8.8
(define (change-exaggerative-word wd)
  (cond ((equal? 'Good wd) 'Great)
        ((equal? 'good wd) 'great)
        ((equal? 'cool wd) 'wtf)
        ((equal? 'Cool wd) 'wtf)
        ((equal? 'Bad wd) 'Terrible)
        ((equal? 'bad wd) 'terrible)
        ((number? wd) '(over 9000)) ;if I had to double the number I can just replace '(over 9000) to (* 2 wd)
        (else wd)))

(define (exaggerate sent)
  (every change-exaggerative-word sent))

;8.10
(define (true-for-all? predicate se)
  (equal? (keep predicate se) se))

;8.13
(define (alphabet-to-number letter)
  (cond ((member? letter '(a b c)) 2)
        ((member? letter '(d e f)) 3)
        ((member? letter '(g h i)) 4)
        ((member? letter '(j k l)) 5)
        ((member? letter '(m n o)) 6)
        ((member? letter '(p q r s)) 7)
        ((member? letter '(t u v)) 8)
        ((member? letter '(w x y z)) 9)
        (else letter)))

(define (phone-unspell wd)
  (every alphabet-to-number wd))

;8.14
(define (subword wd num1 num2)
  ((repeated bl (- (count wd) num2)) ((repeated bf (- num1 1)) wd)))