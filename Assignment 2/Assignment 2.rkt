#lang racket
; updown: list ----> list
;add one to even numbers and subtracts one from odd numbers 
(define updown
  (lambda (x)
    (cond [ (null? x) '()];checks to see if the list passed through is null 
          [(even? (car x)) (cons (+ (car x)  1) (updown (cdr x)))]; if the first atom of the list is even then one is added to it
          ;and then it recursively goes through the rest of the list 
          [(odd? (car x)) (cons (- (car x) 1) (updown (cdr x)))];if the first atom of the list is odd then one is subtracted from it
          ;and then it recursively goes through the rest of the list 
          )))

 (updown '(2 4 10 3 6))
; '(3 5 11 2 7)

  (updown '(1 3))
; '(0 2)

  (updown '())
; '()

  (updown '(1 2 -3 -4 7 12))
; '(0 3 -4 -3 6 13)

(updown '( -2 -3 -4 -19))
;'(-1 -4 -3 -20)

(updown '(2))
;(3)

(define count
  (lambda (x)
    (cond [(null? x) 0]
          [else (+ 1 (count (cdr x )))]
          )))


;zip: list list ---> list
(define zip
  (lambda (x y)
    (cond [(null? x) '()]; checks to see if list x is null
          [(null? y) '()]; checks to see if list y is null 
          [(> (count x ) (count y)) (error 'zip  "second list is too short")] ; checks to see if x has more atoms then y 
          [(< (count x ) (count y)) (error 'zip "first list is too short")]; checks to see if y has more atom then x
          [else (cons (cons (car x) (list(car y))) (zip (cdr x) (cdr y)))]; puts the first atom in both list x and y together. Then recursively goes through both list doing the same 
          )))


  (zip '(a b c d e) '(32 7 10 3 1) )
; '((a 32) (b 7) (c 10) (d 3) (e 1))

  (zip '() '() )
; '()

  (zip '((a b) c (d e f)) '(c (a) (b c)) )
; '(((a b) c) (c (a)) ((d e f) (b c)))

  ;(zip '(1 2 -3) '(50 40 30 20))
; x x zip: first list too short

;(zip '(1 2 -3 4) '(50 40 30))
; x x zip: second list is too short

(zip '(1 2 3 4 5) '(32 7 10 3 1) )
;'((1 32) (2 7) (3 10) (4 3) (5 1))


;deep-mult: list ---> number
(define deep-mult
  (lambda (x)
    (cond [(null? x) 1];checks to see if the list is null 
           [(number? (car x)) (* (car x) (deep-mult (cdr x)))] ;checks to see if the first atom is in the list is a number.
           ;if it is then this is recursively goes though the list multiplying all the numbers together 
           [(pair? (car x)) (* (deep-mult (car x)) (deep-mult (cdr x)))] ; checks to see if the atom is a pair. if it is then is it recursively goes through that list
           [else (deep-mult (cdr x))];if it is not a number or a pair then it recursively goes to the next atom
          )))


(deep-mult '(5 a b 8 2))
; 80

  (deep-mult '((4 (6 1)) 2 3 (4)))
; 576

  (deep-mult '(these (aren't 77) (all 32 (numbers 93 here))))
; 229152

  (deep-mult '())
; 1

  (deep-mult '(no numbers here))
; 1
(deep-mult '(3))
;3
(deep-mult '(5 6 2 4 1 3 4 (4 3 )))
;34560

;drop-parens; list ---> list         
(define drop-parens
  (lambda (x)
    (cond ((null? x) '());checks if the list is null
          ((pair? x) (append (drop-parens (car x)) (drop-parens (cdr x)))) ;checks the atom is a pair, then recurively runs through that list droping the parens and append them together 
          (else (list x));if it is not a pair then recurively goes to the next atom
          )))

(drop-parens '((a 34)(b 77)(g 6)) )
; (a 34 b 77 g 6)

  (drop-parens '(a b c) )
 ;(a b c)

  (drop-parens '() )
 ;()

  (drop-parens '(()((() x)())) )
 ;(x)

(drop-parens '(1 2( 3 4 ) ))
;'(1 2 3 4)
(drop-parens '(a s d f g h ))
;'(a s d f g h)
> 
             