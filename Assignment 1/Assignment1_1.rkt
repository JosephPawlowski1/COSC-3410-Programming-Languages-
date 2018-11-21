#lang racket
;in-range?: atom atom atom -----> boolean
(define in-range?
  (lambda (x y z)
    (cond [(null? z) #t];if list z is null then true is returned 
          [(> x (car z)) #f];if an element in z is less then x then false is returned 
          [(< y (car z)) #f];if an element in z is greater then y then false is returned 
          [else ( in-range? x y (cdr z))];recursively going through the list z 
          )))


 (in-range? 3 12 '(5 3 9) )
 ;#t
 
  (in-range? 3 12 '(5 13 9) )
 ;#f
 
  (in-range? 4 4 '(4 4 4 4 4) )
; #t
 
  (in-range? 3 12 '() )
; #t
(in-range? 5 5 '(5 5 5 6 5 ))
;#f

(in-range? 2 8 '( 2 3 4 5 6 7))
;#t
