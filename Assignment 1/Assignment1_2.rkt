#lang racket
;atom-count: atom list ----> number 
(define atom-count
  (lambda (x y)
     (cond[(null? y) 0]; if the list y is null then 0 is returned 
          [(eq? x (car y)) (+ 1 (atom-count x (cdr y)))]; if x is equal to an element in list y then 1 is recursively to the count  
          [else (atom-count x (cdr y))]; recursively goes through list y 
         )))

;Test Cases
 (atom-count 'b '(a b g a b c b) )
 ;3

  (atom-count 'g '(a b g a b c b) )
 ;1

  (atom-count 'w '(a b g a b c b) )
 ;0
 
  (atom-count 'b '() )
; 0

  (atom-count 'x '(x xx x xxx x xxxx x) )
;4

(atom-count 'a '(a b c d e f a))
;2

(atom-count 'b '(aa gg ss gg ss bb ))
;0