#lang racket
;lookup: atom list -----> atom
(define lookup
  (lambda (x y)
    (cond[(null? y) 'UNKNOWN]; if the list passed in is null then it will return UNKOWN
         [(eq? x (car (car y))) (cdar y)]; if x is equal to the first element in list y 
         [else(lookup x (cdr y))];if x is not equal to the first element in list y then it will recursively check other elements in the list 
         )))


;Test Cases
(lookup 'b '((a 34)(b 77)(g 6)) )
;77
(lookup 'a '((a "apple")(b "boy")(g "gate")) )
;"apple"
(lookup 'c '((a 34)(b 77)(g 6)) )
;UNKNOWN
 (lookup 'food '((lodging 250.0)(gas 98.60)(food 120.44)) )
;120.44
(lookup 15 '((12 (2 3)) (15 (3 5)) (30 (2 3 5)) (99 (3 11))) )
 ;(3 5)
 (lookup 'a '() )
;UNKNOWN
(lookup 'g '((a "apple")(b "boy")(g "gate")) )
;gate
(lookup 99 '((12 (2 3)) (15 (3 5)) (30 (2 3 5)) (99 (3 11))) )
;(3 11)