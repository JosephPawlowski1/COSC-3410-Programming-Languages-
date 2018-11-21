#lang plai
;;Tyler Hackel
;;Joseph Pawlowski
;;Programming Languages 

;; Type define F1WAE
(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?) (rhs F1WAE?)]
  [sub (lhs F1WAE?) (rhs F1WAE?)]
  [div (lhs F1WAE?) (rhs F1WAE?)]
  [mul (lhs F1WAE?) (rhs F1WAE?)]
  [neg (lhs F1WAE?)]
  [with (name symbol?) (named-expr F1WAE?) (body F1WAE?)]
  [id (name symbol?)]
  [if<0 (lhs F1WAE?)(chs F1WAE?) (rhs F1WAE?)]
  [app (fun-name symbol?) (arg F1WAE?)])

;; Type define Fundef
(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)])

;; lookup-fundef: symbol listof(FunDef) --> FunDef
(define (lookup-fundef name defs)
  (cond [(null? defs) (error name "definition not found")]
        [(symbol=? name (fundef-fun-name (car defs))) ;; using the accessor function for fun-name
             (car defs)]
        [else (lookup-fundef name (cdr defs))]
     ))

;; Subst function
(define (subst expr sub-id val)
  (type-case F1WAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [sub (l r) (sub (subst l sub-id val)
                    (subst r sub-id val))]
    [div (l r) (div (subst l sub-id val)
                    (subst r sub-id val))]
    [mul (l r) (mul (subst l sub-id val)
                    (subst r sub-id val))]
    [neg (l) (neg (subst l sub-id val))]
    [with (bound-id named-expr bound-body)
          (if (symbol=? bound-id sub-id)
              (with bound-id
                    (subst named-expr sub-id val)
                    bound-body)
              (with bound-id
                    (subst named-expr sub-id val)
                    (subst bound-body sub-id val)))]
    [app (fun-name arg-expr)
         (app fun-name (subst arg-expr sub-id val))]
    [id (v) (if (symbol=? v sub-id) val expr)]
    [if<0 (l c r) (if<0 (subst l sub-id val) (subst c sub-id val) (subst r sub-id val))]
))

;; interp: F1WAE listof(FunDef) --> number
(define (interp expr fun-defs)
  (type-case F1WAE expr
    [num (n) n]
    [add (l r) (+ (interp l fun-defs) (interp r fun-defs))]
    [sub (l r) (- (interp l fun-defs) (interp r fun-defs))]
    [div (l r) (quotient (interp l fun-defs) (interp r fun-defs))]
    [mul (l r) (* (interp l fun-defs) (interp r fun-defs))]
    [neg (l) (-(interp l fun-defs))]
    [with (bound-id named-expr bound-body)
          (interp (subst bound-body
                         bound-id
                         (num (interp named-expr fun-defs)))
                  fun-defs)]
    [id (v) (error 'interp "free identifier: ~a" v)]
    [if<0 (l c r) (if (< (interp l fun-defs) 0)  (interp c fun-defs) (interp r fun-defs))]
    [app (fun-name arg-expr)
         (let ([the-fun-def (lookup-fundef fun-name fun-defs)])
           (interp (subst (fundef-body the-fun-def)
                          (fundef-arg-name the-fun-def)
                          (num (interp arg-expr fun-defs)))
                   fun-defs))]
  ))

;Count function
(define count
  (lambda (x)
    (cond [(null? x) 0]
          [else (+ 1 (count (cdr x )))]
)))

;; parse: s-expr -->F1WAE
(define (parse sexp)
  (cond [(number? sexp) (num sexp)]
        [(symbol? sexp) (id sexp)]
        [(eq? (first sexp) '+) (add (parse (second sexp))
                                    (parse (third sexp)))]
        [(eq? (first sexp) '-) (if (= (length sexp) 2)
                                   (neg (parse (second sexp)))
                                    (sub (parse (second sexp))
                                          (parse (third sexp))))]
        [(eq? (first sexp) '*) (mul (parse (second sexp))
                                    (parse (third sexp)))]
        [(eq? (first sexp) '/) (div (parse (second sexp))
                                    (parse (third sexp)))]
        [(eq? (first sexp) 'if<0) (if<0 (parse (second sexp))
                                        (parse (third sexp))
                                        (parse (fourth sexp)))]
        [(eq? (first sexp) 'with) (with (first (second sexp))
                                    (parse (second (second sexp)))
                                    (parse (third sexp)))]
        [(symbol? (first sexp)) (app (first sexp) 
                                     (parse (second sexp)))]
        [else (error 'parse "unexpected operator: ~a" (first sexp))]
     ))

;; sample program from Krishnamurthi p.29
;; NOTE: we pass the tree created by parse AND a list of function definitions
(interp (parse ' {double 5})
        (list (fundef 'double
                      'n
                     (parse '{+ n  n  }))))

;Factorial Function
;Enter any number from 0 - n
(interp (parse '(fact 0))
        (list (fundef 'fact 'n
                      (parse '{if<0 {- n 1} 1
                              {* n {fact {- n 1}}}}))))

;Fibbonacci Function
;The complete sequence can be seen by starting with fib(0),
;continuing through fib(n)
(interp (parse '(fib 7))
        (list (fundef 'fib 'n
              (parse '(if<0 {- n 1} 0
                            {if<0 {- n 3} 1
                                  {+ (fib (- n 1)) (fib (- n 2))}})))))
