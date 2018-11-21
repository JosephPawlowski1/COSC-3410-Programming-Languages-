#lang plai

;; Abstract trees for "with" with arithmetic expressions (WAE)
(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [neg (rhs WAE?)]
  [mul (lhs WAE?) (rhs WAE?)]
  [div (lhs WAE?) (rhs WAE?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
  [id (name symbol?)]
  [if<0 (lhs WAE?) (chs WAE?) (rhs WAE?)]
)

;; parse : sexp -> WAE
;; to convert s-expressions into WAEs
(define (parse sexp)
  (cond [(number? sexp) (num sexp)]
        [(symbol? sexp) (id sexp)]
        [(list? sexp)
         (case (first sexp)
           [(+) (add (parse (second sexp))
                     (parse (third sexp)))]
           
           [(-) (sub (parse (second sexp))
                     (parse (third sexp)))]
           [(-) (neg (parse (second sexp)))]
          
           [(*) (mul (parse (second sexp))
                     (parse (third sexp)))]
           [(/) (div (parse (second sexp))
                     (parse (third sexp)))]
           [(with) (with (first (second sexp))
                         (parse (second (second sexp)))
                         (parse (third sexp)))]
           [(if<0) (if<0 (parse second sexp)
                        (parse third sexp)
                        parse fourth sexp)]
           )]     
   ))

;; subst : WAE symbol WAE --> WAE
;; substitutes second argument with third argument in first argument,
;; as per the rules of substitution; the resulting expression contains
;; no free instances of the second argument
;; (version p.26 of Krishnamurthi)
(define(subst expr sub-id val)
    (type-case WAE expr
        [num (n) expr]
        [add (l r) (add (subst l sub-id val) (subst r sub-id val))]
        [sub (l r) (sub (subst l sub-id val) (subst r sub-id val))]
        [neg (r)   (neg (subst r sub-id val))] 
        [mul (l r) (mul (subst l sub-id val) (subst r sub-id val))]
        [div (l r) (div (subst l sub-id val) (subst r sub-id val))]
        [with (bound-id named-expr bound-body)
              (if (symbol=? bound-id sub-id)
                 (with bound-id (subst named-expr sub-id val) bound-body)
                 (with bound-id (subst named-expr sub-id val) (subst bound-body sub-id val)))]
        [id (v) (if (symbol=? v sub-id) val expr)]
        [if<0 (l c r) (if<0 (subst l sub-id val) (subst c sub-id val) (subst r sub-id val))]
   ))


;; calc : WAE --> number
;; evaluates WAE expressions by reducing them to numbers
(define (calc expr)
  (type-case WAE expr
     [num (n) n]
     [add (l r) (+ (calc l) (calc r))]
     [sub (l r) (- (calc l) (calc r))]
     [neg (r)   (* (calc r) -1)]
     [mul (l r) (* (calc l) (calc r))]
     [div (l r) (quotient (calc l) (calc r))]
     [with (bound-id named-expr bound-body)
           (calc (subst bound-body bound-id (num (calc named-expr))))]
     [id (v) (error 'calc "free identifier ~a" v)]
     [if<0 (l c r) [if (< (calc l) 0 ) (calc c) (calc r)]]
                   
  ))




(test (calc (parse '{* 5 5})) 25)