#lang plai

; Patrick Walter
; Student ID: 2568215
; Project: wae Interpreter/parser

; Note on using this language:
; If you want to check out several different cases
; use the test-wae function with eval-wae as the input
; (test-wae eval-wae) will run several test cases for you

; Definition our language
(define-type wae
  (num (n number?))
  (add (rhs wae?)(lhs wae?))
  (sub (rhs wae?)(lhs wae?))
  (with (name symbol?)(name-expr wae?)(body wae?))
  (id (name symbol?)))

; Interp takes the abstract syntax of our language and
; calulates the expression in scheme
(define interp-wae
  (lambda (expr)
    (type-case wae expr
      [num (n) n]
      [add (l r) (+ (interp-wae l) (interp-wae r))]
      [sub (l r) (- (interp-wae l) (interp-wae r))]
      [with (bound-id named-expr bound-body)
            (interp-wae (subst bound-body bound-id (num (interp-wae named-expr))))]
      [id (v) (error `interp-wae "Free Identifier")])))

(define subst
  (lambda (expr sub-id val)
    (type-case wae expr
      [num (n) expr]
      [add (l r) (add (subst l sub-id val) (subst r sub-id val))]
      [sub (l r) (sub (subst l sub-id val) (subst r sub-id val))]
      [with (bound-id named-expr bound-body)
            (if (symbol=? bound-id sub-id)
                (with bound-id (subst named-expr sub-id val) bound-body)
                (with bound-id (subst named-expr sub-id val)
                      (subst bound-body sub-id val)))]
      [id (v) (if (symbol=? v sub-id) val expr)])))

; Takes concrete syntax and transforms it into our defined abstract syntax
(define parse-wae
  (lambda (expr)
    (cond
      [(number? expr) (num expr)]
      [(list? expr)
       (case (car expr)
         ('+ (add (parse-wae (cadr expr)) (parse-wae (caddr expr))))
         ('- (sub (parse-wae (cadr expr)) (parse-wae (caddr expr))))
         ('with (with (caadr expr) (parse-wae (cadadr expr)) (parse-wae (caddr expr)))))]
      [(symbol? expr) (id expr)])))
; bound-id:   (caadr concrete) = 'x
; named-expr: (cadadr concrete) = 5             
; bound-body: (caddr concrete) = '(+ x x)

; Takes concrete syntax, parses it into abstract syntax
; then evaluates the abstract syntax
(define eval-wae
  (lambda (expr)
    (interp-wae (parse-wae expr))))

; Test cases, all should print good
(define test-wae
  (lambda (func)
    (begin
      [test (func '1) 1]
      [test (func '{+ 9 1}) 10]
      [test (func '{- 11 1}) 10]
      [test (func '{- {+ {+ {- 11 10} 9} 1} 0}) 11]
      [test (func '{with {x 5} {+ x x}}) 10]
      [test (func '{with {x 5}
                         {with {y 10} {+ x y}}}) 15]
      [test (func '{with {x 5}
                         {with {y {+ x x}} {+ x y}}}) 15]
      [test (func '{with {x 5}
                         {with {y {+ x x}}
                               {with {x 15} {+ x y}}}}) 25]
      [test (func '{+ {- {with {x 5}
                               {with {y 10}
                                     {with {z {+ x y}} {+ {+ x y} z}}}} 10} 20}) 40]))) 
; Used for reference
(define concrete
  `(with (x 5)(+ x x)))