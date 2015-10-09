#lang plai
(require racket/trace)

; Patrick Walter
; Student ID: 2568215
; Project: waee Interpreter/parser

; Note on using this language:
; If you want to check out several different cases
; use the test-waee function with eval-waee as the input
; (test-wae eval-wae) will run several test cases for you
; Refer to the bottom of the source code for the test cases

; Define our new language waee
; Be careful to note that the with now expects
; a list as its first argument even if you only
; want to bind one instance, it still must be housed
; in two sets of brackets: eg. {with {{x 5}}{+ x x}}
(define-type waee
  (num (n number?))
  (binop (operation op?) (lhs waee?) (rhs waee?))
  (with (bound-list list?)(body waee?)) ; note this is a list now
  (id (name symbol?)))

(define interp-waee
  (lambda (expr)
    (type-case waee expr
      [num (n) n]
      [binop (operation l r) ((lookup (op-operation operation) ops) (interp-waee l) (interp-waee r))]
      [with (bound-list bound-body)
            (cond
              [(null? bound-list) (error 'interp-waee->with "With statement contains no bound instance")] ; empty list error
              [(ormap
                (lambda (x) (if (bind? x) #f #t)) bound-list)
               (error 'interp-waee->with "With statement needs contain a list of bound objects")] ; not a list of bind obj error
              [(ormap
                (lambda (x) (if (> (occurs bound-list (bind-name x)) 1) #t #f)) bound-list)
               (error 'interp-waee->with "With statement identifiers may only be defined once")] ; multiple identifier binding error
              [(interp-waee (foldl (lambda (b-item b-body) (subst b-body (bind-name b-item) (num (interp-waee (bind-value b-item))))) bound-body bound-list))])]
      [id (v) (error 'interp-waee->id "Free identifier")])))
;(trace interp-waee)

(define subst
  (lambda (expr sub-id val)
    (type-case waee expr
      [num (x) expr]
      [binop (operation l r) (binop operation (subst l sub-id val) (subst r sub-id val))]
      [with (bound-list bound-body)
            (with (map
                   (lambda (b) (bind (bind-name b) (subst (bind-value b) sub-id val))) bound-list)
                  (if (ormap
                       (lambda (x) (if (symbol=? (bind-name x) sub-id) #t #f)) bound-list)
                      bound-body
                      (subst bound-body sub-id val)))]
      [id (v) (if (symbol=? v sub-id) val expr)])))

(define parse-waee
  (lambda (expr)
    (cond
      [(number? expr) (num expr)]
      [(symbol? expr) (id expr)]
      [(list? expr)
       (case (car expr)
         ('+ (binop (op 'plus)(parse-waee (cadr expr))(parse-waee (caddr expr))))
         ('- (binop (op 'minus)(parse-waee (cadr expr))(parse-waee (caddr expr))))
         ('* (binop (op 'mult)(parse-waee (cadr expr))(parse-waee (caddr expr))))
         ('/ (binop (op 'div)(parse-waee (cadr expr))(parse-waee (caddr expr))))
         ('with (with (map ; list of bound instances
                       (lambda (x) (if (empty? x) (error 'parse-waee->with "With Statement has no identifiers")
                                       (bind (first x)(parse-waee (second x))))) 
                       (cadr expr))
                      (parse-waee (caddr expr)))))])))
;(trace parse-waee)

(define eval-waee
  (lambda (expr)
    (interp-waee (parse-waee expr))))

(define-type binop-rec
  (binary-op (name symbol?) (op procedure?))
  (op (operation symbol?)))

(define lookup
  (lambda (op-name op-table)
     (cond ((empty? op-table) (error 'lookup "Operator not found"))
           (else (if (symbol=? (binary-op-name (car op-table)) op-name)
                     (binary-op-op (car op-table))
                     (lookup op-name (cdr op-table)))))))

(define-type binding
  (bind (name symbol?) (value waee?)))

(define ops
  (list
   (binary-op 'plus +)
   (binary-op 'minus -)
   (binary-op 'mult *)
   (binary-op 'div /)))

; Counts the number of item in list
(define (occurs list item)
  (define (iter list result)
    (cond ((null? list) result)
          ((equal? (bind-name (car list)) item)
           (iter (cdr list) (cons item result)))
          (else (iter (cdr list) result))))
  (length (iter list '())))

; Test cases, all should print good
(define test-waee
  (lambda (func)
    (begin
      [test (func '1) 1]
      [test (func '{+ 9 1}) 10]
      [test (func '{- 11 1}) 10]
      [test (func '{- {+ {+ {- 11 10} 9} 1} 0}) 11]
      [test (func '{with {{x 5}} {+ x x}}) 10]
      [test (func '{with {{x 5}}
                         {with {{y 10}} {+ x y}}}) 15]
      [test (func '{with {{x 5}}
                         {with {{y {+ x x}}} {+ x y}}}) 15]
      [test (func '{with {{x 5}}
                         {with {{y {+ x x}}}
                               {with {{x 15}} {+ x y}}}}) 25]
      [test (func '{+ {- {with {{x 5}}
                               {with {{y 10}}
                                     {with {{z {+ x y}}} {+ {+ x y} z}}}} 10} 20}) 40]
      [test (func '{with {{x 5}{y 10}}{+ x y}}) 15]
      [test (func '{with {{x 5}{y 10}{z 15}}
                         {with {{w 20}} {+ {- {+ w z} y} x}}}) 30])))

; Error testing scenarios
; expected output: parse-waee->with: With Statement has no identifiers
(define no-id-error
  '{with {{}}{+ x x}})

;expected output: interp-waee->with: With statement contains no bound instance
(define no-binding-error
  '{with {}{+ x x}})

; expected output: interp-waee->with: With statement identifiers may only be defined once
(define repeat-id-error
  '{with {{x 5}{x 10}}{+ x 10}})

; expected output: interp-waee->id: Free identifier
(define free-id-error
  '{with { {x 5} {y {+ x 10} }}{+ x y}})






