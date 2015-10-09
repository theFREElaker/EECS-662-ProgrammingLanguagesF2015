#lang plai
(require racket/trace)

; Patrick Walter
; Student ID: 2568215
; Project 2: CFAE Interpreter/parser
; Exercise 1

; With the CFAE language we are introducing Deferred Substitution
; as a method to replace the need of a subst function. This essentially
; keeps a list of all identifer/value pairs to which the interpretter first
; adds to then later references during evaluation

; To become more familiar with the language and how it is used, refer
; to the test cases at the bottom of the source.

(define-type CFAE
  [num (n number?)]
  [id (name symbol?)]
  [op (operation symbol?) (lhs CFAE?) (rhs CFAE?)]
  [fun (param symbol?) (body CFAE?)]
  [app (fun-expr CFAE?) (arg-expr CFAE?)]
  [if0 (condition CFAE?) (then-body CFAE?) (else-body CFAE?)])

(define-type DefrdSub
  (mtSub)
  (aSub (name symbol?) (value CFAE?) (ds DefrdSub?)))

(define binop
  (list
   (list 'add +)
   (list 'sub -)
   (list 'mult *)
   (list 'div /)))

(define lookup-binop
  (lambda (type binop)
    (cond ((empty? binop)
           (error 'lookup-binop "invalid operator"))
          (else (if (symbol=? type (first (first binop)))
                    (cadar binop)(lookup-binop type (cdr binop)))))))
           
(define lookup
  (lambda (name ds)
    (type-case DefrdSub ds
      (mtSub () (error 'lookup "no binding for identifier"))
      (aSub (bound-name bound-value rest-ds)
            (if (symbol=? name bound-name)
              bound-value
              (lookup name rest-ds))))))

; This is used more for visibility than anything
; It makes it clear that we are only temporarily
; calculating the reult of aoperation before
; raising it back up into the abstract syntax
(define calc
  (lambda (operation lhs rhs)
    (num (operation (num-n lhs) (num-n rhs)))))

; Given an expression in CFAE format, it should intpret it
; Calculate it natively then raise it back up to the Abstract
; CFAE syntax
(define interp-CFAE
  (lambda (expr ds)
    (type-case CFAE expr
      [num (n) (num n)]
      [id (v) (lookup v ds)]
      [op (oper l r) (if (or (fun? l) (fun? r))
                         (error 'interp-CFAE->op "Arthimatic operation cannot be performed on a function")
                         (calc (lookup-binop oper binop)
                          (interp-CFAE l ds)
                          (interp-CFAE r ds)))]
      [fun (bound-id bound-body) (fun bound-id bound-body)]
      [app (fun-expr arg-expr)
           (local ((define fun-val (interp-CFAE fun-expr ds)))
             (if (num? fun-val)
                 (error 'interp-CFAE->app "A Num cannot be applied, Expecting a function")
                 (interp-CFAE (fun-body fun-val)
                              (aSub (fun-param fun-val)
                                    (interp-CFAE arg-expr ds)
                                    ds))))]
      [if0 (condition then-body else-body) (cond ((equal? (interp-CFAE condition ds) (num 0))
                                                  (interp-CFAE then-body ds))
                                                 (else (interp-CFAE else-body ds)))])))

; This automatically inserts the mtSub for the
; Desferred Substitution list
(define eval-CFAE
  (lambda (expr)
    (interp-CFAE expr (mtSub))))

; Tests Cases in BriefCases
(define message1 " -----=== Basic Tests ===-----") message1
(test (eval-CFAE (num 99)) (num 99))
(test (eval-CFAE (op 'add (num 99) (num 101))) (num 200))
(test (eval-CFAE (app (fun 'x (op 'add (id 'x) (num 99))) (num 1))) (num 100))
(test (eval-CFAE (if0 (num 1) (num 101) (num 1))) (num 1))
(test (eval-CFAE (if0 (num 0) (num 101) (num 1))) (num 101))

(define message2 " -----=== Nested Tests ===-----") message2
(test (eval-CFAE (op 'sub (op 'add (num 99) (num 101)) (num 199))) (num 1))
(test (eval-CFAE (op 'add
                     (app (fun 'x (op 'add (id 'x) (num 99))) (num 1))
                     (app (fun 'x (op 'add (id 'x) (num 99))) (num 1)))) (num 200))
(test (eval-CFAE (if0 (if0 (num 0) (num 20) (num 0))
                      (num 101)
                      (num 202))) (num 202))
(test (eval-CFAE (if0 (op 'sub
                          (app (fun 'x (op 'add (id 'x) (num 99))) (num 1))
                          (app (fun 'x (op 'add (id 'x) (num 99))) (num 1))) ; = (num 0)
                      (op 'add
                          (app (fun 'x (op 'add (id 'x) (num 99))) (num 1))
                          (app (fun 'x (op 'add (id 'x) (num 99))) (num 1))) ; = (num 200)
                      (num 101))) (num 200))
(test (eval-CFAE (if0 (op 'add
                          (app (fun 'x (op 'add (id 'x) (num 99))) (num 1))
                          (app (fun 'x (op 'add (id 'x) (num 99))) (num 1))) ; = (num 200)
                      (op 'add
                          (app (fun 'x (op 'add (id 'x) (num 101))) (num 1))
                          (app (fun 'x (op 'add (id 'x) (num 99))) (num 1))) ; = (num 202)
                      (num 101))) (num 101))

(define message3 " -----=== Error Tests ===-----") message3
(test/exn (eval-CFAE (op 'multiply (num 101) (num 202))) "invalid operator")
(test/exn (eval-CFAE (op 'add (num 101) (fun 'x (op 'add (id 'zz) (num 202))))) "Arthimatic operation cannot be performed on a function")
(test/exn (eval-CFAE (app (fun 'x (op 'add (id 'x) (num 1))) (id 'z))) "no binding for identifier")
(test/exn (eval-CFAE (app (num 5) (num 6))) "A Num cannot be applied, Expecting a function")