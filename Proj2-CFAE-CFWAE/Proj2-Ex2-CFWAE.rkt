#lang plai
(require racket/trace)

; Patrick Walter
; Student ID: 2568215
; Project 2: CFWAE Interpreter/parser and Elaborator
; Exercise 2

; This new CFWAE language is meant to ride on our previously
; defined CFAE language. In that, we have left the interpretation
; completely in the CFAE language and opted to use an elaborator
; to translate from CFWAE to CFAE
; Refer to the test cases at the bottom of the page to
; get a better grasp of how the language works

;; CFAE is copied directly from Exercise 1 ;;
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

;; ==== Defining CFWAE and Associated Algorithms ==== ;;
(define-type CFWAE
  [numCW (n number?)]
  [idCW (name symbol?)]
  [opCW (operation symbol?) (lhs CFWAE?) (rhs CFWAE?)]
  [funCW (param symbol?) (body CFWAE?)]
  [appCW (fun-expr CFWAE?) (arg-expr CFWAE?)]
  [if0CW (condition CFWAE?) (then-body CFWAE?) (else-body CFWAE?)]
  [withCW (id symbol?) (bound-expr CFWAE?) (bound-body CFWAE?)]
  [cond0CW (conditions list?) (default CFWAE?)])

(define elab-CFWAE
  (lambda (expr)
    (type-case CFWAE expr
      [numCW (n) (num n)]
      [idCW (v) (id v)]
      [opCW (oper l r) (op oper (elab-CFWAE l) (elab-CFWAE r))]
      [funCW (bound-id bound-body) (fun bound-id (elab-CFWAE bound-body))]
      [appCW (fun-expr arg-expr) (app (elab-CFWAE fun-expr) (elab-CFWAE arg-expr))]
      [if0CW (condition then-body else-body) (if0 (elab-CFWAE condition) (elab-CFWAE then-body) (elab-CFWAE else-body))]
      [withCW (id bound-expr bound-body) (app (fun id (elab-CFWAE bound-body)) (elab-CFWAE bound-expr))]
      [cond0CW (conditions default) (cond-elab conditions (elab-CFWAE default))])))

; cond-elab is meant to sift through the conditions of a cond
; to see which which of the cond's first expressions is 0
; and evaluate its second list element. Otherwise, it goes to the
; end of the list where the default case is and executes that
; NOTE the way I have implemented the cond-elab in such a way
; that it must needs a list of lists, even if there is only
; one cond and a default case. This is demonstrated in the tests
; at the bottom of this source code under the Cond Tests
(define cond-elab
  (lambda (condition default)
    (cond ((empty? condition) default)
          (else (if0 (elab-CFWAE (first (first condition))) ; check cond's condition
                     (elab-CFWAE (second (first condition))) ; first cond elavs to true
                     (cond-elab (cdr condition) default)))))) ; first cond not true, check next
      
(define eval-CFWAE
  (lambda (expr)
    (interp-CFAE (elab-CFWAE expr) prelude)))

(define prelude
  (aSub 'pi (num 3.1415)
  (aSub 'area-circle (fun 'radius (op 'mult (op 'mult (id 'radius) (id 'radius)) (id 'pi)))
  (aSub 'inc (fun 'x (op 'add (id 'x) (num 1))) (mtSub)))))

;; These are the same test cases as for Exercise 1, with some added for withCW and cond0CW ;;
; Tests Cases in BriefCases
(define message1 " -----=== Basic Tests ===-----") message1
(test (eval-CFWAE (numCW 99)) (num 99))
(test (eval-CFWAE (opCW 'add (numCW 99) (numCW 101))) (num 200))
(test (eval-CFWAE (appCW (funCW 'x (opCW 'add (idCW 'x) (numCW 99))) (numCW 1))) (num 100))
(test (eval-CFWAE (if0CW (numCW 1) (numCW 101) (numCW 1))) (num 1))
(test (eval-CFWAE (if0CW (numCW 0) (numCW 101) (numCW 1))) (num 101))

(define message2 " -----=== Nested Tests ===-----") message2
(test (eval-CFWAE (opCW 'sub (opCW 'add (numCW 99) (numCW 101)) (numCW 199))) (num 1))
(test (eval-CFWAE (opCW 'add
                     (appCW (funCW 'x (opCW 'add (idCW 'x) (numCW 99))) (numCW 1))
                     (appCW (funCW 'x (opCW 'add (idCW 'x) (numCW 99))) (numCW 1)))) (num 200))
(test (eval-CFWAE (if0CW (if0CW (numCW 0) (numCW 20) (numCW 0))
                      (numCW 101)
                      (numCW 202))) (num 202))
(test (eval-CFWAE (if0CW (opCW 'sub
                          (appCW (funCW 'x (opCW 'add (idCW 'x) (numCW 99))) (numCW 1))
                          (appCW (funCW 'x (opCW 'add (idCW 'x) (numCW 99))) (numCW 1))) ; = (num 0)
                      (opCW 'add
                          (appCW (funCW 'x (opCW 'add (idCW 'x) (numCW 99))) (numCW 1))
                          (appCW (funCW 'x (opCW 'add (idCW 'x) (numCW 99))) (numCW 1))) ; = (num 200)
                      (numCW 101))) (num 200))
(test (eval-CFWAE (if0CW (opCW 'add
                          (appCW (funCW 'x (opCW 'add (idCW 'x) (numCW 99))) (numCW 1))
                          (appCW (funCW 'x (opCW 'add (idCW 'x) (numCW 99))) (numCW 1))) ; = (num 200)
                      (opCW 'add
                          (appCW (funCW 'x (opCW 'add (idCW 'x) (numCW 101))) (numCW 1))
                          (appCW (funCW 'x (opCW 'add (idCW 'x) (numCW 99))) (numCW 1))) ; = (num 202)
                      (numCW 101))) (num 101))

(define message3 " -----=== Error Tests ===-----") message3
(test/exn (eval-CFWAE (opCW 'multiply (numCW 101) (numCW 202))) "invalid operator")
(test/exn (eval-CFWAE (opCW 'add (numCW 101) (funCW 'x (opCW 'add (idCW 'zz) (numCW 202))))) "Arthimatic operation cannot be performed on a function")
(test/exn (eval-CFWAE (appCW (funCW 'x (opCW 'add (idCW 'x) (numCW 1))) (idCW 'z))) "no binding for identifier")
(test/exn (eval-CFWAE (appCW (numCW 5) (numCW 6))) "A Num cannot be applied, Expecting a function")

(define message4 " -----=== With Tests ===-----") message4
(test (eval-CFWAE (withCW 'x (numCW 101) (opCW 'add (idCW 'x) (numCW 99)))) (num 200))
(test/exn (eval-CFWAE (withCW 'x (numCW 101) (opCW 'mult (idCW 'y) (numCW 99)))) "no binding for identifier")
(test (eval-CFWAE (withCW 'x (funCW 'y (opCW 'div (idCW 'y) (numCW 100))) (appCW (idCW 'x) (numCW 200)))) (num 2))
(test (eval-CFWAE (withCW 'add1 (funCW 'x (opCW 'add (idCW 'x) (numCW 11)))
                      (cond0CW (list (list (appCW (idCW 'add1) (numCW 0)) (numCW 55))
                            (list (numCW 33) (numCW 44))
                            (list (numCW 0) (appCW (idCW 'add1) (numCW 2))))(numCW 22)))) (num 13))

(define message5 " -----=== Cond Tests ===-----") message5
(test (eval-CFWAE (cond0CW (list (list (numCW 0) (numCW 101)) (list (numCW 1) (numCW 22))) (numCW 202))) (num 101))
(test (eval-CFWAE (cond0CW (list (list (numCW 1) (numCW 10))) (numCW 101))) (num 101))
(test (eval-CFWAE (cond0CW (list
                            (list (withCW 'x (numCW 1) (opCW 'sub (numCW 1) (idCW 'x))) (numCW 101))
                            (list (numCW 0) (numCW 10)))
                           (numCW 202))) (num 101))

(define message6 " -----=== Prelude Tests ===-----") message6
(test (eval-CFWAE (appCW (idCW 'area-circle) (numCW 1))) (num 3.1415))
(test (eval-CFWAE (appCW (idCW 'area-circle) (numCW 0))) (num 0))
(test (eval-CFWAE (appCW (idCW 'inc) (numCW 99))) (num 100))
(test (eval-CFWAE (appCW (idCW 'area-circle) (appCW (idCW 'inc) (numCW 0)))) (num 3.1415))
