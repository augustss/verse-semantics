#lang racket
(require redex)

(define-language CBNeed
  (e ::=
     k
     x
     (e e)
     (λ x e)
     (+ e e)
     (let [x e] e))
  (k ::= number)
  (x ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (λ x e #:refers-to x)
  (let [x e_x] e #:refers-to x)
  )

(define-extended-language CBNeed+Ev CBNeed
  (E ::=
     hole
     (E e)
     (+ E e)
     (+ k E)
     (let [x E] (in-hole E x)))
  (v ::=
     k
     x
     (λ x e))
  )

(define red
  (reduction-relation
   CBNeed+Ev
   #:domain e
   (--> (in-hole E ((λ x e_1) e_2))
        (in-hole E (let [x e_2] e_1))
        "beta")
   (--> (in-hole E (let [x v] e))
        (in-hole E (subst x v e))
        "let-V")
   (--> (in-hole E ((let [x e_x] e_1) e_2))
        (in-hole E (let [x e_x] (e_1 e_2)))
        "let-C")
   (--> (in-hole E (let [x_1 (let [x_2 e_2] e_1)] e_3))
        (in-hole E (let [x_2 e_2] (let [x_1 e_1] e_3)))
        "let-A")
   (--> (in-hole E (+ k_1 k_2))
        (in-hole E (add k_1 k_2))
        "delta")
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions

;; Add two numbers
(define-metafunction CBNeed+Ev
  add : k k -> k
  [(add k_1 k_2) ,(+ (term k_1) (term k_2))])

(require redex/tut-subst)
;; Substitution
(define-metafunction CBNeed+Ev
  subst : x v e -> e
  [(subst x v e)
   ,(subst/proc x? (list (term x)) (list (term v)) (term e))])
(define x? (redex-match CBNeed+Ev x))


(define test1 (term (+ 1 2)))
(define test2 (term ((λ x (+ x 1)) 2)))
(define test3 (term ((λ x (+ x x)) (+ 3 4))))
