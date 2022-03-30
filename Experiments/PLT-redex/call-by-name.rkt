#lang racket
(require redex)

(define-language CBName
  (e ::=
     k
     x
     (e e)
     (λ x e)
     (+ e e))
  (k ::= number)
  (x ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (λ x e #:refers-to x)
  )

(define-extended-language CBName+E CBName
  (E ::=
     hole
     (E e)
     (+ E e)
     (+ k E))
  )

(define red
  (reduction-relation
   CBName+E
   #:domain e
   (--> (in-hole E ((λ x e_1) e_2))
        (in-hole E (subst x e_2 e_1))
        "beta")
   (--> (in-hole E (+ k_1 k_2))
        (in-hole E (add k_1 k_2)))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions

;; Add two numbers
(define-metafunction CBName+E
  add : k k -> k
  [(add k_1 k_2) ,(+ (term k_1) (term k_2))])

(require redex/tut-subst)
;; Substitution
(define-metafunction CBName+E
  subst : x e e -> e
  [(subst x e_1 e_2)
   ,(subst/proc x? (list (term x)) (list (term e_1)) (term e_2))])
(define x? (redex-match CBName+E x))

(define test1 (term (+ 1 2)))
(define test2 (term ((λ x (+ x 1)) 2)))
(define test3 (term ((λ x (+ x x)) (+ 1 2))))
  