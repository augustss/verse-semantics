#lang racket
(require redex)
(define-language CBN
  (V (λ x M) number)
  (A V
     (let [x M] A))
  (M x
     V
     (M M)
     (let [x M] M))
  (x variable-not-otherwise-mentioned)
  #:binding-forms
  (λ x M #:refers-to x)
  (let [x M_x] M #:refers-to x)
  )

(define-metafunction CBN
  free-vars-M : M -> (x ...)
  [(free-vars-M x_1) (x_1)]
  [(free-vars-M V_1) (free-vars-V V_1)]
  [(free-vars-M (M_1 M_2)) (union (free-vars-M M_1) (free-vars-M M_2))]
  [(free-vars-M (let [x_1 M_1] M_2)) (union (free-vars-M M_1) (delete-var x_1 (free-vars-M M_2)))])
(define-metafunction CBN
  free-vars-V : V -> (x ...)
  [(free-vars-V (λ x_1 M_1)) (delete-var x_1 (free-vars-M M_1))]
  [(free-vars-V number) ()])
(define-metafunction CBN
  union : (x ...) (x ...) -> (x ...)
  [(union (x_1 ...) (x_2 ...)) (x_1 ... x_2 ...)]
  [(union (x_1 ...) ()) (x_1 ...)]
  [(union () (x_2 ...)) (x_2 ...)]
  [(union () ()) ()])
(define-metafunction CBN
  delete-var : x (x ...) -> (x ...)
  [(delete-var x_1 (x_2 ...)) (x_2 ...)
   (side-condition (not (memq (term x_1) (term (x_2 ...)))))]
  [(delete-var x_1 (x_2 ... x_1 x_3 ...)) (delete-var x_1 (x_2 ... x_3 ...))
   (side-condition (not (memq (term x_1) (term (x_3 ...)))))])
   

(define test1 (term ((λ x x)((λ z z)(λ z z)))))
(define test2 (term ((λ x x)(λ z z))))
(define test3 (term ((let [x 1] 2) x)))
(define test4 (term (let [x 3] ((let [x 1] 2) x))))
(define test5 (term (let [x 1] x)))
(define test6 (term (let [x 1] (x 2))))
(define test7 (term (let [x ((λ y 1) 2)] x)))

(define test-I (term ((λ a (1 2)) (3 4))))
(define test-let-I-1
  (term ,test-I))
(define test-let-I-2
  (term (,test-I (5 6))))
(define test-let-I-3
  (term (let [b (5 6)] ,test-I)))
(define test-let-I-4-1
  (term (let [b ,test-I] b)))
(define test-let-I-4-2
  (term (let [b ,test-I] (b (5 6)))))
(define test-V-1 (term (let [a 1] a)))
(define test-V-2 (term (let [a 1] (a 2))))
(define test-let-V-1-1
  (term ,test-V-1))
(define test-let-V-1-2
  (term (,test-V-1 (5 6))))
(define test-let-V-2-1
  (term ,test-V-2))
(define test-let-V-2-2
  (term (,test-V-2 (5 6))))
(define test-C (term ((let [a (1 2)] 3) (4 5))))
(define test-let-C-1
  (term ,test-C))
(define test-let-C-2
  (term (,test-C (6 7))))
(define test-let-A-1
  (term (let [a (let [b (1 2)] 3)] a)))
(define test-let-A-2
  (term (let [a (let [b (1 2)] 3)] (a 4))))
(define test-let-A-1-1
  (term ,test-let-A-1))
(define test-let-A-1-2
  (term (,test-let-A-1 (5 6))))
(define test-let-A-2-1
  (term ,test-let-A-2))
(define test-let-A-2-2
  (term (,test-let-A-2 (5 6))))

(define-extended-language CBN+E CBN
  (E hole
     (E M)
     (let [x M] E)
     (let [x_1 E] (in-hole E x_1))))

(define red
  (reduction-relation
   CBN+E
   #:domain M
   (--> (in-hole E ((λ x_1 M_1) M_2))
        (in-hole E (let [x_1 M_2] M_1))
        "let-I")
   (--> (in-hole E (let [x_1 V] (in-hole E_1 x_1)))
        (in-hole E (let [x_1 V] (in-hole E_1 V)))
        "let-V")
   (--> (in-hole E ((let [x_1 M_1] A) M_2))
        (in-hole E (let [x_1 M_1] (A M_2)))
        "let-C")
   (--> (in-hole E (let [x_1 (let [x_2 M_1] A_1)] (in-hole E_1 x_1)))
        (in-hole E (let [x_2 M_1] (let [x_1 A_1] (in-hole E_1 x_1))))
        "let-A")
   (--> (in-hole E (let [x_1 M_1] M_2))
        (in-hole E M_2)
        "let-GC"
        (side-condition (not (memq (term x_1) (term (free-vars-M M_2)))))
        )
   ))
