#lang racket
(require redex)

(define-language verse
  (k ::= number)
  (x ::= variable-not-otherwise-mentioned)
  (op ::= cop apply)
  (cop ::= gt add semi unify)
  (he ::=
      (def h e)
      (dbar he ...))
  (e ::=
     x
     k
     (array e ...)
     (=> x he)
     (op e)
     (bar e e)
     fail
     (if he e e)
     (for he e)
     (do he)
     )
  (hnf ::=
       k
       (array v ...)
       (=> x he))
  (v ::= x hnf)
  (ce ::=
      x
      k
      (array ce ...)
      (=> x he)
      (cop ce))
  (h ::=
     (heap h ...)
     (var x)
     (:= x v))
  #:binding-forms
  (=> x he #:refers-to x)
  ; TODO Test the def binding form.
  ; It's rather complex and fresh variables make testing harder.
  ; Comment it out for the moment:
  ;;; (heap h ...) #:exports (shadow h ...)
  ;;; (:= x v) #:exports x
  ;;; (var x) #:exports x
  ;;; (def h e #:refers-to h)
  ;;; (if (def h e #:refers-to h) e #:refers-to h e)
  ;;; (for (def h e #:refers-to h) e #:refers-to h)
  )

(define-metafunction verse
  = : e e -> e
  [(= e_1 e_2) (unify (array e_1 e_2))]
  )
;LA: I would like to name this +, but then the actual + in
;    meta-function plus refers to the wrong thing.
(define-metafunction verse
  ++ : e e -> e
  [(++ e_1 e_2) (add (array e_1 e_2))]
  )
(define-metafunction verse
  > : e e -> e
  [(> e_1 e_2) (gt (array e_1 e_2))]
  )
(define-metafunction verse
  seq : e ... -> e
  [(seq e ...) (semi (array e ...))]
  )
(define-metafunction verse
  bvs-he : he -> (x ...)
  [(bvs-he (def h e)) (bvs-h h)]
  )
(define-metafunction verse
  bvs-h : h -> (x ...)
  [(bvs-h (var x)) (x)]
  [(bvs-h (:= x v)) (x)]
  [(bvs-h (heap h ...))
   (x ... ...)
   (where ((x ...) ...) ((bvs-h h) ...))]
  )

(module+ test
  (test-match verse he (term (def (heap) 0)))
  (test-match verse he (term (dbar (def (heap) 0) (def (heap) 1))))
  (test-match verse he (term (def (heap (var x)) (= x k))))
  (test-match verse h (term (:= a 2)))
  (test-match verse h (term (heap (var x) (:= y 1) (heap) (heap (var z)))))
  (test-equal (term (bvs-h (heap (var x) (:= y 1) (heap (var z)) (heap)))) (term (x y z)))
  )

(define-extended-language verse+E verse
  (E ::=
     hole
     (array e ... E e ...)
     (op E))
  (CE ::=
      hole
      (array ce ... CE e ...)
      (op CE))
  (L ::=
       hole
       (dbar L he))
  (H ::=
     hole
     (heap h ... H h ...))
  )

(module+ test
  (test-match verse+E (in-hole H x) (term (heap b (:= a 1))))
  ; Sadly, no meta-function expansion in patterns.
  ; (test-match verse+E (in-hole E (= x k)) (term (= a 5)))
  (test-match verse+E (in-hole E (unify (array x k))) (term (= a 5)))
  )

;; Axioms for h-expressions
(define he-axioms
  (reduction-relation
   verse+E
   #:domain he
   (--> (dbar he_1 ... (dbar he_2 ...) he_3 ...)
        (dbar he_1 ... he_2 ... he_3 ...)
        "X")
   (--> (def h (in-hole CE fail))
        (dbar)
        "C1")
   (--> (def h (in-hole CE (bar e_1 e_2)))
        (dbar (def h (in-hole CE e_1))
              (def h (in-hole CE e_2)))
        "C2")
   (--> (def (in-hole H (var x)) (in-hole E (unify (array x k))))
        (def (in-hole H (:= x k)) (in-hole E k))
        "B1")
   (--> (def (in-hole H (var x)) (in-hole E (unify (array k x))))
        (def (in-hole H (:= x k)) (in-hole E k))
        "B2")
   (--> (def (in-hole H (var x)) (in-hole E (unify (array x (array e ...)))))
        (def (in-hole H (heap (:= x (array x_s ...)) (var x_s) ...)) (in-hole E (array (= x_s e) ...)))
        (fresh ((x_s ...) (e ...)))
        "B3")
   (--> (def (in-hole H (var x)) (in-hole E (unify (array (array e ...) x))))
        (def (in-hole H (heap (:= x (array x_s ...)) (var x_s) ...)) (in-hole E (array (= x_s e) ...)))
        (fresh ((x_s ...) (e ...)))
        "B4")
   (--> (def (in-hole H (:= x v)) e)
        (def (in-hole H (heap)) (substitute e x v))
        "S1")
   ))

(module+ test
  (test--> he-axioms ;; X-1
           (term (dbar (def (heap) 1) (dbar) (def (heap) 2)))
           (term (dbar (def (heap) 1) (def (heap) 2))))
  (test--> he-axioms ;; X-2
           (term (dbar (def (heap) 1) (dbar (def (heap) 2))))
           (term (dbar (def (heap) 1) (def (heap) 2))))
  (test--> he-axioms ;; C1
           (term (def (heap (var x) (var y)) fail))
           (term (dbar)))
  (test--> he-axioms ;; C2
           (term (def (heap) (bar 1 2)))
           (term (dbar (def (heap) 1) (def (heap) 2))))
  (test--> he-axioms ;; B1
           (term (def (heap (var a)) (= a 5)))
           (term (def (heap (:= a 5)) 5)))
  (test--> he-axioms ;; B2
           (term (def (heap (var a)) (= 5 a)))
           (term (def (heap (:= a 5)) 5)))
  (test--> he-axioms ;; B3
           (term (def (heap (var a)) (= a (array 1 2 3))))
           (term (def (heap (heap (:= a (array x_s x_s1 x_s2)) (var x_s) (var x_s1) (var x_s2)))
                      (array (= x_s 1) (= x_s1 2) (= x_s2 3)))))
  (test--> he-axioms ;; B4
           (term (def (heap (var a)) (= (array 1 2 3) a)))
           (term (def (heap (heap (:= a (array x_s x_s1 x_s2)) (var x_s) (var x_s1) (var x_s2)))
                      (array (= x_s 1) (= x_s1 2) (= x_s2 3)))))
  (test--> he-axioms ;; S1
           (term (def (:= x 5) x))
           (term (def (heap) 5)))
  )

;; Flatten the heap structure.
;; Not really needed.
;;;(define h-axioms
;;;  (reduction-relation
;;;   verse
;;;   #:domain h
;;;   (--> (heap h_1 ... (heap h_2 ...) h_3 ...)
;;;        (heap h_1 ... h_2 ... h_3 ...)
;;;        "heap-merge")))

; Axioms for expressions
(define e-axioms
  (reduction-relation
   verse+E
   #:domain e
   (--> (add (array k_1 k_2))
        (plus k_1 k_2)
        "P1")
   (--> (semi (array v e))
        e
        "P2")
   (--> (unify (array (array e_1 ...) (array e_2 ...)))
        (array (= e_1 e_2) ...)
        (side-condition (equal? (length (term (e_1 ...))) (length (term (e_2 ...)))))
        "U1")
   (--> (unify (array (array e_1 ...) (array e_2 ...)))
        fail
        (side-condition (not (equal? (length (term (e_1 ...))) (length (term (e_2 ...))))))
        "U1F")
   ))

(define-metafunction verse
  plus : k k -> k
  [(plus k_1 k_2) ,(+ (term k_1) (term k_2))])

(module+ test
  (test--> e-axioms ;; P1
           (term (++ 3 4))
           (term 7))
  (test--> e-axioms ;; P2
           (term (seq 5 (++ x y)))
           (term (++ x y)))
  (test--> e-axioms ;; U1
           (term (= (array x 1 2) (array 3 y 2)))
           (term (array (= x 3) (= 1 y) (= 2 2))))
  (test--> e-axioms ;; U1F
           (term (= (array x 1 2) (array 3 y 2 4)))
           (term fail))
  )

(module+ test
  (test-results))
