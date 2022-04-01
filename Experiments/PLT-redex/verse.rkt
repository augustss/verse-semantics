#lang racket
(require redex)

(define-language verse
  (k ::= number)
  (x ::= variable-not-otherwise-mentioned)
  (op ::= cop apply)
  (cop ::= gt add semi unify index)
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
  @ : e e -> e
  [(@ e_1 e_2) (apply (array e_1 e_2))]
  )
(define-metafunction verse
  seq : e ... -> e
  [(seq e ...) (semi (array e ...))]
  )

; Subtract the second list from the first (as sets).
(define-metafunction verse
  subtract : (x ...) (x ...) -> (x ...)
  [(subtract (x ...) ()) (x ...)]
  [(subtract (x_1 ... x_2 x_3 ...) (x_2 x_4 ...))
   (subtract (x_1 ... x_3 ...) (x_2 x_4 ...))
   (side-condition (not (memq (term x_2) (term (x_3 ...)))))]
  [(subtract (x_1 ...) (x_2 x_3 ...))
   (subtract (x_1 ...) (x_3 ...))])

; Variables defined in a he
(define-metafunction verse
  bvs-he : he -> (x ...)
  [(bvs-he (def h e)) (bvs-h h)]
  [(bvs-he (dbar he ...)) (x ... ...)
   (where ((x ...) ...) ((bvs-he he) ...))]
  )
; Variables that are bound in a heap
(define-metafunction verse
  bvs-h : h -> (x ...)
  [(bvs-h (var x)) (x)]
  [(bvs-h (:= x v)) (x)]
  [(bvs-h (heap h ...))
   (x ... ...)
   (where ((x ...) ...) ((bvs-h h) ...))]
  )
; Variables that have values (i.e., that are set) in the heap.
(define-metafunction verse
  svs : h -> (x ...)
  [(svs (var x)) ()]
  [(svs (:= x v)) (x)]
  [(svs (heap h ...))
   (x ... ...)
   (where ((x ...) ...) ((svs h) ...))]
  )

(define-metafunction verse
  fvs-e : e -> (x ...)
  [(fvs-e x) (x)]
  [(fvs-e k) ()]
  [(fvs-e (array e ...)) (x ... ...)
   (where ((x ...) ...) ((fvs-e e) ...))]
  [(fvs-e (=> x he)) (subtract (fvs-he he) (x))]
  [(fvs-e (op e)) (fvs-e e)]
  [(fvs-e (bar e_1 e_2)) (x_1 ... x_2 ...)
   (where (x_1 ...) (fvs-e e_1))
   (where (x_2 ...) (fvs-e e_2))]
  [(fvs-e fail) ()]
  [(fvs-e (if he_1 e_2 e_3)) (x_1 ... x_2 ... x_3 ...)
   (where (x_1 ...) (fvs-he he_1))
   (where (x_2 ...) (subtract (fvs-e e_2) (bvs-he he_1)))
   (where (x_3 ...) (fvs-e e_3))]
  [(fvs-e (for he_1 e_2)) (x_1 ... x_2 ...)
   (where (x_1 ...) (fvs-he he_1))
   (where (x_2 ...) (subtract (fvs-e e_2) (bvs-he he_1)))]
  [(fvs-e (do he)) (fvs-he he)]
  )
(define-metafunction verse
  fvs-he : he -> (x ...)
  [(fvs-he (def h e)) (x_1 ... x_2 ...)
   (where (x_1 ...) (fvs-h h))
   (where (x_2 ...) (subtract (fvs-e e) (bvs-h h)))]
  [(fvs-he (dbar he ...)) (x ... ...)
   (where ((x ...) ...) ((fvs-he he) ...))]
  )
(define-metafunction verse
  fvs-h : h -> (x ...)
  [(fvs-h (var x)) ()]
  [(fvs-h (:= x v)) (fvs-v v)]
  [(fvs-h (heap h ...)) (x ... ...)
   (where ((x ...) ...) ((fvs-h h) ...))]
  )
   
(define-metafunction verse
  fvs-v : v -> (x ...)
  [(fvs-v v) (fvs-e v)])

(module+ test
  (test-match verse he (term (def (heap) 0)))
  (test-match verse he (term (dbar (def (heap) 0) (def (heap) 1))))
  (test-match verse he (term (def (heap (var x)) (= x k))))
  (test-match verse h (term (:= a 2)))
  (test-match verse h (term (heap (var x) (:= y 1) (heap) (heap (var z)))))
  (test-equal (term (bvs-h (heap (var x) (:= y 1) (heap (var z)) (heap)))) (term (x y z)))
  (test-equal (term (svs (heap (var x) (:= y 1) (heap (var z)) (heap)))) (term (y)))
  (test-equal (term (bvs-he (def (var x) 0))) (term (x)))
  (test-equal (term (bvs-he (dbar
                             (def (heap (var x) (:= y 1)) 0)
                             (def (var z) 1))))
              (term (x y z)))
  (test-equal (term (fvs-e x)) (term (x)))
  (test-equal (term (fvs-e (array (@ f x) (@ g x)))) (term (f x g x)))
  (test-equal (term (fvs-e (bar x (@ f y)))) (term (x f y)))
  (test-equal (term (fvs-e (=> x (def (heap) (@ f x))))) (term (f)))
  (test-equal (term (fvs-he (def (var x) (@ f x)))) (term (f)))
  (test-equal (term (fvs-he (def (:= x y) (@ x z)))) (term (y z)))
  (test-equal (term (fvs-h (heap (var x) (:= y z)))) (term (z)))
  (test-equal (term (fvs-v (=> x (def (var y) (array x y z))))) (term (z)))
  (test-equal (term (fvs-e (if (def (var x) x) (++ x y) z))) (term (y z)))
  (test-equal (term (fvs-e (for (def (var x) (@ x y)) (@ x z)))) (term (y z)))
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
       (dbar L he ...))
  (H ::=
     hole
     (heap h ... H h ...))

  ;; The X contexts are a work-around for compatible-closure not working.
  ;;
  ;; XH-HE and XH-E will find a 'he' hole that can reduce.
  ;; Not reducing under lambda, nor in if/for bodies.
  (XH-HE ::=
         hole
         (dbar he ... XH-HE he ...)
         (def h XH-E))
  (XH-E ::=
        (array e ... XH-E e ...)
        (op XH-E)
        (if XH-HE e e)
        (for XH-HE e)
        (do XH-HE))
  ;; XE-HE and XE-E will find an 'e' hole that can reduce.
  ;; Not reducing under lambda, nor in if/for bodies.
  (XE-HE ::=
         (dbar he ... XE-HE he ...)
         (def h XE-E))
  (XE-E ::=
        hole
        (array e ... XE-E e ...)
        (op XE-E)
        (if XE-HE e e)
        (for XE-HE e)
        (do XE-HE))  
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
   #:domain e
   (==> (dbar he_1 ... (dbar he_2 ...) he_3 ...)
        (dbar he_1 ... he_2 ... he_3 ...)
        "Xa")
   (==> (dbar he)
        he
        "Xb")
   (==> (def h (in-hole CE fail))
        (dbar)
        "C1")
   (==> (def h (in-hole CE (bar e_1 e_2)))
        (dbar (def h (in-hole CE e_1))
              (def h (in-hole CE e_2)))
        "C2")
   (==> (def (in-hole H (var x)) (in-hole E (unify (array x v))))
        (def (in-hole H (:= x v)) (in-hole E v))
        (side-condition (disjoint (term (svs (in-hole H (:= x v)))) (term (fvs-v v))))
        "B1")
   (==> (def (in-hole H (var x)) (in-hole E (unify (array v x))))
        (def (in-hole H (:= x v)) (in-hole E v))
        (side-condition (disjoint (term (svs (in-hole H (:= x v)))) (term (fvs-v v))))
        "B2")
   (==> (def (in-hole H (var x)) (in-hole E (unify (array x (array e ...)))))
        (def (in-hole H (heap (:= x (array x_s ...)) (var x_s) ...)) (in-hole E (array (= x_s e) ...)))
        (fresh ((x_s ...) (e ...)))
        "B3")
   (==> (def (in-hole H (var x)) (in-hole E (unify (array (array e ...) x))))
        (def (in-hole H (var x)) (in-hole E (unify (array x (array e ...)))))
        "B4")
   (==> (def (in-hole H (:= x v)) e)
        (def (in-hole H (:= x v)) (substitute e x v))
        (side-condition (member (term x) (term (fvs-e e))))
        "S1")
  
  with
  [(--> (in-hole XH-E_1 a) (in-hole XH-E_1 b))
   (==> a b)]
  ))

(define he-axioms-closure
  (compatible-closure he-axioms verse+E he))

(define (disjoint l1 l2)
  (null? (set-intersect l1 l2)))

(module+ test
  (require syntax/parse/define)
  ; The he-axioms no longer operate on 'he', but 'e'.
  ; The term-do macro will wrap each 'he' in a 'do' to make it an 'e'.
  (define-syntax-parse-rule (term-do t) (term (do t)))
  (test--> he-axioms ;; X-1
           (term-do (dbar (def (heap) 1) (dbar) (def (heap) 2)))
           (term-do (dbar (def (heap) 1) (def (heap) 2))))
  (test--> he-axioms ;; X-2
           (term-do (dbar (def (heap) 1) (dbar (def (heap) 2))))
           (term-do (dbar (def (heap) 1) (def (heap) 2))))
  (test--> he-axioms ;; C1
           (term-do (def (heap (var x) (var y)) fail))
           (term-do (dbar)))
  (test--> he-axioms ;; C2
           (term-do (def (heap) (bar 1 2)))
           (term-do (dbar (def (heap) 1) (def (heap) 2))))
  (test--> he-axioms ;; B1-1
           (term-do (def (heap (var a)) (= a 5)))
           (term-do (def (heap (:= a 5)) 5)))
  (test--> he-axioms ;; B1-2
           (term-do (def (heap (var a)) (= a b)))
           (term-do (def (heap (:= a b)) b)))  
  (test-equal ;; B1-3  do NOT allow circularity
           (apply-reduction-relation he-axioms (term-do (def (heap (var a)) (= a a))))
           '())
  (test--> he-axioms ;; B2
           (term-do (def (heap (var a)) (= 5 a)))
           (term-do (def (heap (:= a 5)) 5)))
  (test--> he-axioms ;; B3
           (term-do (def (heap (var a)) (= a (array 1 2 (++ 3 4)))))
           (term-do (def (heap (heap (:= a (array x_s x_s1 x_s2)) (var x_s) (var x_s1) (var x_s2)))
                      (array (= x_s 1) (= x_s1 2) (= x_s2 (++ 3 4))))))
  (test--> he-axioms ;; B4
           (term-do (def (heap (var a)) (= (array 1 2 (++ 3 4)) a)))
           (term-do (def (heap (var a)) (= a (array 1 2 (++ 3 4))))))
  (test--> he-axioms ;; S1
           (term-do (def (:= x 5) x))
           (term-do (def (:= x 5) 5)))
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
   (==> (add (array k_1 k_2))
        (plus k_1 k_2)
        "P1")
   (==> (semi (array v e))
        e
        "P2")
   (==> (unify (array x_1 x_1))
        x_1
        "U1")
   (==> (unify (array k_1 k_1))
        k_1
        "U2")
   (==> (unify (array (array e_1 ...) (array e_2 ...)))
        (array (= e_1 e_2) ...)
        (side-condition (equal? (length (term (e_1 ...))) (length (term (e_2 ...)))))
        "U7")
   (==> (unify (array hnf_1 hnf_2))
        fail
        (side-condition (not (equal? (term (head hnf_1)) (term (head hnf_2)))))
        "UF")
   (==> (apply (array (=> x (def h e_1)) e_2))
        (do (def (heap h (var x)) (seq (= x e_2) e_1)))
        "L1")
   ;; TODO L2
   ;; L2 needs more cleverness than is in the paper.
   (==> (apply (array (array e_1 ...) e_2))
        (do (def (var i) (seq (seq (= i e_2) (= 1 (alts (count (e_1 ...))))) (index (array (array e_1 ...) i)))))
        (fresh i)
        "L2")
   (==> (index (array (array e ...) k))
        (nth (e ...) k)
        (side-condition (and (>= (term k)) (< (term k) (length (term (e ...))))))
        "I1")
   (==> (if (dbar) e_1 e_2)
        e_2
        "K1")
   (==> (if (in-hole L (def h v)) e_1 e_2)
        (do (def h e_1))
        "K2")
   (==> (for (dbar (def h v) ...) e)
        (array (do (def h e)) ...)
        "F1")
   (==> (do (def h e))
        e
        (side-condition (disjoint (term (fvs-e e)) (term (bvs-h h))))
        "D1")
   (==> (do (dbar he_1 he_2 ...))
        (bar (do he_1) (do (dbar he_2 ...)))
        "D2")
   (==> (do (dbar))
        fail
        "D3")
  with
  [(--> (in-hole XE-E_1 a) (in-hole XE-E_1 b))
   (==> a b)]
  ))

(define-metafunction verse+E
  count : (e ...) -> (k ...)
  [(count (e ...))
          ,(range (length (term (e ...))))]
  )

(define-metafunction verse+E
  nth : (e ...) k -> e
  [(nth (e ...) k) ,(list-ref (term (e ...)) (term k))]
  )

(define-metafunction verse+E
  alts : (e ...) -> e
  [(alts ()) fail]
  [(alts (e)) e]
  [(alts (e_1 e_2 ...)) (bar e_1 (alts (e_2 ...)))]
  )

(define e-he-axioms
  (union-reduction-relations e-axioms he-axioms))

; LA: This should have worked, but it doesn't.
; I've contacted Robby Findlert for help.

(define axioms
  (compatible-closure e-he-axioms verse+E e))

; Throw away all information about nested values.
; It doesn't matter what it returns as long as the different hnf heads get different results.
; Note: constants are all considered to be different.
(define-metafunction verse
  head : hnf -> hnf
  [(head k) k]
  [(head (array e ...)) (array ,(length (term (e ...))))]
  [(head (=> x he)) (=> a (def (heap) 0))]
  )

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
           (term (= x x))
           (term x))
  (test--> e-axioms ;; U2
           (term (= 5 5))
           (term 5))
  (test--> e-axioms ;; UF
           (term (= 5 6))
           (term fail))
  (test--> e-axioms ;; U7
           (term (= (array x 1 2) (array 3 y 2)))
           (term (array (= x 3) (= 1 y) (= 2 2))))
  (test--> e-axioms ;; UF
           (term (= (array x 1 2) (array 3 y 2 4)))
           (term fail))
  (test--> e-axioms ;; UF
           (term (= 5 (array 1 2 3)))
           (term fail))
; This test gets unique variables that cannot be predicted.
; We need to check alpha equivalence instead.
;  (test--> e-axioms ;; L1
;           (term (@ (=> x (def (heap) (++ x 1))) (++ 3 4)))
;           (term 0))
  (test--> e-axioms ;; K1
           (term (if (dbar) 1 2))
           (term 2))
  (test--> e-axioms ;; K2-1
           (term (if (def (heap) 3) 1 2))
           (term (do (def (heap) 1))))
  (test--> e-axioms ;; K2-2
           (term (if (dbar (def (heap) 3)) 1 2))
           (term (do (def (heap) 1))))
  (test--> e-axioms ;; F1
           (term (for (dbar (def (var x) 1) (def (var y) 2)) 5))
           (term (array (do (def (var x) 5)) (do (def (var y) 5)))))
  (test--> e-axioms ;; D1-1
           (term (do (def (heap) 5)))
           (term 5))
  (test--> e-axioms ;; D1-2
           (term (do (def (var x) y)))
           (term y))
  (test--> e-axioms ;; D2
           (term (do (dbar (def (heap) 1) (def (heap) 2))))
           (term (bar (do (def (heap) 1)) (do (dbar (def (heap) 2))))))
  (test--> e-axioms ;; D3
           (term (do (dbar)))
           (term fail))
  )

(module+ test
  (test-->> e-he-axioms
            (term (do (def (:= x 1) x)))
            1)
  (test-->> e-he-axioms
            (term (do (def (var x) (seq (= 6 x) (++ x 1)))))
            (term 7))
  (test-->> e-he-axioms
            (term (for (def (var x) (= x (bar 1 2))) (++ x 1)))
            (term (array 2 3)))
  (test-->> e-he-axioms
            (term (do (def (heap (var x) (var y)) (seq (= y (if (def (heap) (= x 1)) 111 222)) (seq (= x 1) y)))))
            (term 111))
  (test-->> e-he-axioms
            (term (do (def (heap (var x) (var y)) (seq (= y (if (def (heap) (= x 1)) 111 222)) (seq (= x 2) y)))))
            (term 222))
  ; This test is incredibly slow because of the many reduction paths.
  ;(test-->> e-he-axioms
  ;          (term (@ (array 1 2) 1))
  ;          2)
  )

; Nice examples
; (traces e-he-axioms (term (do (def (:= x 1) x))))
; (traces e-he-axioms (term (for (def (var x) (= x (bar 1 2))) (++ x 1))))

(module+ test
  (test-results))
