#lang racket
(require redex)
; (check-redundancy #t)

(define-language verse
  (p ::= e)
  (e ::=
     v
     (= e e)
     (seq e ...)
     (op e)
     (bar e ...)
     (if e e e)
     (for e e)
     (def q h e)  ;; See comment for binding forms.
     wrong
     )
  (op ::= cop apply)
  (cop ::= gt add mul int)
  (hnf ::=
       k
       (arr v ...)
       (=> x e)
       (rec x e)  ;; rec x e = fix (x => e)
       )
  (v ::= x hnf)
  (ce ::=
      v
      (= ce ce)
      (seq ce ...)
      (cop ce)
      )
  (h ::=
     (heap h ...)
     x
     (:= x v))
  (k ::= number)
  (x ::= variable-not-otherwise-mentioned)
  (q ::= r i)  ;; what kind of def regular or if/for
  #:binding-forms
  ;; The binding forms require non-overlapping patterns
  ;; (according to Robby Findler).
  ;; The def construct binds differently in an if/for and otherwise.
  ;; To accound for the (def r ...) is used for regular defs
  ;; and (def i ...) for defs in an if/for.
  ;; Most rules just match with (def q ...) and work for both,
  ;; but the binding forms distinguish the two.
  (=> x e #:refers-to x)
  (rec x e #:refers-to x)
  (heap h ...) #:exports (shadow h ...)
  (:= x v) #:exports x
  x #:exports x
  (def r h #:refers-to h e #:refers-to h)             ;; h variables just bound in e
  (def i h #:refers-to h e #:refers-to h) #:exports h ;; h variables accessible
  ; XXX I don't know how to encode the fact that all (def i h ...) in a 'for'
  ; should bind the same variables.  Instead, the 'for' loop does not respect bindings properly.
  ;(bar (def i h e_1 #:refers-to h) e_2 ...) #:exports h
  (if e_1 e_2 #:refers-to e_1 e_3)
  (for e_1 e_2 #:refers-to e_1)
  )

;LA: I would like to name this +, but then the actual + in
;    meta-function plus refers to the wrong thing.
(define-metafunction verse
  ++ : e e -> e
  [(++ e_1 e_2) (add (array e_1 e_2))]
  )
(define-metafunction verse
  ** : e e -> e
  [(** e_1 e_2) (mul (array e_1 e_2))]
  )
(define-metafunction verse
  >> : e e -> e
  [(>> e_1 e_2) (gt (array e_1 e_2))]
  )
(define-metafunction verse
  @ : e e -> e
  [(@ e_1 e_2) (apply (array e_1 e_2))]
  )
;(define-metafunction verse
;  seq : e ... -> e
;  [(seq e ...) (semi (array e ...))]
;  )


;; Arrays are in ANF form, the array metafunction
;; does this conversion.
;; Inventing new variables can only(?) be done in rules
;; so we introduce a sublanguage with some rules to do the conversion.
(define-metafunction verse
  array : e ... -> e
  [(array v ...) (arr v ...)]
  [(array e ...) ,(anf-array (term (xarray () () () (e ...))))]
  )

(define-extended-language verse+xarray verse
  (ex ::= (xarray (h ...) (e ...) (v ...) (e ...))))
(define anf-array-red
  (reduction-relation
   verse+xarray
   ; #:domain ex
   ; #:codomain e
   (--> (xarray (h ...) (e ...) (v ...) ())
        (def r (heap h ...) (seq e ... (arr v ...))))
   (--> (xarray (h ...) (e_1 ...) (v_1 ...) (v_2 e_2 ...))
        (xarray (h ...) (e_1 ...) (v_1 ... v_2) (e_2 ...)))
   (--> (xarray (h ...) (e_1 ...) (v_1 ...) (e_2 e_3 ...))
        (xarray (h ... a) (e_1 ... (= a e_2)) (v_1 ... a) (e_3 ...))
        (side-condition (not (redex-match verse v (term e_2))))
        (fresh a))
   ))
(define (anf-array a)
  (let ((b (car (apply-reduction-relation anf-array-red a))))
    (if
     (redex-match? verse+xarray ex b)
     (anf-array b)
     b)))

; Subtract the second list from the first (as sets).
(define-metafunction verse
  subtract : (x ...) (x ...) -> (x ...)
  [(subtract (x ...) ()) (x ...)]
  [(subtract (x_1 ... x_2 x_3 ...) (x_2 x_4 ...))
   (subtract (x_1 ... x_3 ...) (x_2 x_4 ...))
   (side-condition (not (memq (term x_2) (term (x_3 ...)))))]
  [(subtract (x_1 ...) (x_2 x_3 ...))
   (subtract (x_1 ...) (x_3 ...))])

(define-metafunction verse
  bvs-e : e -> (x ...)
  [(bvs-e (def i h e)) (bvs-h h)]
  [(bvs-e e) ()]
  )

; Variables that are bound in a heap
(define-metafunction verse
  bvs-h : h -> (x ...)
  [(bvs-h x) (x)]
  [(bvs-h (:= x v)) (x)]
  [(bvs-h (heap h ...))
   (x ... ...)
   (where ((x ...) ...) ((bvs-h h) ...))]
  )
; Variables that have values (i.e., that are set) in the heap.
(define-metafunction verse
  vvs : h -> (x ...)
  [(vvs x) ()]
  [(vvs (:= x v)) (x)]
  [(vvs (heap h ...))
   (x ... ...)
   (where ((x ...) ...) ((vvs h) ...))]
  )

(define-metafunction verse
  fvs-e : e -> (x ...)
  [(fvs-e v) (fvs-v v)]
  [(fvs-e (= e_1 e_2)) (x_1 ... x_2 ...)
   (where (x_1 ...) (fvs-e e_1))
   (where (x_2 ...) (fvs-e e_2))]
  [(fvs-e (seq e ...)) (x ... ...)
   (where ((x ...) ...) ((fvs-e e) ...))]
  [(fvs-e (op e)) (fvs-e e)]
  [(fvs-e (bar e ...)) (x ... ...)
   (where ((x ...) ...) ((fvs-e e) ...))]
  [(fvs-e (if e_1 e_2 e_3)) (x_1 ... x_2 ... x_3 ...)
   (where (x_1 ...) (fvs-e e_1))
   (where (x_2 ...) (subtract (fvs-e e_2) (bvs-e e_1)))
   (where (x_3 ...) (fvs-e e_3))]
  [(fvs-e (for e_1 e_2)) (x_1 ... x_2 ...)
   (where (x_1 ...) (fvs-e e_1))
   (where (x_2 ...) (subtract (fvs-e e_2) (bvs-e e_1)))]
  [(fvs-e (def q h e)) (x_1 ... x_2 ...)
   (where (x_1 ...) (fvs-h h))
   (where (x_2 ...) (subtract (fvs-e e) (bvs-h h)))]
  [(fvs-e wrong) ()]
  )
(define-metafunction verse
  fvs-h : h -> (x ...)
  [(fvs-h x) ()]
  [(fvs-h (:= x v)) (fvs-v v)]
  [(fvs-h (heap h ...)) (x ... ...)
   (where ((x ...) ...) ((fvs-h h) ...))]
  )
   
(define-metafunction verse
  fvs-v : v -> (x ...)
  [(fvs-v x) (x)]
  [(fvs-v k) ()]
  [(fvs-v (arr v ...)) (x ... ...)
   (where ((x ...) ...) ((fvs-v v) ...))]
  [(fvs-v (=> x e)) (subtract (fvs-e e) (x))]
  [(fvs-v (rec x e)) (subtract (fvs-e e) (x))]
  )

(module+ test
  (test-match verse e (term (def r (heap) 0)))
  (test-match verse e (term (bar (def r (heap) 0) (def r (heap) 1))))
  (test-match verse e (term (def r (heap x) (= x k))))
  (test-match verse h (term (:= a 2)))
  (test-match verse h (term (heap x (:= y 1) (heap) (heap z))))
  (test-equal (term (bvs-h (heap x (:= y 1) (heap z) (heap)))) (term (x y z)))
  (test-equal (term (vvs (heap x (:= y 1) (heap z) (heap)))) (term (y)))
  (test-equal (term (bvs-e (def i x 0))) (term (x)))
;  (test-equal (term (bvs-e (bar
;                             (def r (heap x (:= y 1)) 0)
;                             (def r z 1))))
;              (term (x y z)))
  (test-equal (term (fvs-e x)) (term (x)))
  (test-equal (term (fvs-e 5)) (term ()))
  (test-equal (term (fvs-e (= x y))) (term (x y)))
  (test-equal (term (fvs-e (seq (@ f x) (@ g x)))) (term (f x g x)))
  (test-equal (term (fvs-e (array (@ f x) (@ g x)))) (term (f x g x)))
  (test-equal (term (fvs-e (bar x (@ f y)))) (term (x f y)))
  (test-equal (term (fvs-e (=> x (def r (heap) (@ f x))))) (term (f)))
  (test-equal (term (fvs-e (def r x (@ f x)))) (term (f)))
  (test-equal (term (fvs-e (def r (:= x y) (@ x z)))) (term (y z)))
  (test-equal (term (fvs-h (heap x (:= y z)))) (term (z)))
  (test-equal (term (fvs-e (=> x (def r y (array x y z))))) (term (z)))
  (test-equal (term (fvs-e (if (def i x x) (++ x y) z))) (term (y z)))
  (test-equal (term (fvs-e (for (def i x (@ x y)) (@ x z)))) (term (y z)))
  )

(define-extended-language verse+E verse
  (X ::=
     hole
     (= X e)
     (= e X)
     (seq e ... X e ...)
     (op X)
     (def q h X)
     )
  (CX ::=
      hole
      (= CX e)
      (= e CX)
      (seq e ... CX e ...)
      (op CX)
      (def q h CX)
      )
  (L ::=
       hole
       (bar L e ...))
  (H ::=
     hole
     (heap h ... H h ...))
  ;; E will find an 'e' hole that can reduce.
  ;; Not reducing under lambda, nor in if/for bodies.
  (E ::=
        hole
        (= E e)
        (= e E)
        (seq e ... E e ...)
        (op E)
        (if E e e)
        (for E e)
        (bar e ... E e ...)
        (def q h E)
        )  
  )

(define-metafunction verse+E
  bvs-X : X -> (x ...)
  [(bvs-X hole) ()]
  [(bvs-X (def q h X)) (x_1 ... x_2 ...)
   (where (x_1 ...) (bvs-h h))
   (where (x_2 ...) (bvs-X X))]
  [(bvs-X (= X e)) (bvs-X X)]
  [(bvs-X (= e X)) (bvs-X X)]
  [(bvs-X (seq e_1 ... X e_2 ...)) (bvs-X X)]
  [(bvs-X (op X)) (bvs-X X)]
  )
   

(module+ test
  (test-match verse+E (in-hole H x) (term (heap b (:= a 1))))
  (test-match verse+E (in-hole X (= x k)) (term (= a 5)))
  )

;; Axioms for h-expressions
(define e-axioms
  (reduction-relation
   verse+E
   #:domain e
   ;; Substitution
   (--> (def q (in-hole H (:= x v)) e)
        (def q (in-hole H (:= x v)) (substitute e x v))
        (side-condition (member (term x) (term (fvs-e e))))
        "Subst")
   ;; Choice
   (--> (bar e_1 ... (bar e_2 ...) e_3 ...)
        (bar e_1 ... e_2 ... e_3 ...)
        "Bar-assoc")
;   (--> (bar e)
;        e
;        "Bar-sing")
   (--> (in-hole X (bar))
        (bar)
        (side-condition (not (equal? (term X) (term hole))))
        "Fail")
   (--> (in-hole CX (bar e ...))
        (bar (in-hole CX e) ...)
        (side-condition (not (equal? (term CX) (term hole))))
        "Choose")
   ;; Primitive operations
   (--> (add (arr k_1 k_2))
        (plus k_1 k_2)
        "P-add")
   (--> (mul (arr k_1 k_2))
        (times k_1 k_2)
        "P-mul")
   (--> (gt (arr k_1 k_2))
        k_1
        (side-condition (> (term k_1) (term k_2)))
        "P-gt1")
   (--> (gt (arr k_1 k_2))
        (bar)
        (side-condition (not (> (term k_1) (term k_2))))
        "P-gt2")
   (--> (int k)
        k
        "P-int1")
   (--> (int v)
        (bar)
        (side-condition (not (redex-match? verse k (term v))))
        "P-int2")
   ;; Sequencing
   (--> (seq v ... e)
        e
        "Seq")
   (--> (op (seq e_1 e_2))
        (seq e_1 (op e_2))
        "Op-seq")
   (--> (= (seq e_1 e_2) e_3)
        (seq e_1 (= e_2 e_3))
        "Unify-seql")
   (--> (= v_1 (seq e_2 e_3))
        (seq e_2 (= v_1 e_3))
        "Unify-seqr")
   ;; Lambda and applications
   (--> (apply (arr (=> x e_1) e_2))
        (def r t (seq (= t e_2) (substitute e_1 x t)))
        (fresh t)
        "App-lam")
   (--> (apply (arr (rec x e_1) e_2))
        (apply (arr (substitute e_1 x (rec x e_1)) e_2))
        (fresh t)
        "App-rec")
   (--> (apply (arr (arr v ...) k))
        (nth (v ...) k)
        (side-condition (and (>= (term k) 0) (< (term k) (length (term (v ...))))))
        "App-arr1")
   (--> (apply (arr (arr v ...) k))
        (bar)
        (side-condition (not (and (>= (term k) 0) (< (term k) (length (term (v ...)))))))
        "App-arr2")

   ;; Conditionals
   ;; If-true2 only needed when the 'if' does not have a 'def'
   (--> (if (bar) e_1 e_2)
        e_2
        "If-false")
   (--> (if (in-hole L (def i h v)) e_1 e_2)
        (def r h e_1)
        "If-true1")
   (--> (if (in-hole L v) e_1 e_2) ;; missing def
        e_1
        "If-true2")
   ;; For-loops
   ;; For2 only needed when the 'for' does not have a 'def'
   ;; For3 only needed when the 'for' does not have a 'bar'
   ;; For4 only needed when the 'for' does not have a 'bar' nor 'def'
   (--> (for (bar (def i h v) ...) e)
        (def r (heap t ...) (seq (= t (def r h e)) ... (arr t ...)))
        (fresh ((t ...) (v ...)))
        "For1")
   (--> (for (bar v ...) e) ;; missing def
        (def r (heap t ...) (seq (= t e) ... (arr t ...)))
        (fresh ((t ...) (v ...)))
        "For2")
   (--> (for (def i h v) e)
        (def r (heap t) (seq (= t (def r h e)) (arr t)))
        (fresh t)
        "For3")
   (--> (for v e)
        (def r (heap t) (seq (= t e) (arr t)))
        (fresh t)
        "For4")
   ;; Def blocks
   (--> (in-hole X (def q h e))
        (in-hole X e)
        (side-condition (not (equal? (term X) (term hole))))
        (side-condition (disjoint (term (fvs-e e)) (term (bvs-h h))))
        "Def-elim")
;   (--> (def r e)
;        e
;        (side-condition (not (equal? (term X) (term hole))))
;        (side-condition (disjoint (term (fvs-e e)) (term (bvs-h h))))
;        "Do-def")
   ;; Unification
   (--> (def q (in-hole H x) (in-hole X (= x v)))
        (def q (in-hole H (:= x v)) (in-hole X v))
        (side-condition (disjoint (term (fvs-v v)) (term (vvs (in-hole H (:= x v))))))
        (side-condition (disjoint (term (fvs-v v)) (term (bvs-X X))))
        "Bind")
   (--> (def q (in-hole H x) (in-hole X (= x v)))
        (def q (in-hole H (heap x y)) (in-hole X (seq (= x_1 y) (= x (substitute v z y)))))
        ;; z \in (fvs-v v) and z \in (bvs-X X)
        (where (x_1 x_2 ...) (intersect (fvs-v v) (bvs-X X)))
        ; (side-condition (not (redex-match? verse x v)))
        (fresh y)
        "Promote")
;   (--> (def q (in-hole H x) (in-hole X (= v x)))
;        (def q (in-hole H x) (in-hole X (= x v)))
;        (side-condition (not (member (term v) (term (vvs (in-hole H x))))))
;        (side-condition (not (member (term v) (term (bvs-X X)))))
;        (side-condition (not (redex-match? verse x (term v))))
;        "Swap")
   (--> (= v x)
        (= x v)
        (side-condition (not (redex-match? verse x (term v))))
        "Swap")
   (--> (= x_1 x_1)
        x_1
        "Uvar")
   (--> (= k_1 k_1)
        k_1
        "Ucon")
   (--> (= (arr v_1 ...) (arr v_2 ...))
        (seq (= v_1 v_2) ... (arr v_1 ...))
        (side-condition (equal? (length (term (v_1 ...))) (length (term (v_2 ...)))))
        "Utup")
   (--> (= hnf_1 hnf_2)
        (bar)
        (side-condition (not (equal? (term (head hnf_1)) (term (head hnf_2)))))
        "UX")  
  ))

(define-metafunction verse
  plus : k k -> k
  [(plus k_1 k_2) ,(+ (term k_1) (term k_2))])

(define-metafunction verse
  times : k k -> k
  [(times k_1 k_2) ,(* (term k_1) (term k_2))])

(define-metafunction verse
  nth : (e ...) k -> e
  [(nth (e ...) k) ,(list-ref (term (e ...)) (term k))]
  )

(define-metafunction verse
  head : hnf -> hnf
  [(head k) k]
  [(head (arr v ...)) (arr ,(length (term (v ...))))]
  [(head (=> x he)) (=> a 0)]
  )

(define-metafunction verse
  intersect : (x ...) (x ...) -> (x ..._)
  [(intersect (x_1 ...) (x_2 ...)) ,(set-intersect (term (x_1 ...)) (term (x_2 ...)))]
  )

(define (disjoint l1 l2)
  (null? (set-intersect l1 l2)))

(module+ test
  (define e-axioms-coverage (make-coverage e-axioms))
  (relation-coverage (list e-axioms-coverage))
  )

(define (bad-num? v)
  (not (or (redex-match? verse k v) (redex-match? verse x v))))

;; Axioms covering WRONG
(define w-axioms
  (reduction-relation
   verse+E
   #:domain e
   ;; Ill-typed expressions that generate WRONG
   (--> (add (arr v_1 v_2))
        wrong
        (side-condition (or (bad-num? (term v_1)) (bad-num? (term v_2))))
        "P-add-wrong")
   (--> (mul (arr v_1 v_2))
        wrong
        (side-condition (or (bad-num? (term v_1)) (bad-num? (term v_2))))
        "P-mul-wrong")
   (--> (gt (arr v_1 v_2))
        wrong
        (side-condition (or (bad-num? (term v_1)) (bad-num? (term v_2))))
        "P-gt-wrong")
   (--> (apply (arr k e))
        wrong
        "App-wrong")
; Propagation of WRONG conflicts with propagation of FAIL
;   ;; Propagation of WRONG
;   (--> (arr v_1 ... wrong v_2 ...)
;        wrong
;        "Arr-wrong")
;   (--> (op wrong)
;        wrong
;        "Op-wrong")
;   (--> (= wrong e)
;        wrong
;        "UnifyL-wrong")
;   (--> (= e wrong)
;        wrong
;        "UnifyR-wrong")
;   (--> (bar wrong e ...)  ;; XXX wrong anywhere?
;        wrong
;        "Choice-wrong")
;   (--> (seq e_1 ... wrong e_2 ...) ;; This will conflict with fail!
;        wrong
;        "Seq-wrong")
;   (--> (def h wrong)
;        wrong
;        "Def-wrong")
   ;; FAIL propagation happens in an X context.
   ;; if/for are not part of X, so they can propagate
   (--> (if wrong e_1 e_2)
        wrong
        "If-wrong")
   (--> (for wrong e)
        wrong
        "For-wrong")
  )
)

;; Is there a wrong in a position that needs reduction?
(define (wrong-expr? e)
  (redex-match? verse+E (in-hole E wrong) e))

(define (alpha? a b) (alpha-equivalent? verse a b))
(define (atest--> l a b) (test--> l #:equiv alpha? a b))
(define (atest-->> l a b) (test-->> l #:equiv alpha? a b))

(module+ test
  ;; Substitution
  (atest--> e-axioms ;; Subst
           (term (def r (:= x 5) x))
           (term (def r (:= x 5) 5)))
  ;; Choice
  (test--> e-axioms ;; Bar-assoc-1
           (term (bar 1 (bar) 2))
           (term (bar 1 2)))
  (test--> e-axioms ;; Bar-assoc-2
           (term (bar 1 (bar 2)))
           (term (bar 1 2)))
  (test--> e-axioms ;; Fail
           (term (def r (heap x y) (bar)))
           (term (bar)))
  (test--> e-axioms ;; Choice
           (term (def i (heap) (bar 1 2)))
           (term (bar (def i (heap) 1) (def i (heap) 2))))
  ;; Primitive operations
  (test--> e-axioms ;; P-add
           (term (++ 3 4))
           (term 7))
  (test--> e-axioms ;; P-mul
           (term (** 3 4))
           (term 12))
  (test--> e-axioms ;; P-gt1
           (term (>> 5 4))
           (term 5))
  (test--> e-axioms ;; P-gt2
           (term (>> 3 4))
           (term (bar)))
  (test--> e-axioms ;; P-int1
           (term (int 5))
           (term 5))
  (test--> e-axioms ;; P-int2
           (term (int (arr 5)))
           (term (bar)))
  ;; Floating
  (test-->> e-axioms ;; Seq
           (term (seq 5 10))
           (term 10))
  (test--> e-axioms ;; Op-seq
           (term (add (seq x y)))
           (term (seq x (add y))))
  (test--> e-axioms ;; Unify-seql
           (term (= (seq x y) z))
           (term (seq x (= y z))))
  (test--> e-axioms ;; Unify-seqr
           (term (= x (seq y z)))
           (term (seq y (= x z))))
  ;; Lambda and applications
  (test--> e-axioms ;; App-lam
           (term (@ (=> a (++ a 1)) 5))
           (term (def r t (seq (= t 5) (++ t 1)))))
  (atest--> e-axioms ;; App-rec
           (term (@ (rec a (=> n (arr a n))) 5))
           (term (@ (=> n (arr (rec a (=> n (arr a n))) n)) 5)))
  (test--> e-axioms ;; App-arr1
           (term (@ (array 10 20 30 40) 2))
           (term 30))
  (test--> e-axioms ;; App-arr2
           (term (@ (array 10 20 30 40) 5))
           (term (bar)))
  ;; Conditionals
  (test--> e-axioms ;; If-false
           (term (if (bar) 1 2))
           (term 2))
  (test--> e-axioms ;; If-true1-1
           (term (if (def i (heap) 3) 1 2))
           (term (def r (heap) 1)))
  (test--> e-axioms ;; If-true1-2
           (term (if (bar (def i (heap) 3) (def i (heap) 4)) 1 2))
           (term (def r (heap) 1)))
  (test--> e-axioms ;; If-true2
           (term (if 3 1 2))
           (term 1))
  ;; For-loops
  (test--> e-axioms ;; For1-1
           (term (for (bar) 0))
           (term (def r (heap) (seq (arr)))))
  (test--> e-axioms ;; For1-2
           (term (for (bar (def i x 5)) 0))
           (term (def r (heap t) (seq (= t (def r x 0)) (arr t)))))
  (test--> e-axioms ;; For2
           (term (for (bar 5) 0))
           (term (def r (heap t) (seq (= t 0) (arr t)))))
  (atest--> e-axioms ;; For3
           (term (for (def i x 5) 0))
           (term (def r (heap t) (seq (= t (def r x 0)) (arr t)))))
  (test--> e-axioms ;; For4
           (term (for 5 0))
           (term (def r (heap t) (seq (= t 0) (arr t)))))
  ;; Do blocks
  (test--> e-axioms ;; Do-def
           (term (add (def r (:= x 1) y)))
           (term (add y)))
  ;; Unification
  (atest--> e-axioms ;; Bind-1
           (term (def r (heap a) (= a 5)))
           (term (def r (heap (:= a 5)) 5)))
  (atest--> e-axioms ;; Bind-2
           (term (def r (heap a) (= a b)))
           (term (def r (heap (:= a b)) b)))  
  (test-equal ;; Bind-3  do NOT allow circularity
           (apply-reduction-relation e-axioms (term (def r (heap a) (= a a))))
           '())
  (atest--> e-axioms
           (term (def r a (def r b (= a (arr 1 b)))))
           (term (def r (heap a y) (def r b (seq (= b y) (= a (arr 1 b)))))))
;  (test--> e-axioms ;; Swap
;           (term (def r (heap a) (= 5 a)))
;           (term (def r (heap a) (= a 5))))
  (test--> e-axioms ;; Swap
           (term (= 5 a))
           (term (= a 5)))
  (test--> e-axioms ;; Uvar
           (term (= x x))
           (term x))
  (test--> e-axioms ;; Ucon
           (term (= 5 5))
           (term 5))
  (test--> e-axioms ;; Utup
           (term (= (arr x 1 2) (arr 3 y 2)))
           (term (seq (= x 3) (= 1 y) (= 2 2) (arr x 1 2))))
  (test--> e-axioms ;; UX
           (term (= 5 6))
           (term (bar)))
  (test--> e-axioms ;; UX
           (term (= (arr x 1 2) (arr 3 y 2 4)))
           (term (bar)))
  (test--> e-axioms ;; UX
           (term (= 5 (array 1 2 3)))
           (term (bar)))
  )


;;; This contains code that will be used when apply unifies its argument with the domain.
;(define e-axioms
;  (reduction-relation
;   verse+E
;   #:domain he
;   (--> (apply (array (array e_1 ...) e_2))
;        (do (def i (seq (= i e_2) (= i (alts (count (e_1 ...)))) (index (array (array e_1 ...) i)))))
;        (fresh i)
;        "App-arr")
;   (--> (index (array (array e ...) k))
;        (nth (e ...) k)
;        (side-condition (and (>= (term k)) (< (term k) (length (term (e ...))))))
;        "Idx")
;  ))
;
;(define-metafunction verse+E
;  count : (e ...) -> (k ...)
;  [(count (e ...))
;          ,(range (length (term (e ...))))]
;  )
;
;(define-metafunction verse+E
;  alts : (e ...) -> e
;  [(alts ()) fail]
;  [(alts (e)) e]
;  [(alts (e_1 e_2 ...)) (bar e_1 (alts (e_2 ...)))]
;  )
;

;; This rule gets rid of an unused def at the top level of a program.
(define top-def
  (reduction-relation
   verse+E
   #:domain e
   (--> (def q h e)
        e
        (side-condition (disjoint (term (fvs-e e)) (term (bvs-h h))))
        "Top-def")
   ))
   

(define e-axioms*
  (context-closure (union-reduction-relations e-axioms w-axioms) verse+E E))

(define p-axioms
  (union-reduction-relations e-axioms* top-def))

(module+ test
  (test-->> p-axioms
            (term (def r (:= x 1) x))
            (term 1))
  (test-->> p-axioms
            (term (def r x (seq (= 6 x) (++ x 1))))
            (term 7))
  (test-->> p-axioms
            (term (def r (heap x y) (seq (= y (if (def i (heap) (= x 1)) 111 222)) (seq (= x 1) y))))
            (term 111))
  (test-->> p-axioms
            (term (def r (heap x y) (seq (= y (if (def i (heap) (= x 1)) 111 222)) (seq (= x 2) y))))
            (term 222))
; Using test-->> is better, but very slow.
  (test-->>E p-axioms
            (term (for (def i x (= x (bar 1 2))) (++ x 1)))
            (term (array 2 3)))
  (test-->> p-axioms
            (term (@ (array 1 2) 1))
            (term 2))
  (test-->> p-axioms
            (term (@ (=> x (++ x 1)) (++ 2 3)))
            (term 6))
  )

(module+ test
  (test-->> p-axioms ;; Example-1
            (term (def r (heap x y) (seq (= x 3) (= y (++ x 1)))))
            (term 4))
  (test-->> p-axioms ;; Example-2
            (term (def r (heap x y) (seq (= y (++ x 1)) (= x 3) y)))
            (term 4))
  (atest-->> p-axioms ;; Example-3
            (term (def r (heap x y) (seq (= y (++ x 1)) (bar (= x 3) (= x 77)))))
            (term (bar
                   (def r (heap (:= x 3) (:= y 4)) 3)
                   (def r (heap (:= x 77) (:= y 78)) 77))))
  (test-->>E p-axioms ;; simple recusion
             (term
              (def r (:= f (rec g (=> n (if (= n 0) 0 (++ n -1)))))
                (@ f 1)))
             (term 0))
  (test-->>E p-axioms ;; factorial
             (term
              (def r (heap fac)
                (seq
                 (= fac (rec f (=> n (if (>> n 0) (** n (@ f (++ n -1))) 1))))
                 (@ fac 3))))
             (term 6))
  (test-->>E p-axioms
             (term
              (def r (heap even-odd) ; even odd)
                (seq
                 (= even-odd
                    (rec f
                      (array
                       (=> n (if (= n 0) ;; function even
                                 0       ;; 0 indicates even
                                 (@ (@ f 1) (++ n -1)) ;; call odd
                                 ))
                       (=> n (if (= n 0) ;; function odd
                                 1       ;; 1 indicates odd
                                 (@ (@ f 0) (++ n -1)) ;; call even
                                 ))
                       ))
                    )
; Too slow
;                 (= even (@ even-odd 0))
;                 (= odd (@ even-odd 1))
;                 (array (@ even 0) (@ odd 1) (@ even 2))
                 (@ (@ even-odd 0) 2)
                 ))
              )
             ;(term (arr 0 1 0))
             (term 0)
             )
  (test-->>E p-axioms
            (term
             (def r (heap adder inc)
               (seq
                (= adder (=> n (=> k (++ n k))))
                (= inc (@ adder 1))
                (array (@ inc 2) (@ inc 42)))))
            (term (arr 3 43)))         
  )

(module+ test
  (test--> w-axioms ;; P-add-wrong-1
           (term (++ (arr) 1))
           (term wrong))
  (test--> w-axioms ;; P-add-wrong-2
           (term (++ 1 (arr)))
           (term wrong))
  (test--> w-axioms ;; P-mul-wrong-1
           (term (** (arr) 1))
           (term wrong))
  (test--> w-axioms ;; P-mul-wrong-2
           (term (** 1 (arr)))
           (term wrong))
  (test--> w-axioms ;; P-gt-wrong-1
           (term (>> (arr) 1))
           (term wrong))
  (test--> w-axioms ;; P-gt-wrong-2
           (term (++ 1 (arr)))
           (term wrong))
  (test--> w-axioms ;; App-wrong
           (term (@ 1 2))
           (term wrong))
  (test--> w-axioms
           (term (if wrong 1 2))
           (term wrong))
  (test--> w-axioms
           (term (for wrong 1))
           (term wrong))
;  (test--> w-axioms ;; Arr-wrong
;           (term (arr 1 2 wrong 3))
;           (term wrong))
;  (test--> w-axioms ;; Op-wrong
;           (term (add wrong))
;           (term wrong))
;  (test--> w-axioms ;; UnifyL-wrong
;           (term (= wrong 2))
;           (term wrong))
;  (test--> w-axioms ;; UnifyR-wrong
;           (term (= 2 wrong))
;           (term wrong))
  (test-->> p-axioms
            (term (if (@ (++ 1 2) 3) 4 5))
            (term wrong))
)

;; Return a list of one reduction path tagged with rule names.
(define (apply-reduction-relation-trace-one e)
  (let ((xs (apply-reduction-relation/tag-with-names p-axioms e)))
    (if (null? xs)
        xs
        (let ((x (car xs)))
          (cons x (apply-reduction-relation-trace-one (cadr x)))))))

(module+ test
  (covered-cases e-axioms-coverage)
  (test-results)
  )
