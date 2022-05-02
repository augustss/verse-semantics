#lang racket
(require redex)
; (check-redundancy #t)

(define-language verse
  (p ::= e)
  (e ::=
     v
     (= e e)
     (seq e ...)
     (e e)
     (bar e ...)
     (orElse e e)
     (collect e)
     (def h e)  ;; See comment for binding forms.
     wrong
     )
  (v ::= x hnf)
  (hnf ::=
       k
       op
       (arr v ...)
       (=> x e)
       (rec x e)  ;; rec x e = fix (x => e)
       ty
       )
  (op ::= cop range)
  (cop ::= gt add sub mul)
  (ty ::= int (type v))
  (ce ::=
      v
      (cop ce)
      (= ce ce)
      (seq ce ...)
      )
  (h ::=
     (h ...)
     (var x)
     )
  (k ::= number)
  (x ::= variable-not-otherwise-mentioned)
  #:binding-forms
  ;; The binding forms require non-overlapping patterns
  ;; (according to Robby Findler).
  ;; The def construct binds differently in an if/for and otherwise.
  ;; To account for this (def r ...) is used for regular defs
  ;; and (def i ...) for defs in an if/for.
  ;; Most rules just match with (def q ...) and work for both,
  ;; but the binding forms distinguish the two.
  (=> x e #:refers-to x)
  (rec x e #:refers-to x)
  (h ...) #:exports (shadow h ...)
  (var x) #:exports x
  (def h e #:refers-to h)             ;; h variables bound in e
  )

;LA: I would like to name this +, but then the actual + in
;    meta-function plus refers to the wrong thing.
(define-metafunction verse
  ++ : e e -> e
  [(++ e_1 e_2) (add (array e_1 e_2))]
  )
(define-metafunction verse
  -- : e e -> e
  [(-- e_1 e_2) (sub (array e_1 e_2))]
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
  [(@ e_1 e_2) (e_1 e_2)]
  )

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
  (ex ::=
      (xarray (h ...) (e ...) (v ...) (e ...))
   )
  )
(define anf-array-red
  (reduction-relation
   verse+xarray
   ; #:domain ex
   ; #:codomain e
   (--> (xarray (h ...) (e ...) (v ...) ())
        (def (h ...) (seq e ... (arr v ...))))
   (--> (xarray (h ...) (e_1 ...) (v_1 ...) (v_2 e_2 ...))
        (xarray (h ...) (e_1 ...) (v_1 ... v_2) (e_2 ...)))
   (--> (xarray (h ...) (e_1 ...) (v_1 ...) (e_2 e_3 ...))
        (xarray (h ... (var a)) (e_1 ... (= a e_2)) (v_1 ... a) (e_3 ...))
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

; Bound variables from a leading def.
;(define-metafunction verse
;  bvs-e : e -> (x ...)
;  [(bvs-e (def h e)) (bvs-h h)]
;  [(bvs-e e) ()]
;  )

; Variables that are bound in a heap.
(define-metafunction verse
  bvs-h : h -> (x ...)
  [(bvs-h (var x)) (x)]
  [(bvs-h (h ...))
   (x ... ...)
   (where ((x ...) ...) ((bvs-h h) ...))]
  )

; Free variables in e.
(define-metafunction verse
  fvs-e : e -> (x ...)
  [(fvs-e v) (fvs-v v)]
  [(fvs-e (= e_1 e_2)) (x_1 ... x_2 ...)
   (where (x_1 ...) (fvs-e e_1))
   (where (x_2 ...) (fvs-e e_2))]
  [(fvs-e (seq e ...)) (x ... ...)
   (where ((x ...) ...) ((fvs-e e) ...))]
  [(fvs-e (e_1 e_2)) (x_1 ... x_2 ...)
   (where (x_1 ...) (fvs-e e_1))
   (where (x_2 ...) (fvs-e e_2))]
  [(fvs-e (bar e ...)) (x ... ...)
   (where ((x ...) ...) ((fvs-e e) ...))]
  [(fvs-e (orElse e_1 e_2)) (x_1 ... x_2 ...)
   (where (x_1 ...) (fvs-e e_1))
   (where (x_2 ...) (fvs-e e_2))]
  [(fvs-e (collect e_1)) (x_1 ...)
   (where (x_1 ...) (fvs-e e_1))]
  [(fvs-e (def h e)) (x ...)
   (where (x ...) (subtract (fvs-e e) (bvs-h h)))]
  [(fvs-e wrong) ()]
  )
  ; Free variables in v 
(define-metafunction verse
  fvs-v : v -> (x ...)
  [(fvs-v x) (x)]
  [(fvs-v k) ()]
  [(fvs-v (type v)) (fvs-v v)]
  [(fvs-v cop) ()]
  [(fvs-v (arr v ...)) (x ... ...)
   (where ((x ...) ...) ((fvs-v v) ...))]
  [(fvs-v (=> x e)) (subtract (fvs-e e) (x))]
  [(fvs-v (rec x e)) (subtract (fvs-e e) (x))]
  )

(module+ test
  (test-match verse e (term (def () 0)))
  (test-match verse e (term (bar (def () 0) (def () 1))))
  (test-match verse e (term (def (var x) (= x k))))
  (test-match verse h (term (var a)))
  (test-match verse h (term ((var x) (var y) () (var z))))
  (test-equal (term (bvs-h ((var x) (var y) (var z) ()))) (term (x y z)))
  (test-equal (term (fvs-e x)) (term (x)))
  (test-equal (term (fvs-e 5)) (term ()))
  (test-equal (term (fvs-e (= x y))) (term (x y)))
  (test-equal (term (fvs-e (seq (@ f x) (@ g x)))) (term (f x g x)))
  (test-equal (term (fvs-e (array (@ f x) (@ g x)))) (term (f x g x)))
  (test-equal (term (fvs-e (bar x (@ f y)))) (term (x f y)))
  (test-equal (term (fvs-e (=> x (def () (@ f x))))) (term (f)))
  (test-equal (term (fvs-e (def (var x) (@ f x)))) (term (f)))
  (test-equal (term (fvs-e (=> x (def (var y) (array x y z))))) (term (z)))
  (test-equal (term (fvs-e (orElse (def (var x) (seq x (=> a (++ x y)))) (=> b z)))) (term (y z)))
  (test-equal (term (fvs-e (collect (def (var x) (seq (@ x y) (=> a (@ x z))))))) (term (y z)))
  )

(define-extended-language verse+E verse
  (X ::=
     hole
     (= X e)
     (= e X)
     (seq e ... X e ...)
     (X e)
     (e X)
     (def h X)
     )
  (CX ::=
      hole
      (= CX e)
      (= ce CX)
      (seq ce ... CX e ...)
      (CX e)
      (ce CX)
      (def h CX)
      )
  (L ::=
       hole
       (bar L e ...))
  (H ::=
     hole
     (h ... H h ...))
  ;; E will find an 'e' hole that can reduce.
  ;; Not reducing under lambda, nor in if/for bodies.
  (E ::=
        hole
        (= E e)
        (= e E)
        (seq e ... E e ...)
        (E e)
        (e E)
        (orElse E e)
        (collect E)
        (bar e ... E e ...)
        (def h E)
        )  
  )

; Bound variables in an X.
(define-metafunction verse+E
  bvs-X : X -> (x ...)
  [(bvs-X hole) ()]
  [(bvs-X (def h X)) (x_1 ... x_2 ...)
   (where (x_1 ...) (bvs-h h))
   (where (x_2 ...) (bvs-X X))]
  [(bvs-X (= X e)) (bvs-X X)]
  [(bvs-X (= e X)) (bvs-X X)]
  [(bvs-X (seq e_1 ... X e_2 ...)) (bvs-X X)]
  [(bvs-X (X e)) (bvs-X X)]
  [(bvs-X (e X)) (bvs-X X)]
  )
   

(module+ test
  (test-match verse+E (in-hole H (var x)) (term ((var a) (var b))))
  (test-match verse+E (in-hole X (= x k)) (term (= a 5)))
  )

;; Axioms for h-expressions
(define e-axioms
  (reduction-relation
   verse+E
   #:domain e
   ;; Heaps
   (--> (def (h_1 ... () h_2 ...) e)
        (def (h_1 ... h_2 ...) e)
        "Heap")
   ;; Choice
   (--> (bar e_1 ... (bar e_2 ...) e_3 ...)
        (bar e_1 ... e_2 ... e_3 ...)
        "Bar-assoc")
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
   (--> (sub (arr k_1 k_2))
        (minus k_1 k_2)
        "P-sub")
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
   ;; TODO: This rule could be more generous and remove all vs the sequence.
   ;; It shouldn'e make any difference, but would give smaller terms.
   (--> (seq v ... e)
        e
        "Seq")
   (--> ((seq e_1 e_2) e_3)
        (seq e_1 (e_2 e_3))
        "App-seql")
   (--> (v_1 (seq e_2 e_3))
        (seq e_2 (v_1 e_3))
        "App-seqr")
   (--> (= (seq e_1 e_2) e_3)
        (seq e_1 (= e_2 e_3))
        "Unify-seql")
   (--> (= v_1 (seq e_2 e_3))
        (seq e_2 (= v_1 e_3))
        "Unify-seqr")
   ;; Lambda and applications
   (--> ((=> x e_1) e_2)
        (def (var t) (seq (= t e_2) (substitute e_1 x t)))
        (fresh t)
        "App-lam")
   (--> ((rec x e_1) e_2)
        ((substitute e_1 x (rec x e_1)) e_2)
        (fresh t)
        "App-rec")
   (--> ((arr v ...) k)
        (nth (v ...) k)
        (side-condition (and (>= (term k) 0) (< (term k) (length (term (v ...))))))
        "App-arr1")
   (--> ((arr v ...) k)
        (bar)
        (side-condition (not (and (>= (term k) 0) (< (term k) (length (term (v ...)))))))
        "App-arr2")
   ;; Range
   (--> (range (arr v ...))
        (bar v ...)
        "Range-arr")

   ;; Conditionals
   ;; If-true2 only needed when the 'if' does not have a 'def'
   (--> (orElse (bar) e_2)
        (e_2 (arr))
        "OrElse-false")
   (--> (orElse (in-hole L v) e_2)
        (v (arr))
        "OrElse-true")
   ;; For-loops
   ;; For2 only needed when the 'for' does not have a 'def'
   ;; For3 only needed when the 'for' does not have a 'bar'
   ;; For4 only needed when the 'for' does not have a 'bar' nor 'def'
   (--> (collect (bar v ...))
        (def ((var t) ...) (seq (= t (v (arr))) ... (arr t ...)))
        (fresh ((t ...) (v ...)))
        "Collect1")
   (--> (collect v)
        (def ((var t)) (seq (= t (v (arr))) (arr t)))
        (fresh t)
        "Collect2")
   ;; Def blocks
   (--> (def () e)
        e
        "Def-elim")
   ;; Unification
   (--> (def (in-hole H (var x)) (in-hole X (= x v)))
        (def (in-hole H ()) (substitute (in-hole X v) x v))
        (side-condition (disjoint (term (fvs-v v)) (term (union (x) (bvs-X X)))))
        "Bind")
   (--> (def (in-hole H (var x)) (in-hole X (= x v)))
        (def (in-hole H ((var x) (var y))) (in-hole X (seq (= x_1 y) (= x (substitute v x_1 y)))))
        ;; z \in (fvs-v v) and z \in (bvs-X X)
        (where (x_1 x_2 ...) (intersect (fvs-v v) (bvs-X X)))
        ; (side-condition (not (redex-match? verse x v)))
        (fresh y)
        "Promote")
   ;; This SWAP rule is more generous than the one in the paper.
   (--> (= e_1 x_1)
        (= x_1 e_1)
        (side-condition (not (redex-match? verse x (term e_1))))
        "Swap")
   (--> (= x (range ty))
        (ty x) ;; rules has (seq (ty x) x)
        "Utype")
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
  minus : k k -> k
  [(minus k_1 k_2) ,(- (term k_1) (term k_2))])

(define-metafunction verse
  times : k k -> k
  [(times k_1 k_2) ,(* (term k_1) (term k_2))])

(define-metafunction verse
  nth : (e ...) k -> e
  [(nth (e ...) k) ,(list-ref (term (e ...)) (term k))]
  )

;; Take a hnf and turn it into a new hnf that can be used
;; to tell different kinds of hnfs apart.  This used for the fail
;; case in unification.
;; Mapping:
;;  k -> k
;;  (arr ...) -> (arr length-of-array)
;;  (=> ...) -> (=> a 0)
(define-metafunction verse
  head : hnf -> hnf
  [(head k) k]
  [(head (arr v ...)) (arr ,(length (term (v ...))))]
  [(head (=> x e)) (=> a 0)]
  )

(define-metafunction verse
  intersect : (x ...) (x ...) -> (x ..._)
  [(intersect (x_1 ...) (x_2 ...)) ,(set-intersect (term (x_1 ...)) (term (x_2 ...)))]
  )

(define-metafunction verse
  union : (x ...) (x ...) -> (x ..._)
  [(union (x_1 ...) (x_2 ...)) (x_1 ... x_2 ...)]
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
   (--> (k e)
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
   (--> (orElse wrong e)
        wrong
        "If-wrong")
   (--> (for (bar v ... wrong) e)
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
  ;; Choice
  (test--> e-axioms ;; Bar-assoc-1
           (term (bar 1 (bar) 2))
           (term (bar 1 2)))
  (test--> e-axioms ;; Bar-assoc-2
           (term (bar 1 (bar 2)))
           (term (bar 1 2)))
  (test--> e-axioms ;; Fail
           (term (def ((var x) (var y)) (bar)))
           (term (bar)))
  (atest--> e-axioms ;; Choice
           (term (def (var x) (bar 1 x)))
           (term (bar (def (var x) 1) (def (var x) x))))
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
           (term (= (seq x y) (f z)))
           (term (seq x (= y (f z)))))
  (test--> e-axioms ;; Unify-seqr
           (term (= x (seq y z)))
           (term (seq y (= x z))))
  ;; Lambda and applications
  (test--> e-axioms ;; App-lam
           (term (@ (=> a (++ a 1)) 5))
           (term (def (var t) (seq (= t 5) (++ t 1)))))
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
           (term (orElse (bar) (=> x 2)))
           (term ((=> x 2) (arr))))
  (test--> e-axioms ;; If-true1-1
           (term (orElse (=> x 1) 2))
           (term ((=> x 1) (arr))))
  ;; For-loops
  (test--> e-axioms ;; Collect1-1
           (term (collect (bar)))
           (term (def () (seq (arr)))))
  (test--> e-axioms ;; Collect1-2
           (term (collect (bar (=> x 0) (=> x 2))))
           (term (def ((var t) (var t1)) (seq (= t ((=> x 0) (arr))) (= t1 ((=> x 2) (arr))) (arr t t1)))))
  (test--> e-axioms ;; Collect2
           (term (collect (=> x 0)))
           (term (def ((var t)) (seq (= t ((=> x 0) (arr))) (arr t)))))
  ;; Def blocks
  (test--> e-axioms ;; Def-elim
           (term (def () y))
           (term y))
  ;; Unification
  (atest--> e-axioms ;; Bind-1
           (term (def ((var b) (var a)) (seq (= a 5) a)))
           (term (def ((var b) ()) (seq 5 5))))
  (test-equal ;; Bind-3  do NOT allow circularity
           (apply-reduction-relation e-axioms (term (def (var a) (= a a))))
           '())
  (atest--> e-axioms ;; Promote
           (term (def (var a) (def (var b) (= a (arr 1 b)))))
           (term (def ((var a) (var y)) (def (var b) (seq (= b y) (= a (arr 1 y)))))))
;  (test--> e-axioms ;; Swap
;           (term (def r (a) (= 5 a)))
;           (term (def r (a) (= a 5))))
  (test--> e-axioms ;; Swap
           (term (= 5 a))
           (term (= a 5)))
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

(define e-axioms*
  (context-closure (union-reduction-relations e-axioms w-axioms) verse+E E))

(define p-axioms
  e-axioms*)

(module+ test
  (test-->> p-axioms
            (term (def (var x) (seq (= 6 x) (++ x 1))))
            (term 7))
  (test-->> p-axioms
            (term (def ((var x) (var y)) (seq (= y (orElse (seq (= x 1) (=> z 111)) (=> z 222))) (= x 1) y)))
            (term 111))
  (test-->> p-axioms
            (term (def ((var x) (var y)) (seq (= y (orElse (seq (= x 1) (=> z 111)) (=> z 222))) (= x 2) y)))
            (term 222))
;; Using test-->> is better, but very slow.
  (test-->>E p-axioms
            (term (collect (def (var x) (seq (= x (bar 1 2)) (=> z (++ x 1))))))
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
            (term (def ((var x) (var y)) (seq (= x 3) (= y (++ x 1)))))
            (term 4))
  (test-->> p-axioms ;; Example-2
            (term (def ((var x) (var y)) (seq (= y (++ x 1)) (= x 3) y)))
            (term 4))
  (test-->>E p-axioms ;; Example-3
            (term (def ((var x) (var y)) (seq (= y (++ x 1)) (bar (= x 3) (= x 77)))))
            (term (bar 3 77)))
  (test-->>E p-axioms ;; simple recusion
             (term
              (def (var f)
                (seq
                 (= f (rec g (=> n (orElse (seq (= n 0) (=> z 0)) (=> z (++ n -1))))))
                 (@ f 1))))
             (term 0))
  (test-->>E p-axioms ;; factorial
             (term
              (def (var fac)
                (seq
                 (= fac (rec f (=> n (orElse (seq (>> n 0) (=> z (** n (@ f (++ n -1))))) (=> z 1)))))
                 (@ fac 3))))
             (term 6))
  (test-->>E p-axioms
             (term
              (def (var even-odd) ; even odd)
                (seq
                 (= even-odd
                    (rec f
                      (array
                       (=> n (orElse (seq (= n 0)             ;; function even
                                          (=> z 0))           ;; 0 indicates even
                                 (=> z (@ (@ f 1) (++ n -1))) ;; call odd
                                 ))
                       (=> n (orElse (seq (= n 0)             ;; function odd
                                          (=> z 1))           ;; 1 indicates odd
                                 (=> z (@ (@ f 0) (++ n -1))) ;; call even
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
             (def ((var adder) (var inc))
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
;  (test--> w-axioms
;           (term (if wrong 1 2))
;           (term wrong))
;  (test--> w-axioms
;           (term (for wrong 1))
;           (term wrong))
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
;  (test-->> p-axioms
;            (term (if (@ (++ 1 2) 3) 4 5))
;            (term wrong))
)

;; Return a list of one reduction path tagged with rule names.
(define (apply-reduction-relation-trace-one e)
  (let ((xs (apply-reduction-relation/tag-with-names p-axioms e)))
    (if (null? xs)
        xs
        (let ((x (car xs)))
          (cons x (apply-reduction-relation-trace-one (cadr x)))))))

(define (red e)
  (cons (list "START" e)
        (apply-reduction-relation-trace-one e)))

(define (redout fn e)
  (let ((out (open-output-file fn #:exists 'truncate)))
    (print (red e) out)
    (close-output-port out)))

(module+ test
  (covered-cases e-axioms-coverage)
  (test-results)
  )
