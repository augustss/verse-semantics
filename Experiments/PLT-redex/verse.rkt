#lang racket
(require redex)

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
     (def h e)
     )
  (op ::= cop apply)
  (cop ::= gt add)
  (hnf ::=
       k
       (arr v ...)
       (=> x e)
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
     (var x)
     (:= x v))
  (k ::= number)
  (x ::= variable-not-otherwise-mentioned)
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

;(define-metafunction verse
;  = : e e -> e
;  [(= e_1 e_2) (unify (array e_1 e_2))]
;  )
;LA: I would like to name this +, but then the actual + in
;    meta-function plus refers to the wrong thing.
(define-metafunction verse
  ++ : e e -> e
  [(++ e_1 e_2) (add (array e_1 e_2))]
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
        (def (heap h ...) (seq e ... (arr v ...))))
   (--> (xarray (h ...) (e_1 ...) (v_1 ...) (v_2 e_2 ...))
        (xarray (h ...) (e_1 ...) (v_1 ... v_2) (e_2 ...)))
   (--> (xarray (h ...) (e_1 ...) (v_1 ...) (e_2 e_3 ...))
        (xarray (h ... (var t)) (e_1 ... (= t e_2)) (v_1 ... t) (e_3 ...))
        (side-condition (not (redex-match verse v (term e_2))))
        (fresh t))
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
  [(bvs-e (def h e)) (bvs-h h)]
  [(bvs-e e) ()]
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
  vvs : h -> (x ...)
  [(vvs (var x)) ()]
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
  [(fvs-e (def h e)) (x_1 ... x_2 ...)
   (where (x_1 ...) (fvs-h h))
   (where (x_2 ...) (subtract (fvs-e e) (bvs-h h)))]
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
  [(fvs-v x) (x)]
  [(fvs-v k) ()]
  [(fvs-v (arr v ...)) (x ... ...)
   (where ((x ...) ...) ((fvs-v v) ...))]
  [(fvs-v (=> x e)) (subtract (fvs-e e) (x))]
  )

(module+ test
  (test-match verse e (term (def (heap) 0)))
  (test-match verse e (term (bar (def (heap) 0) (def (heap) 1))))
  (test-match verse e (term (def (heap (var x)) (= x k))))
  (test-match verse h (term (:= a 2)))
  (test-match verse h (term (heap (var x) (:= y 1) (heap) (heap (var z)))))
  (test-equal (term (bvs-h (heap (var x) (:= y 1) (heap (var z)) (heap)))) (term (x y z)))
  (test-equal (term (vvs (heap (var x) (:= y 1) (heap (var z)) (heap)))) (term (y)))
  (test-equal (term (bvs-e (def (var x) 0))) (term (x)))
;  (test-equal (term (bvs-e (bar
;                             (def (heap (var x) (:= y 1)) 0)
;                             (def (var z) 1))))
;              (term (x y z)))
  (test-equal (term (fvs-e x)) (term (x)))
  (test-equal (term (fvs-e 5)) (term ()))
  (test-equal (term (fvs-e (= x y))) (term (x y)))
  (test-equal (term (fvs-e (seq (@ f x) (@ g x)))) (term (f x g x)))
  (test-equal (term (fvs-e (array (@ f x) (@ g x)))) (term (f x g x)))
  (test-equal (term (fvs-e (bar x (@ f y)))) (term (x f y)))
  (test-equal (term (fvs-e (=> x (def (heap) (@ f x))))) (term (f)))
  (test-equal (term (fvs-e (def (var x) (@ f x)))) (term (f)))
  (test-equal (term (fvs-e (def (:= x y) (@ x z)))) (term (y z)))
  (test-equal (term (fvs-h (heap (var x) (:= y z)))) (term (z)))
  (test-equal (term (fvs-e (=> x (def (var y) (array x y z))))) (term (z)))
  (test-equal (term (fvs-e (if (def (var x) x) (++ x y) z))) (term (y z)))
  (test-equal (term (fvs-e (for (def (var x) (@ x y)) (@ x z)))) (term (y z)))
  )

(define-extended-language verse+E verse
  (X ::=
     hole
     (= X e)
     (= e X)
     (seq e ... X e ...)
     (op X)
     (def h X)
     )
  (CX ::=
      hole
      (= CX e)
      (= e CX)
      (seq e ... CX e ...)
      (op CX)
      (def h CX)
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
        (def h E)
        )  
  )

(define-metafunction verse+E
  bvs-X : X -> (x ...)
  [(bvs-X hole) ()]
  [(bvs-X (def h X)) (x_1 ... x_2 ...)
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
   (==> (def (in-hole H (:= x v)) e)
        (def (in-hole H (:= x v)) (substitute e x v))
        (side-condition (member (term x) (term (fvs-e e))))
        "Subst")
   ;; Choice
   (==> (bar e_1 ... (bar e_2 ...) e_3 ...)
        (bar e_1 ... e_2 ... e_3 ...)
        "Bar-assoc")
;   (==> (bar e)
;        e
;        "Bar-sing")
   (==> (in-hole X (bar))
        (bar)
        (side-condition (not (equal? (term X) (term hole))))
        "Fail")
   (==> (in-hole CX (bar e ...))
        (bar (in-hole CX e) ...)
        (side-condition (not (equal? (term CX) (term hole))))
        "Choose")
   ;; Primitive operations
   (==> (add (arr k_1 k_2))
        (plus k_1 k_2)
        "P-add")
   (==> (gt (arr k_1 k_2))
        k_1
        (side-condition (> (term k_1) (term k_2)))
        "P-gt1")
   (==> (gt (arr k_1 k_2))
        (bar)
        (side-condition (not (> (term k_1) (term k_2))))
        "P-gt2")
   ;; Sequencing
   (==> (seq v ... e)
        e
        "Seq")
   (==> (op (seq e_1 e_2))
        (seq e_1 (op e_2))
        "Op-seq")
   (==> (= (seq e_1 e_2) e_3)
        (seq e_1 (= e_2 e_3))
        "Unify-seql")
   (==> (= v_1 (seq e_2 e_3))
        (seq e_2 (= v_1 e_3))
        "Unify-seqr")
   ;; Lambda and applications
   (==> (apply (arr (=> x e_1) e_2))
        (def (var t) (seq (= t e_2) (substitute e_1 x t)))
        (fresh t)
        "App-lam")
   (==> (apply (arr (arr v ...) k))
        (nth (v ...) k)
        (side-condition (and (>= (term k) 0) (< (term k) (length (term (v ...))))))
        "App-arr1")
   (==> (apply (arr (arr v ...) k))
        (bar)
        (side-condition (not (and (>= (term k) 0) (< (term k) (length (term (v ...)))))))
        "App-arr2")

   ;; Conditionals
   (==> (if (bar) e_1 e_2)
        e_2
        "If-false")
   (==> (if (in-hole L (def h v)) e_1 e_2)
        (def h e_1)
        "If-true")
   ;; For-loops
   (==> (for (bar (def h v) ...) e)
        (def (heap (var t) ...) (seq (= t (def h e)) ... (arr t ...)))
        (fresh ((t ...) (h ...)))
        "For")
   ;; Do blocks
   (==> (in-hole X (def h e))
        (in-hole X e)
        (side-condition (not (equal? (term X) (term hole))))
        (side-condition (disjoint (term (fvs-e e)) (term (bvs-h h))))
        "Do-def")
   ;; Unification
   (==> (def (in-hole H (var x)) (in-hole X (= x v)))
        (def (in-hole H (:= x v)) (in-hole X v))
        (side-condition (disjoint (term (fvs-v v)) (term (vvs (in-hole H (:= x v))))))
        (side-condition (disjoint (term (fvs-v v)) (term (bvs-X X))))
        "Bind")
   (==> (def (in-hole H (var x)) (in-hole X (= x v)))
        (def (in-hole H (heap (var x) (var y))) (in-hole X (seq (= x_1 y) (= x (substitute v x_1 y)))))
        ;; z \in (fvs-v v) and z \in (bvs-X X)
        (where (x_1 x_2 ...) (intersect (fvs-v v) (bvs-X X)))
        ; (side-condition (not (redex-match? verse x v)))
        (fresh y)
        "Promote")
;   (==> (def (in-hole H (var x)) (in-hole X (= v x)))
;        (def (in-hole H (var x)) (in-hole X (= x v)))
;        (side-condition (not (member (term v) (term (vvs (in-hole H (var x)))))))
;        (side-condition (not (member (term v) (term (bvs-X X)))))
;        (side-condition (not (redex-match? verse x (term v))))
;        "Swap")
   (==> (= v x)
        (= x v)
        (side-condition (not (redex-match? verse x (term v))))
        "Swap")
   (==> (= x_1 x_1)
        x_1
        "Uvar")
   (==> (= k_1 k_1)
        k_1
        "Ucon")
   (==> (= (arr v_1 ...) (arr v_2 ...))
        (seq (= v_1 v_2) ... (arr v_1 ...))
        (side-condition (equal? (length (term (v_1 ...))) (length (term (v_2 ...)))))
        "Utup")
   (==> (= hnf_1 hnf_2)
        (bar)
        (side-condition (not (equal? (term (head hnf_1)) (term (head hnf_2)))))
        "UX")
  
  with
  [(--> a b)
   ;; WAS (--> (in-hole XE-E a) (in-hole XE-E b))
   (==> a b)]
  ))

(define aaa (term (def (var a) (seq (= 6 a) (++ a 1)))))
;(define ppp (term (def (in-hole H (var x)) (in-hole X (= x v)))))
;(define bbb (redex-match verse+E ppp aaa))

(define-metafunction verse
  plus : k k -> k
  [(plus k_1 k_2) ,(+ (term k_1) (term k_2))])

(define-metafunction verse
  nth : (e ...) k -> e
  [(nth (e ...) k) ,(list-ref (term (e ...)) (term k))]
  )

(define-metafunction verse
  head : hnf -> hnf
  [(head k) k]
  [(head (arr v ...)) (arr ,(length (term (v ...))))]
  [(head (=> x he)) (=> a (def (heap) 0))]
  )

(define-metafunction verse
  intersect : (x ...) (x ...) -> (x ..._)
  [(intersect (x_1 ...) (x_2 ...)) ,(set-intersect (term (x_1 ...)) (term (x_2 ...)))]
  )

(define (disjoint l1 l2)
  (null? (set-intersect l1 l2)))


(module+ test
  ;; Substitution
  (test--> e-axioms ;; Subst
           (term (def (:= x 5) x))
           (term (def (:= x 5) 5)))
  ;; Choice
  (test--> e-axioms ;; Bar-assoc-1
           (term (bar 1 (bar) 2))
           (term (bar 1 2)))
  (test--> e-axioms ;; Bar-assoc-2
           (term (bar 1 (bar 2)))
           (term (bar 1 2)))
  (test--> e-axioms ;; Fail
           (term (def (heap (var x) (var y)) (bar)))
           (term (bar)))
  (test--> e-axioms ;; Choice
           (term (def (heap) (bar 1 2)))
           (term (bar (def (heap) 1) (def (heap) 2))))
  ;; Primitive operations
  (test--> e-axioms ;; P-add
           (term (++ 3 4))
           (term 7))
  (test--> e-axioms ;; P-gt1
           (term (>> 5 4))
           (term 5))
  (test--> e-axioms ;; P-gt2
           (term (>> 3 4))
           (term (bar)))
  ;; Floating
  (test--> e-axioms ;; Seq
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
           (term (def (var t) (seq (= t 5) (++ t 1)))))
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
  (test--> e-axioms ;; If-true-1
           (term (if (def (heap) 3) 1 2))
           (term (def (heap) 1)))
  (test--> e-axioms ;; If-true-2
           (term (if (bar (def (heap) 3)) 1 2))
           (term (def (heap) 1)))
  ;; For-loops
  (test--> e-axioms ;; For-1
           (term (for (bar) 0))
           (term (def (heap) (seq (arr)))))
  (test--> e-axioms ;; For-2
           (term (for (bar (def (var x) 5)) 0))
           (term (def (heap (var t)) (seq (= t (def (var x) 0)) (arr t)))))
  ;; Do blocks
  (test--> e-axioms ;; Do-def
           (term (add (def (:= x 1) y)))
           (term (add y)))
  ;; Unification
  (test--> e-axioms ;; Bind-1
           (term (def (heap (var a)) (= a 5)))
           (term (def (heap (:= a 5)) 5)))
  (test--> e-axioms ;; Bind-2
           (term (def (heap (var a)) (= a b)))
           (term (def (heap (:= a b)) b)))  
  (test-equal ;; Bind-3  do NOT allow circularity
           (apply-reduction-relation e-axioms (term (def (heap (var a)) (= a a))))
           '())
  (test--> e-axioms
           (term (def (var a) (def (var b) (= a (arr 1 b)))))
           (term (def (heap (var a) (var y)) (def (var b) (seq (= b y) (= a (arr 1 y)))))))
;  (test--> e-axioms ;; Swap
;           (term (def (heap (var a)) (= 5 a)))
;           (term (def (heap (var a)) (= a 5))))
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


;; Axioms for expressions
;(define e-axioms
;  (reduction-relation
;   verse+E
;   #:domain he
;   (==> (apply (array (array e_1 ...) e_2))
;        (do (def (var i) (seq (= i e_2) (= i (alts (count (e_1 ...)))) (index (array (array e_1 ...) i)))))
;        (fresh i)
;        "App-arr")
;   (==> (index (array (array e ...) k))
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

(define top-def
  (reduction-relation
   verse+E
   #:domain e
   (--> (def h e)
        e
        (side-condition (disjoint (term (fvs-e e)) (term (bvs-h h))))
        "Top-def")
   ))
   

(define e-axioms*
  (context-closure e-axioms verse+E E))

(define p-axioms
  (union-reduction-relations e-axioms* top-def))

(module+ test
  (test-->> p-axioms
            (term (def (:= x 1) x))
            (term 1))
  (test-->> p-axioms
            (term (def (var x) (seq (= 6 x) (++ x 1))))
            (term 7))
  (test-->> p-axioms
            (term (def (heap (var x) (var y)) (seq (= y (if (def (heap) (= x 1)) 111 222)) (seq (= x 1) y))))
            (term 111))
  (test-->> p-axioms
            (term (def (heap (var x) (var y)) (seq (= y (if (def (heap) (= x 1)) 111 222)) (seq (= x 2) y))))
            (term 222))
; SLOW
;  (test-->> p-axioms
;            (term (for (def (var x) (= x (bar 1 2))) (++ x 1)))
;            (term (array 2 3)))
  (test-->> p-axioms
            (term (@ (array 1 2) 1))
            (term 2))
  (test-->> p-axioms
            (term (@ (=> x (++ x 1)) (++ 2 3)))
            (term 6))
  )

(module+ test
  (test-->> p-axioms ;; Example-1
            (term (def (heap (var x) (var y)) (seq (= x 3) (= y (++ x 1)))))
            (term 4))
  (test-->> p-axioms ;; Example-2
            (term (def (heap (var x) (var y)) (seq (= y (++ x 1)) (= x 3) y)))
            (term 4))
  (test-->> p-axioms ;; Example-2
            (term (def (heap (var x) (var y)) (seq (= y (++ x 1)) (bar (= x 3) (= x 77)))))
            (term (bar
                   (def (heap (:= x 3) (:= y 4)) 3)
                   (def (heap (:= x 77) (:= y 78)) 77))))
  )

(module+ test
  (test-results))
