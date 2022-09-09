#lang racket
(require redex)
; (check-redundancy #t)

; The syntax for e include split, even when not used.
; Otherwise it cannot occur in the metafunctions; they are not extensible.
(define-language verse
  (k ::= number)
  (x ::= variable-not-otherwise-mentioned)
  (p ::= (one e))
  (e ::=
     v
     (v_1 v_2)
     (= e_1 e_2)
     (seq e_1 e_2)
     (exists x e)
     (bar e_1 e_2)
     fail
     (one e)
     (all e)
     ;; Only used in the SPLIT version
     (split e v_1 v_2)
     )
  (v ::= x hnf)
  (hnf ::=
       k
       op
       (arr v ...)
       (lam x e)
       )
  (op ::= gt add sub mul mapap
      ;; Only used in the SPLIT version
      cons
  )
  #:binding-forms
  (lam x e #:refers-to x)
  (exists x e #:refers-to x)
 )

;LA: I would like to name this +, but then the actual + in
;    meta-function plus refers to the wrong thing.
(define-metafunction verse
  ++ : v v -> e
  [(++ v_1 v_2) (add (arr v_1 v_2))]
  )
(define-metafunction verse
  -- : v v -> e
  [(-- v_1 v_2) (sub (arr v_1 v_2))]
  )
(define-metafunction verse
  ** : v v -> e
  [(** v_1 v_2) (mul (arr v_1 v_2))]
  )
(define-metafunction verse
  >> : v v -> e
  [(>> v_1 v_2) (gt (arr v_1 v_2))]
  )

(define-metafunction verse
  union : (x ...) (x ...) -> (x ...)
  [(union (x_1 ...) (x_2 ...)) ,(set-union (term (x_1 ...)) (term (x_2 ...)))]
  )

(define-metafunction verse
  unions : (x ...) ... -> (x ...)
  [(unions) ()]
  [(unions (x ...)) (x ...)]
  [(unions (x_1 ...) (x_2 ...)) (union (x_1 ...) (x_2 ...))]
  [(unions (x_1 ...) (x_2 ...) (x_3 ...) ...) (unions (union (x_1 ...) (x_2 ...)) (x_3 ...) ...)]
  )
  
(define-metafunction verse
  subtract : (x ...) (x ...) -> (x ...)
  [(subtract (x_1 ...) (x_2 ...)) ,(set-subtract (term (x_1 ...)) (term (x_2 ...)))]
  )

(define-metafunction verse
  intersect : (x ...) (x ...) -> (x ...)
  [(intersect (x_1 ...) (x_2 ...)) ,(set-intersect (term (x_1 ...)) (term (x_2 ...)))]
  )

(define-metafunction verse
  fvs-e : e -> (x ...)
  [(fvs-e v) (fvs-v v)]
  [(fvs-e (v_1 v_2)) (union (fvs-v v_1) (fvs-v v_2))]
  [(fvs-e (= e_1 e_2)) (union (fvs-e e_1) (fvs-e e_2))]
  [(fvs-e (seq e_1 e_2)) (union (fvs-e e_1) (fvs-e e_2))]
  [(fvs-e (exists x e)) (subtract (fvs-e e) (x))]
  [(fvs-e (bar e_1 e_2)) (union (fvs-e e_1) (fvs-e e_2))]
  [(fvs-e fail) ()]
  [(fvs-e (one e)) (fvs-e e)]
  [(fvs-e (all e)) (fvs-e e)]
  [(fvs-e (split e v_1 v_2)) (union (fvs-e e) (union (fvs-v v_1) (fvs-v v_2)))]
  )

(define-metafunction verse
  fvs-v : v -> (x ...)
  [(fvs-v x) (x)]
  [(fvs-v hnf) (fvs-hnf hnf)]
  )

(define-metafunction verse
  fvs-hnf : hnf -> (x ...)
  [(fvs-hnf k) ()]
  [(fvs-hnf op) ()]
  [(fvs-hnf (arr v ...)) (unions (fvs-v v) ...)]
  [(fvs-hnf (lam x e)) (subtract (fvs-e e) (x))]
  )

(define-metafunction verse
  arr-index : v k (v ...) -> e
  [(arr-index v_1 k ()) fail]
  [(arr-index v_1 k (v_2)) (seq (= v_1 k) v_2)]
  [(arr-index v_1 k (v_2 v_3 ...)) (bar (seq (= v_1 k) v_2) (arr-index v_1 ,(+ (term k) 1) (v_3 ...)))]
  )

(define-metafunction verse
  seq* : e ... -> e
  [(seq* e) e]
  [(seq* e_1 e_2 ...) (seq e_1 (seq* e_2 ...))]
  )

(define-metafunction verse
  exists* : (x ...) e -> e
  [(exists* () e) e]
  [(exists* (x_1 x_2 ...) e) (exists x_1 (exists* (x_2 ...) e))]
  )

(define-metafunction verse
  lamp : e e -> e
  [(lamp e_1 e_2)
   (lam ttt (exists* (fvs-e e_1) (seq (= ttt e_1) e_2)))
   ] ;; (fresh ttt) XXX ttt should be fresh, but that's not possible in a metafunction
  )

(define-metafunction verse
  if* : (x ...) e e e -> e
  [(if* (x ...) e_1 e_2 e_3)
   (exists vvv (seq (= vvv (one (bar (exists* (x ...) (seq e_1 (lam dummy e_2))) (lam dummy e_3))))
                    (vvv (arr))))
   ] ;; (fresh vvv)
  )

(define-metafunction verse
  for* : (x ...) e e -> e
  [(for* (x ...) e_1 e_2) (:= aaa (all (exists* (x ...) (seq e_1 (lam dummy e_2)))) (mapap aaa))
  ] ;; (fresh aaa)
  )

(define-metafunction verse
  bar* : e ... -> e
  [(bar*) fail]
  [(bar* e) e]
  [(bar* e_1 e_2 ...) (bar e_1 (bar* e_2 ...))]
  )

(define-metafunction verse
  bar-values : e -> (v ...)
  [(bar-values v) (v)]
  [(bar-values (bar v e))
   (v v_1 v_2 ...)
   (where (v_1 v_2 ...) (bar-values e))]
  [(bar-values (bar v e))
   ()
   (where () (bar-values e))]
  [(bar-values e)
   ()
   (side-condition (not (redex-match? verse v (term e))))]
  )

(define-metafunction verse
  := : x e e -> e
  [(:= x e_1 e_2)
   (exists x (seq (= x e_1) e_2))]
  )

(define-extended-language verse+context verse
  (X ::=
     hole
     (= X e)
     (= e X)
     (seq X e)
     (seq e X)
     )
  (V ::=
     hole
     (arr v_1 ... V v_2 ...)
     )
  (SX ::=
      (bar hole e)
      (bar e hole)
      (one hole)
      (all hole)
      ;; Only used in the SPLIT version
      (split hole v_1 v_2)
      )
  (CX ::=
      hole
      (= CX e)
      (= ce CX)
      (seq CX e)
      (seq ce CX)
      (exists x CX)
      )
  (ce ::=
      v
      (= ce_1 ce_2)
      (seq ce_1 ce_2)
      (one e)
      (all e)
      ;; Only used in the SPLIT version
      (split e v_1 v_2)
      )
  ; Extra context for an arbitrary number of exists
  (Es ::=
      hole
      (exists x Es)
      )
  ; Evaluation context, reduce anywhere except under lambda
  ; and after bar.
  (E ::=
     hole
     (= E e)
     (= e E)
     (seq E e)
     (seq e E)
     (exists x E)
     (bar E e)
     ; Note: There is no (bar e E).  Evaluating the second
     ; operand of bar make 'one' uneffective in cutting off
     ; evaluation that is not needed.
     ; When using (split f e g) this works, when using (all e)
     ; we need (bar v E), but that can make (one e) loop when
     ; using recursion.
     (one E)
     (all E)
     ;; Only used in the SPLIT version
     (split E v_1 v_2)
     )
  )

(define-metafunction verse+context
  fvs-X : X -> (x ...)
  [(fvs-X hole) ()]
  [(fvs-X (= X e)) (union (fvs-X X) (fvs-e e))]
  [(fvs-X (= e X)) (union (fvs-X X) (fvs-e e))]
  [(fvs-X (seq X e)) (union (fvs-X X) (fvs-e e))]
  [(fvs-X (seq e X)) (union (fvs-X X) (fvs-e e))]
  )

;;;;;;;;;;; array ANF ;;;;;;;;;;;
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
      (xarray (x ...) (e ...) (v ...) (e ...))
   )
  )
(define anf-array-red
  (reduction-relation
   verse+xarray
   ; #:domain ex
   ; #:codomain e
   (--> (xarray (x ...) (e ...) (v ...) ())
        (exists* (x ...) (seq* e ... (arr v ...))))
   (--> (xarray (x ...) (e_1 ...) (v_1 ...) (v_2 e_2 ...))
        (xarray (x ...) (e_1 ...) (v_1 ... v_2) (e_2 ...)))
   (--> (xarray (x ...) (e_1 ...) (v_1 ...) (e_2 e_3 ...))
        (xarray (x ... a) (e_1 ... (= a e_2)) (v_1 ... a) (e_3 ...))
        (side-condition (not (redex-match verse v (term e_2))))
        (fresh a))
   ))
(define (anf-array a)
  (let ((b (car (apply-reduction-relation anf-array-red a))))
    (if
     (redex-match? verse+xarray ex b)
     (anf-array b)
     b)))
;;;;;;;;;;; end array ANF ;;;;;;;;;;;

(define rules
  (reduction-relation
   verse+context
   #:domain e
   ;; Primitive operations
   (--> (add (arr k_1 k_2))
        ,(+ (term k_1) (term k_2))
        "p-add")
   (--> (sub (arr k_1 k_2))
        ,(- (term k_1) (term k_2))
        "p-sub")
   (--> (mul (arr k_1 k_2))
        ,(* (term k_1) (term k_2))
        "p-mul")
   (--> (gt (arr k_1 k_2))
        k_1
        (side-condition (> (term k_1) (term k_2)))
        "p-gt-1")
   (--> (gt (arr k_1 k_2))
        fail
        (side-condition (not (> (term k_1) (term k_2))))
        "p-gt-2")
   (--> (mapap (arr (lam x e) ...))
        (array e ...)
        "p-map-ap")
   ;; Application
   (--> ((lam x e) v)
        (exists t (seq (= t v) (substitute e x t)))
        (fresh t)  ;; Use a fresh variable instead of a fvs side condition
        "app-beta")
   (--> ((arr v_1 ...) v_2)
        (arr-index v_2 0 (v_1 ...))
        "app-arr"
        )
   ;; Unification
   (--> (= k_1 k_1)
        k_1
        "ulit-1")
   (--> (= k_1 k_2)
        fail
        (side-condition (not (equal? (term k_1) (term k_2))))
        "ulit-2")
   (--> (= (arr v_1 ...) (arr v_2 ...))
        (seq* (= v_1 v_2) ... (arr v_1 ...))
        (side-condition (equal? (length (term (v_1 ...))) (length (term (v_2 ...)))))
        "utup-1")
   (--> (= (arr v_1 ...) (arr v_2 ...))
        fail
        (side-condition (not (equal? (length (term (v_1 ...))) (length (term (v_2 ...))))))
        "utup-2")
   (--> (= k (arr v ...))
        fail
        "ux1"
        )
   (--> (= (arr v ...) k)
        fail
        "ux2"
        )
   (--> (= (lam x e) hnf)
        fail
        "ux3"
        )
   (--> (= hnf (lam x e))
        fail
        "ux4"
        )
   (--> (= op hnf)
        fail
        "ux5"
        )
   (--> (= hnf op)
        fail
        "ux6"
        )
   (--> (= x (in-hole V x))
        fail
        (side-condition (not (equal? (term V) (term hole))))
        "ux-occurs"
        )
   ;; Unification variables
   (--> (in-hole X (= x v))
        (in-hole (substitute X x v) (= x v))
        (where (x_1) (intersect (fvs-X X) (x)))
        (where () (intersect (fvs-v v) (x)))
        "subst"
        )
   (--> (= x_1 (in-hole V (lam x_2 e)))
        (= x_1 (in-hole V (lam x_2 (exists x_1 (seq (= x_1 (in-hole V (lam x_2 e))) e)))))
        (where (x) (intersect (x_1) (fvs-e e)))
        "subst-rec"
        )
   (--> (exists x (in-hole Es (in-hole X (= x v))))
        (in-hole Es (in-hole X v))
        (where () (intersect (x) (union (fvs-X X) (fvs-v v))))
        "def-eliml"
        )
   (--> (= hnf x)
        (= x hnf)
        "swap"
        )
   (--> (in-hole X (exists x e))
        (exists x (in-hole X e))
        (where () (intersect (x) (fvs-X X)))
        (side-condition (not (equal? (term X) (term hole))))
        "def-float"
        )
   ;; Sequencing
   (--> (seq v e)
        e
        "seq")
   (--> (= (seq e_1 e_2) e_3)
        (seq e_1 (= e_2 e_3))
        "unify-seql"
        )
   (--> (= v (seq e_1 e_2))
        (seq e_1 (= v e_2))
        "unify-seqr"
        )
   (--> (= (= e_1 e_2) e_3)
        (exists x (seq* (= x e_1) (= x e_2) (= x e_3)))
        (fresh x)
        "unify-unifyl"
        )
   (--> (= e_1 (= e_2 e_3))
        (exists x (seq* (= x e_1) (= x e_2) (= x e_3)))
        (fresh x)
        "unify-unifyr"
        )
   ;; Fail propagation
   (--> (exists x fail)
        fail
        "fail-def"
        )
   (--> (in-hole X fail)
        fail
        (side-condition (not (equal? (term X) (term hole))))
        "fail"
        )
   (--> (in-hole SX (bar fail e))
        (in-hole SX e)
        "fail-l"
        )
   (--> (in-hole SX (bar e fail))
        (in-hole SX e)
        "fail-r"
        )
   (--> (in-hole SX (bar (bar e_1 e_2) e_3))
        (in-hole SX (bar e_1 (bar e_2 e_3)))
        "assoc-choice"
        )
   (--> (in-hole SX (in-hole CX (bar e_1 e_2)))
        (in-hole SX (bar (in-hole CX e_1) (in-hole CX e_2)))
        (side-condition (not (equal? (term CX) (term hole))))
        "choose"
        )
   ))

(define-extended-language verse-oneall verse+context
  (E ::= ....
     (bar v E)
  )
)

(define rules-oneall
  (extend-reduction-relation
   rules
   verse-oneall
   #:domain e
   ;; One and all
   (--> (one fail)
        fail
        "one-fail"
        )
   (--> (one (bar v e))
        v
        "one-choice"
        )
   (--> (one v)
        v
        "one-value"
        )
   (--> (all fail)
        (arr)
        "all-fail"
        )
   (--> (all (bar v e))
        (arr v_1 v_2 ...)
        (where (v_1 v_2 ...) (bar-values (bar v e)))
        "all-choice"
        )
   (--> (all v)
        (arr v)
        "all-value"
        )
  )
)

;;;;;;;;;;; SPLIT ;;;;;;;;;;;;;;;;;;;

(define rules-split
  (extend-reduction-relation
   rules
   verse+context
   #:domain e
   ;; Primitive operations
   (--> (split fail v_1 v_2)
        (v_1 (arr))
        "split-fail")
   (--> (split v_0 v_1 v_2)
        (exists t (seq (= t (v_2 v_0)) (t (lam x fail))))
        (fresh t)
        (fresh x)
        "split-value")
   (--> (split (bar v_0 e) v_1 v_2)
        (exists t (seq (= t (v_2 v_0)) (t (lam x e))))
        (fresh t)
        (fresh x)
        "split-choice")
   (--> (cons (arr v_1 (arr v_2 ...)))
        (arr v_1 v_2 ...)
        "cons")
   (--> (one e)
        (split e (lam x fail) (lam x (lam y x)))
        (fresh x)
        (fresh y)
        "one")
   (--> (all e)
        (exists*
         (f g)
         (seq*
          (= f (lam x (arr)))
          (= g
            (lam x
              (lam y
                (exists* (t a)
                  (seq* (= t (split (y (arr)) f g))
                        (= a (arr x t))
                        (cons a))))))
          (split e f g)
          )
         )
        (fresh f)
        (fresh g)
        (fresh t)
        (fresh a)
        (fresh x)
        (fresh y)
        "all")
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rules-oneall*
  (context-closure rules-oneall verse-oneall E))

(define rules-split*
  (context-closure rules-split verse+context E))

(define (reduce e r)
  (apply-reduction-relation* r
                             e
                             ;; #:all? #t
                             #:cache-all? #t
                             ;; #:error-on-multiple? #t
   )) 

(define rules-test rules-oneall*)
; (define test-rules rules-split*)  ;; this is very slow


(define (testred x y)
  (test-equal (reduce x rules-test) (list y)))

(define (testred1 x y)
  (test-->>E rules-test x y))

(define test0
  (term (exists a (seq (= a 1) (++ a a)))))
(define test1
  (term (all (exists i (seq (= i (bar 1 0)) ((arr 1 2 3) i))))))
(define test2
  (term ((lam x (++ x 1)) 3)))
(define test3
  (term (if* () (>> 1 0) 2 3)))
(define test4
  (term (if* () (>> 1 2) 2 3)))
(define test5
  (term (if* (a) (= a 2) a 3)))
(define test6
  (term (exists f (seq (= f (lam x (one (bar (= x 0) (:= y (-- x 1) (f y)))))) (f 1)))))
(define test7
  (term (exists f (seq (= f (lam x (if* () (= x 0) 99 (:= y (-- x 1) (f y))))) (f 1)))))

(define example1
  (term (exists* (x y z) (seq* (= x (arr y 3)) (= x (arr 2 z)) y))))

(define example2
  (term (:= first (lam pr (exists* (a b) (seq (= pr (arr a b)) a)))
            (exists* (x y) (seq* (= x (arr y 5)) (= (first x) 2) y)))))

(define example3
  (term (all (exists x (seq (= x (bar 7 5)) (arr 3 x))))))

(define example4
  (term (all (exists x (bar (seq (= x 3) (++ x 1)) (seq (= x 4) (** x 2)))))))

(define example5
  (term (all (:= t (arr 10 27 32) (t 1)))))

(define example6
  (term (all (:= t (arr 10 27 32) (t 3)))))

(define example7
  (term (all (:= t (arr 10 27 32) (:= a (bar* 1 0 1) (t a))))))

;; example5 append



(module+ test
  (testred test0 (term 2))
  (testred test1 (term (arr 2 1)))
  (testred test2 (term 4))
  (testred test3 (term 2))
  (testred test4 (term 3))
  (testred test5 (term 2))
  (testred1 test6 (term 0))
  ;;;;
  (testred example1 (term 2))
  ;; slow (testred example2 (term 2))
  (testred1 example2 (term 2))
  (testred example3 (term (arr (arr 3 7) (arr 3 5))))
  (testred example4 (term (arr 4 8)))
  (testred example5 (term (arr 27)))
  (testred example6 (term (arr)))
  ;; slow (testred example7 (term (arr 27 10 27)))
  (testred1 example7 (term (arr 27 10 27)))
  )

(module+ test
  (test-results)
  )
