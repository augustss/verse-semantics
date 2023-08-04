;;; TO DO
;;; contexts: match, join, and submatches
;;; diagrams
;;;   remove "u" from paths in diagrams (will need some Lambdas after all)
;;; Flip diagrams that are too wide
;;; Check decreasing diagrams


;;; Compute all critical pairs for the Verse Calculus

;;; It suffices to assume all variable-length tuples are length 2.
;;; We assume all rules are left-linear (makes matching trivial).

(setq the-grammar
      '((k ((quote 0)))			;integer
	(op ((quote add) (quote gt)))	;operation
	(d (k op (tup0) (tup2 v v)))	;data
	(hnf (d (lam x e)))		;head normal form
	(x ((quote var)))		;variable
	(v (x hnf))			;value
	(eq (e (= v e)))		;expression or equation
	(e (v (seq eq e) (exists x e) (quote fail) (choice e e) (app v v) (one e) (all e)))))			;expression

(defstruct context name arg-type result-type alternatives)

(setq the-contexts
      (list (make-context :name 'V :arg-type 'v :result-type 'v
			  :alternatives '(HOLE (tup2 V v) (tup2 v V)))
	    (make-context :name 'SX :arg-type 'e :result-type 'e
			  :alternatives '((one SC) (all SC)))
	    (make-context :name 'SC :arg-type 'e :result-type 'e
			  :alternatives '(HOLE (choice SC e) (choice e SC)))
	    (make-context :name 'CX :arg-type 'e :result-type 'e
			  :alternatives '(HOLE (seq (= v CX) e) (seq CX e) (seq ceq CX) (exists x CX)))
	    (make-context :name 'VX :arg-type 'e :result-type 'v
			  :alternatives '((lam x EX) (tup2 VX v) (tup2 v VX)))
	    (make-context :name 'EQX :arg-type 'e :result-type 'eq
			  :alternatives '(EX (= VX e) (= v EX)))
	    (make-context :name 'EX :arg-type 'e :result-type 'e
			  :alternatives '(HOLE VX (seq EQX e) (seq eq EX) (exists x EX) (choice EX e) (choice e EX) (app VX v) (all v VX) (one EX) (all EX)))))

(defstruct rule name lhs rhs cond (priority 0))

;;; Note that primes are used only in rule U-TUP, and that is in conjunction with integer subscripts.
;;; We may make use of that in function add-primes someday.
(setq the-rules
      (list (make-rule :name 'app-add :lhs '(app (quote add) (tup2 k1 k2)) :rhs 'k3 :cond '(compute k3 (+ k1 k2)))
	    (make-rule :name 'app-gt :lhs '(app (quote gt) (tup2 k1 k2)) :rhs 'k1 :cond '(if (> k1 k2)))
	    (make-rule :name 'app-gt-fail :lhs '(app (quote gt) (tup2 k1 k2)) :rhs '(quote fail) :cond '(if (not (> k1 k2))))
	    (make-rule :name 'app-beta :lhs '(app (lam x e) v) :rhs '(exists x (seq (= x v) e)) :cond '(if (not (elt x (fvs v)))))
	    (make-rule :name 'app-tup :lhs '(app (tup2 v0 v1) v) :rhs '(exists x (seq (= x v) (choice (seq (= x (quote 0)) v0) (seq (= x (quote 1)) v1)))) :cond '(fresh (not (elt x (fvs v v0 v1)))))
	    (make-rule :name 'app-tup-0 :lhs '(app (tup0) v) :rhs '(quote fail))
	    (make-rule :name 'u-lit :lhs '(seq (= k1 k2) e) :rhs 'e :cond '(if (= k1 k2)))
	    (make-rule :name 'u-tup :lhs '(seq (= (tup2 v1 vn) (tup2 v1prime vnprime)) e) :rhs '(seq (= v1 v1prime) (seq (= vn vnprime) e)))
	    (make-rule :name 'u-fail-op-d :lhs '(seq (= op d) e) :rhs '(quote fail))
	    (make-rule :name 'u-fail-d-op :lhs '(seq (= d op) e) :rhs '(quote fail))
	    (make-rule :name 'u-fail-tup-k :lhs '(seq (= (tup2 v1 vn) k) e) :rhs '(quote fail))
	    (make-rule :name 'u-fail-k-tup :lhs '(seq (= k (tup2 v1 vn)) e) :rhs '(quote fail))
	    ;; (make-rule :name 'u-occurs :lhs '() :rhs '())
	    ;; (make-rule :name 'unroll :lhs '() :rhs '())
	    ;; (make-rule :name 'subst :lhs '(seq (= x v) e) :rhs '(seq (= x v) (subst e v x)))
	    (make-rule :name 'hnf-swap :lhs '(seq (= hnf x) e) :rhs '(seq (= x hnf) e))
	    (make-rule :name 'var-swap :lhs '(seq (= x1 x2) e) :rhs '(seq (= x2 x1) e))
	    (make-rule :name 'seq-swap :lhs '(seq eq (seq (= x v) e)) :rhs '(seq (= x v) (seq eq e)))
	    (make-rule :name 'val-elim :lhs '(seq v e) :rhs 'e)
	    (make-rule :name 'exi-elim :lhs '(exists x e) :rhs 'e :cond '(if (not (elt x (fvs e)))))
	    (make-rule :name 'eqn-elim :lhs '(exists x (seq (= x v) e)) :rhs 'e :cond '(if (not (elt x (fvs v e)))))
	    (make-rule :name 'fail-elim-eq :lhs '(seq (= v (quote fail)) e) :rhs '(quote fail))
	    (make-rule :name 'fail-elim-l :lhs '(seq (quote fail) e) :rhs '(quote fail))
	    (make-rule :name 'fail-elim-r :lhs '(seq e (quote fail)) :rhs '(quote fail))
	    (make-rule :name 'exi-float-eq :lhs '(seq (= v (exists x e1)) e2) :rhs '(exists x (seq (= v e1) e2)) :cond '(if (not (elt x (fvs v e2)))))
	    (make-rule :name 'exi-float-l :lhs '(seq (exists x e1) e2) :rhs '(exists x (seq e1 e2)) :cond '(if (not (elt x (fvs e2)))))
	    (make-rule :name 'exi-float-r :lhs '(seq eq (exists x e)) :rhs '(exists x (seq eq e)) :cond '(if (not (elt x (fvs eq)))))
	    (make-rule :name 'eqn-float :lhs '(seq (= x (seq eq e1)) e2) :rhs '(seq eq (seq (= v1 v2) e2)))
	    (make-rule :name 'seq-assoc :lhs '(seq (seq eq e1) e2) :rhs '(seq eq (seq e1 e2)))
	    (make-rule :name 'exi-swap :lhs '(exists x1 (exists x2 e)) :rhs '(exists x2 (exists x1 e)))
	    (make-rule :name 'one-fail :lhs '(one (quote fail)) :rhs '(quote fail))
	    (make-rule :name 'one-value :lhs '(one v) :rhs 'v)
	    (make-rule :name 'one-choice :lhs '(one (choice v e)) :rhs 'v)
	    (make-rule :name 'all-fail :lhs '(all (quote fail)) :rhs '(tup0))
	    (make-rule :name 'all-value :lhs '(all v) :rhs '(tup1 v))
	    (make-rule :name 'all-choice-2 :lhs '(all (choice v1 vn)) :rhs '(tup2 v1 vn))
	    (make-rule :name 'all-choice-3 :lhs '(all (choice v1 (choice v2 v3))) :rhs '(tup3 v1 v2 v3))
	    (make-rule :name 'all-choice-4 :lhs '(all (choice v1 (choice v2 (choice v3 v4)))) :rhs '(tup4 v1 v2 v3 v4))
	    (make-rule :name 'choose-r :lhs '(choice (quote fail) e) :rhs 'e)
	    (make-rule :name 'choose-l :lhs '(choice e (quote fail)) :rhs 'e)
	    (make-rule :name 'choose-assoc :lhs '(choice (choice e1 e2) e3) :rhs '(choice e1 (choice e2 e3)))
	    ;; (make-rule :name 'choose :lhs '() :rhs '())
	    ))


(defstruct rewrite rulename path ellipsis)

;;;, A desired goal for tiles is that rewrites2 or alterewrites2 have at most one rewrite.
(defstruct proof rulename1 rulename2 path1 rewrites1 rewrites2 altrewrites1 altrewrites2)

;;; The bulk of this was constructed automatically by function print-proof-skeletons (below).
;; (print-proof-skeletons the-proofs)

(setq the-proofs                ;66 proofs
      (list (make-proof :rulename1 'u-lit :rulename2 'seq-swap :path1 '()
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'u-lit :path '(2))))
            (make-proof :rulename1 'u-lit :rulename2 'exi-float-r :path1 '()
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'u-lit :path '(2))))
            (make-proof :rulename1 'u-lit :rulename2 'seq-assoc :path1 '(1)
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'u-lit :path '())))
            (make-proof :rulename1 'u-tup :rulename2 'seq-swap :path1 '()
                        :rewrites1 (list (make-rewrite :rulename 'seq-swap :path '((high-exclusive 2))) (make-rewrite :rulename 'seq-swap :path '((low-exclusive 2)) :ellipsis t))
                        :rewrites2 (list (make-rewrite :rulename 'u-tup :path '(2))))
            (make-proof :rulename1 'u-tup :rulename2 'exi-float-r :path1 '()
                        :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '((high-exclusive 2))) (make-rewrite :rulename 'exi-float-r :path '((low-exclusive 2)) :ellipsis t))
                        :rewrites2 (list (make-rewrite :rulename 'u-tup :path '(2))))
            (make-proof :rulename1 'u-tup :rulename2 'seq-assoc :path1 '(1)
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'u-tup :path '())))
            (make-proof :rulename1 'u-fail-op-d :rulename2 'u-fail-d-op :path1 '()
                        :rewrites1 ()
                        :rewrites2 ())
            (make-proof :rulename1 'u-fail-op-d :rulename2 'seq-swap :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-op-d :path '())))
            (make-proof :rulename1 'u-fail-op-d :rulename2 'exi-float-r :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-op-d :path '())))
            (make-proof :rulename1 'u-fail-op-d :rulename2 'seq-assoc :path1 '(1)
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-op-d :path '())))
            (make-proof :rulename1 'u-fail-d-op :rulename2 'seq-swap :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-d-op :path '())))
            (make-proof :rulename1 'u-fail-d-op :rulename2 'exi-float-r :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-d-op :path '())))
            (make-proof :rulename1 'u-fail-d-op :rulename2 'seq-assoc :path1 '(1)
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-d-op :path '())))
            (make-proof :rulename1 'u-fail-tup-k :rulename2 'seq-swap :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-tup-k :path '())))
            (make-proof :rulename1 'u-fail-tup-k :rulename2 'exi-float-r :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-tup-k :path '())))
            (make-proof :rulename1 'u-fail-tup-k :rulename2 'seq-assoc :path1 '(1)
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-tup-k :path '())))
            (make-proof :rulename1 'u-fail-k-tup :rulename2 'seq-swap :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-k-tup :path '())))
            (make-proof :rulename1 'u-fail-k-tup :rulename2 'exi-float-r :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-k-tup :path '())))
            (make-proof :rulename1 'u-fail-k-tup :rulename2 'seq-assoc :path1 '(1)
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-k-tup :path '())))
            (make-proof :rulename1 'hnf-swap :rulename2 'seq-swap :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'hnf-swap :path '())))
            (make-proof :rulename1 'hnf-swap :rulename2 'exi-float-r :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'hnf-swap :path '())))
            (make-proof :rulename1 'hnf-swap :rulename2 'seq-assoc :path1 '(1)
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'hnf-swap :path '())))
            (make-proof :rulename1 'var-swap :rulename2 'seq-swap :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'var-swap :path '())))
            (make-proof :rulename1 'var-swap :rulename2 'seq-swap :path1 '(2)
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'var-swap :path '())))
            (make-proof :rulename1 'var-swap :rulename2 'exi-float-r :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'var-swap :path '())))
            (make-proof :rulename1 'var-swap :rulename2 'seq-assoc :path1 '(1)
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'var-swap :path '())))
            (make-proof :rulename1 'seq-swap :rulename2 'seq-swap :path1 '(2)
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
            (make-proof :rulename1 'seq-swap :rulename2 'val-elim :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'val-elim :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
            (make-proof :rulename1 'seq-swap :rulename2 'fail-elim-eq :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'fail-elim-eq :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
            (make-proof :rulename1 'seq-swap :rulename2 'fail-elim-l :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'fail-elim-l :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
            (make-proof :rulename1 'seq-swap :rulename2 'exi-float-eq :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-eq :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
            (make-proof :rulename1 'seq-swap :rulename2 'exi-float-l :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-l :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
            (make-proof :rulename1 'exi-float-r :rulename2 'seq-swap :path1 '(2)
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-r :path '())))
            (make-proof :rulename1 'seq-swap :rulename2 'eqn-float :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
            (make-proof :rulename1 'seq-swap :rulename2 'seq-assoc :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
            (make-proof :rulename1 'seq-swap :rulename2 'seq-assoc :path1 '(1)
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
            (make-proof :rulename1 'val-elim :rulename2 'fail-elim-r :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'fail-elim-r :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'val-elim :path '())))
            (make-proof :rulename1 'val-elim :rulename2 'exi-float-r :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'val-elim :path '())))
            (make-proof :rulename1 'val-elim :rulename2 'seq-assoc :path1 '(1)
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'val-elim :path '())))
            (make-proof :rulename1 'exi-elim :rulename2 'eqn-elim :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'eqn-elim :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-elim :path '())))
            (make-proof :rulename1 'exi-elim :rulename2 'exi-swap :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-swap :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-elim :path '())))
            (make-proof :rulename1 'exi-elim :rulename2 'exi-swap :path1 '(2)
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-swap :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-elim :path '())))
            (make-proof :rulename1 'eqn-elim :rulename2 'exi-swap :path1 '(2)
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-swap :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'eqn-elim :path '())))
            (make-proof :rulename1 'fail-elim-eq :rulename2 'exi-float-r :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-eq :path '())))
            (make-proof :rulename1 'fail-elim-eq :rulename2 'seq-assoc :path1 '(1)
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-eq :path '())))
            (make-proof :rulename1 'fail-elim-l :rulename2 'fail-elim-r :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'fail-elim-r :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-l :path '())))
            (make-proof :rulename1 'fail-elim-l :rulename2 'exi-float-r :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-l :path '())))
            (make-proof :rulename1 'fail-elim-l :rulename2 'seq-assoc :path1 '(1)
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-l :path '())))
            (make-proof :rulename1 'fail-elim-r :rulename2 'exi-float-l :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-l :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-r :path '())))
            (make-proof :rulename1 'fail-elim-r :rulename2 'seq-assoc :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-r :path '())))
            (make-proof :rulename1 'fail-elim-r :rulename2 'seq-assoc :path1 '(1)
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-r :path '())))
            (make-proof :rulename1 'exi-float-eq :rulename2 'exi-float-r :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-eq :path '())))
            (make-proof :rulename1 'exi-float-eq :rulename2 'seq-assoc :path1 '(1)
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-eq :path '())))
            (make-proof :rulename1 'exi-float-l :rulename2 'exi-float-r :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-l :path '())))
            (make-proof :rulename1 'exi-float-l :rulename2 'seq-assoc :path1 '(1)
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-l :path '())))
            (make-proof :rulename1 'exi-float-r :rulename2 'eqn-float :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-r :path '())))
            (make-proof :rulename1 'exi-float-r :rulename2 'seq-assoc :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-r :path '())))
            (make-proof :rulename1 'exi-float-r :rulename2 'seq-assoc :path1 '(1)
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-r :path '())))
            (make-proof :rulename1 'eqn-float :rulename2 'seq-assoc :path1 '(1)
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'eqn-float :path '())))
            (make-proof :rulename1 'seq-assoc :rulename2 'seq-assoc :path1 '(1)
                        :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'seq-assoc :path '())))
            (make-proof :rulename1 'exi-swap :rulename2 'exi-swap :path1 '(2)
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-swap :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-swap :path '())))
            (make-proof :rulename1 'choose-r :rulename2 'choose-l :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'choose-l :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'choose-r :path '())))
            (make-proof :rulename1 'choose-r :rulename2 'choose-assoc :path1 '(1)
                        :rewrites1 (list 'X (make-rewrite :rulename 'choose-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'choose-r :path '())))
            (make-proof :rulename1 'choose-l :rulename2 'choose-assoc :path1 '()
                        :rewrites1 (list 'X (make-rewrite :rulename 'choose-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'choose-l :path '())))
            (make-proof :rulename1 'choose-l :rulename2 'choose-assoc :path1 '(1)
                        :rewrites1 (list 'X (make-rewrite :rulename 'choose-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'choose-l :path '())))
            (make-proof :rulename1 'choose-assoc :rulename2 'choose-assoc :path1 '(1)
                        :rewrites1 (list 'X (make-rewrite :rulename 'choose-assoc :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'choose-assoc :path '())))))



;; (setq the-proofs                ;66 proofs
;;       (list (make-proof :rulename1 'u-lit :rulename2 'seq-swap :path1 '()
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-lit :path '(2))))
;;             (make-proof :rulename1 'u-lit :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-lit :path '(2))))
;;             (make-proof :rulename1 'u-lit :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-lit :path '())))
;;             (make-proof :rulename1 'u-lit :rulename2 'seq-swap :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-lit :path '())))
;;             (make-proof :rulename1 'u-lit :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-lit :path '())))
;;             (make-proof :rulename1 'u-lit :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-lit :path '())))
;;             (make-proof :rulename1 'u-tup :rulename2 'seq-swap :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-tup :path '())))
;;             (make-proof :rulename1 'u-tup :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-tup :path '())))
;;             (make-proof :rulename1 'u-tup :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-tup :path '())))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'u-fail-d-op :path1 '()
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'seq-swap :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-op-d :path '())))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-op-d :path '())))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-op-d :path '())))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'seq-swap :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-d-op :path '())))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-d-op :path '())))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-d-op :path '())))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'seq-swap :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-tup-k :path '())))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-tup-k :path '())))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-tup-k :path '())))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'seq-swap :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-k-tup :path '())))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-k-tup :path '())))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-k-tup :path '())))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'seq-swap :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'hnf-swap :path '())))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'hnf-swap :path '())))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'hnf-swap :path '())))
;;             (make-proof :rulename1 'var-swap :rulename2 'seq-swap :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'var-swap :path '())))
;;             (make-proof :rulename1 'var-swap :rulename2 'seq-swap :path1 '(2)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'var-swap :path '())))
;;             (make-proof :rulename1 'var-swap :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'var-swap :path '())))
;;             (make-proof :rulename1 'var-swap :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'var-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'seq-swap :path1 '(2)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'val-elim :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'val-elim :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'fail-elim-eq :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'fail-elim-eq :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'fail-elim-l :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'exi-float-eq :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-eq :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'exi-float-l :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-l :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'seq-swap :path1 '(2)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'eqn-float :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'seq-assoc :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'val-elim :rulename2 'fail-elim-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'fail-elim-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'val-elim :path '())))
;;             (make-proof :rulename1 'val-elim :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'val-elim :path '())))
;;             (make-proof :rulename1 'val-elim :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'val-elim :path '())))
;;             (make-proof :rulename1 'exi-elim :rulename2 'eqn-elim :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-elim :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'exi-elim :rulename2 'exi-swap :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'exi-elim :rulename2 'exi-swap :path1 '(2)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'eqn-elim :rulename2 'exi-swap :path1 '(2)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'eqn-elim :path '())))
;;             (make-proof :rulename1 'fail-elim-eq :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-eq :path '())))
;;             (make-proof :rulename1 'fail-elim-eq :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-eq :path '())))
;;             (make-proof :rulename1 'fail-elim-l :rulename2 'fail-elim-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'fail-elim-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-l :path '())))
;;             (make-proof :rulename1 'fail-elim-l :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-l :path '())))
;;             (make-proof :rulename1 'fail-elim-l :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-l :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'exi-float-l :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-l :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'seq-assoc :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'exi-float-eq :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-eq :path '())))
;;             (make-proof :rulename1 'exi-float-eq :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-eq :path '())))
;;             (make-proof :rulename1 'exi-float-l :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-l :path '())))
;;             (make-proof :rulename1 'exi-float-l :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-l :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'eqn-float :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'seq-assoc :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'eqn-float :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'eqn-float :path '())))
;;             (make-proof :rulename1 'seq-assoc :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-assoc :path '())))
;;             (make-proof :rulename1 'exi-swap :rulename2 'exi-swap :path1 '(2)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-swap :path '())))
;;             (make-proof :rulename1 'choose-r :rulename2 'choose-l :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'choose-l :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'choose-r :path '())))
;;             (make-proof :rulename1 'choose-r :rulename2 'choose-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'choose-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'choose-r :path '())))
;;             (make-proof :rulename1 'choose-l :rulename2 'choose-assoc :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'choose-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'choose-l :path '())))
;;             (make-proof :rulename1 'choose-l :rulename2 'choose-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'choose-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'choose-l :path '())))
;;             (make-proof :rulename1 'choose-assoc :rulename2 'choose-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'choose-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'choose-assoc :path '())))))



;; (setq the-proofs                ;66 proofs
;;       (list (make-proof :rulename1 'u-lit :rulename2 'seq-swap :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'u-lit :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'u-lit :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-lit :path '(2))))
;;             (make-proof :rulename1 'u-lit :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 ())
;;             (make-proof :rulename1 'u-tup :rulename2 'seq-swap :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'u-tup :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'u-tup :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-tup :path '())))
;;             (make-proof :rulename1 'u-tup :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-tup :path '())))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'u-fail-d-op :path1 '()
;;                         :rewrites1 ()
;;                         :rewrites2 ())
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'seq-swap :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'u-fail-op-d :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-op-d :path '())))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-op-d :path '())))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'seq-swap :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'u-fail-d-op :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-d-op :path '())))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-d-op :path '())))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'seq-swap :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'u-fail-tup-k :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-tup-k :path '())))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-tup-k :path '())))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'seq-swap :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'u-fail-k-tup :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-k-tup :path '())))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-k-tup :path '())))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'seq-swap :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'hnf-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'hnf-swap :path '())))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'hnf-swap :path '())))
;;             (make-proof :rulename1 'var-swap :rulename2 'seq-swap :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'var-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'var-swap :rulename2 'seq-swap :path1 '(2)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'var-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'var-swap :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'var-swap :path '())))
;;             (make-proof :rulename1 'var-swap :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'var-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'seq-swap :path1 '(2)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'val-elim :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'val-elim :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'fail-elim-eq :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-eq :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'fail-elim-l :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-l :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'exi-float-eq :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-eq :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'exi-float-l :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-l :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'seq-swap :path1 '(2)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'eqn-float :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'eqn-float :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'seq-assoc :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-assoc :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-assoc :path '())))
;;             (make-proof :rulename1 'val-elim :rulename2 'fail-elim-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'fail-elim-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'val-elim :path '())))
;;             (make-proof :rulename1 'val-elim :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'val-elim :path '())))
;;             (make-proof :rulename1 'val-elim :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'val-elim :path '())))
;;             (make-proof :rulename1 'exi-elim :rulename2 'eqn-elim :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-elim :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'exi-elim :rulename2 'exi-swap :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'exi-elim :rulename2 'exi-swap :path1 '(2)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'eqn-elim :rulename2 'exi-swap :path1 '(2)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'eqn-elim :path '())))
;;             (make-proof :rulename1 'fail-elim-eq :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-eq :path '())))
;;             (make-proof :rulename1 'fail-elim-eq :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-eq :path '())))
;;             (make-proof :rulename1 'fail-elim-l :rulename2 'fail-elim-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'fail-elim-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-l :path '())))
;;             (make-proof :rulename1 'fail-elim-l :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-l :path '())))
;;             (make-proof :rulename1 'fail-elim-l :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-l :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'exi-float-l :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-l :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'seq-assoc :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'exi-float-eq :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-eq :path '())))
;;             (make-proof :rulename1 'exi-float-eq :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-eq :path '())))
;;             (make-proof :rulename1 'exi-float-l :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-l :path '())))
;;             (make-proof :rulename1 'exi-float-l :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-l :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'eqn-float :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'seq-assoc :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'eqn-float :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'eqn-float :path '())))
;;             (make-proof :rulename1 'seq-assoc :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-assoc :path '())))
;;             (make-proof :rulename1 'exi-swap :rulename2 'exi-swap :path1 '(2)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-swap :path '())))
;;             (make-proof :rulename1 'choose-r :rulename2 'choose-l :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'choose-l :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'choose-r :path '())))
;;             (make-proof :rulename1 'choose-r :rulename2 'choose-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'choose-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'choose-r :path '())))
;;             (make-proof :rulename1 'choose-l :rulename2 'choose-assoc :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'choose-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'choose-l :path '())))
;;             (make-proof :rulename1 'choose-l :rulename2 'choose-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'choose-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'choose-l :path '())))
;;             (make-proof :rulename1 'choose-assoc :rulename2 'choose-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'choose-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'choose-assoc :path '())))))

;; (setq the-proofs   ;; HAND DONE
;;       (list (make-proof :rulename1 'u-lit :rulename2 'seq-swap-n :path1 '()
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-swap-n :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-lit :path '(2))))
;;             (make-proof :rulename1 'u-lit :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-lit :path '(2))))
;;             (make-proof :rulename1 'u-lit :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-tup :rulename2 'seq-swap-n :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap-n :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-tup :path '())))
;;             (make-proof :rulename1 'u-tup :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-tup :path '())))
;;             (make-proof :rulename1 'u-tup :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-tup :path '())))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'u-fail-d-op :path1 '()
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'seq-swap-n :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap-n :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-op-d :path '())))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-op-d :path '())))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-op-d :path '())))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'seq-swap-n :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap-n :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-d-op :path '())))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-d-op :path '())))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-d-op :path '())))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'seq-swap-n :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap-n :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-tup-k :path '())))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-tup-k :path '())))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-tup-k :path '())))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'seq-swap-n :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap-n :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-k-tup :path '())))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-k-tup :path '())))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-k-tup :path '())))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'seq-swap-n :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap-n :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'hnf-swap :path '())))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'hnf-swap :path '())))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'hnf-swap :path '())))
;;             (make-proof :rulename1 'var-swap :rulename2 'seq-swap-s :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap-s :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'var-swap :path '())))
;;             (make-proof :rulename1 'var-swap :rulename2 'seq-swap-s :path1 '(2)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap-s :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'var-swap :path '())))
;;             (make-proof :rulename1 'var-swap :rulename2 'seq-swap-n :path1 '(2)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap-n :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'var-swap :path '())))
;;             (make-proof :rulename1 'var-swap :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'var-swap :path '())))
;;             (make-proof :rulename1 'var-swap :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'var-swap :path '())))
;;             (make-proof :rulename1 'seq-swap-s :rulename2 'seq-swap-s :path1 '(2)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap-s :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap-s :path '())))
;;             (make-proof :rulename1 'seq-swap-s :rulename2 'seq-swap-n :path1 '(2)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap-n :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap-s :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'seq-swap-s :path1 '(2)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap-s :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'seq-swap-s :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap-s :path '())))
;;             (make-proof :rulename1 'val-elim :rulename2 'seq-swap-n :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap-n :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'val-elim :path '())))
;;             (make-proof :rulename1 'fail-elim-eq :rulename2 'seq-swap-n :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap-n :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-eq :path '())))
;;             (make-proof :rulename1 'fail-elim-l :rulename2 'seq-swap-n :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap-n :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-l :path '())))
;;             (make-proof :rulename1 'exi-float-eq :rulename2 'seq-swap-n :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap-n :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-eq :path '())))
;;             (make-proof :rulename1 'exi-float-l :rulename2 'seq-swap-n :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap-n :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-l :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'seq-swap-n :path1 '(2)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap-n :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'eqn-float :rulename2 'seq-swap-n :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap-n :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'eqn-float :path '())))
;;             (make-proof :rulename1 'seq-assoc :rulename2 'seq-swap-n :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-swap-n :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-assoc :path '())))
;;             (make-proof :rulename1 'seq-swap-n :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap-n :path '())))
;;             (make-proof :rulename1 'val-elim :rulename2 'fail-elim-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'fail-elim-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'val-elim :path '())))
;;             (make-proof :rulename1 'val-elim :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'val-elim :path '())))
;;             (make-proof :rulename1 'val-elim :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'val-elim :path '())))
;;             (make-proof :rulename1 'exi-elim :rulename2 'eqn-elim :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-elim :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'exi-elim :rulename2 'exi-swap :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'exi-elim :rulename2 'exi-swap :path1 '(2)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'eqn-elim :rulename2 'exi-swap :path1 '(2)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'eqn-elim :path '())))
;;             (make-proof :rulename1 'fail-elim-eq :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-eq :path '())))
;;             (make-proof :rulename1 'fail-elim-eq :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-eq :path '())))
;;             (make-proof :rulename1 'fail-elim-l :rulename2 'fail-elim-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'fail-elim-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-l :path '())))
;;             (make-proof :rulename1 'fail-elim-l :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-l :path '())))
;;             (make-proof :rulename1 'fail-elim-l :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-l :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'exi-float-l :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-l :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'seq-assoc :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'exi-float-eq :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-eq :path '())))
;;             (make-proof :rulename1 'exi-float-eq :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-eq :path '())))
;;             (make-proof :rulename1 'exi-float-l :rulename2 'exi-float-r :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-l :path '())))
;;             (make-proof :rulename1 'exi-float-l :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-l :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'eqn-float :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'seq-assoc :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'eqn-float :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'eqn-float :path '())))
;;             (make-proof :rulename1 'seq-assoc :rulename2 'seq-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-assoc :path '())))
;;             (make-proof :rulename1 'exi-swap :rulename2 'exi-swap :path1 '(2)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-swap :path '())))
;;             (make-proof :rulename1 'choose-r :rulename2 'choose-l :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'choose-l :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'choose-r :path '())))
;;             (make-proof :rulename1 'choose-r :rulename2 'choose-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'choose-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'choose-r :path '())))
;;             (make-proof :rulename1 'choose-l :rulename2 'choose-assoc :path1 '()
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'choose-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'choose-l :path '())))
;;             (make-proof :rulename1 'choose-l :rulename2 'choose-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'choose-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'choose-l :path '())))
;;             (make-proof :rulename1 'choose-assoc :rulename2 'choose-assoc :path1 '(1)
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'choose-assoc :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'choose-assoc :path '())))))


(defun canonical-nt (nt)
  (intern (strip-decorations (symbol-name nt))))

(defun strip-decorations (str)
  (let ((n (length str)))
    (cond ((string= str "vn") "v")   ;; Support the special n-tuple hack
	  ((cl-digit-char-p (elt str (- n 1)))
           (strip-decorations (substring str 0 (- n 1))))
          ((and (> n 5) (string= (downcase (substring str (- n 5) n)) "prime"))
           (strip-decorations (substring str 0 (- n 5))))
          ((and (> n 3) (string= (downcase (substring str (- n 3) n)) "hat"))
           (strip-decorations (substring str 0 (- n 3))))
          (t str))))

(defun add-prime (str)
  (let ((n (length str)))
    (cond ((cl-digit-char-p (elt str (- n 1)))
           (concat (add-prime (substring str 0 (- n 1))) (substring str (- n 1) n)))
          ((and (> n 5) (string= (downcase (substring str (- n 5) n)) "prime"))
           (concat (add-prime (substring str 0 (- n 5))) (substring str (- n 5) n)))
          ((and (> n 3) (string= (downcase (substring str (- n 3) n)) "hat"))
           (concat (add-prime (substring str 0 (- n 3))) (substring str (- n 3) n)))
          (t (concat str "prime")))))

(defun add-prime (str) (concat str "prime"))

;; Add a prime to every variable that needs one in the term alpha.
(defun add-primes (alpha vars-to-avoid)
  (cond ((atom alpha)
	 (cond ((member alpha vars-to-avoid)
		(intern (add-prime (symbol-name alpha))))
	       (t alpha)))
	((eq (first alpha) 'quote) alpha)
	(t (cons (first alpha) (mapcar #'(lambda (x) (add-primes x vars-to-avoid)) (rest alpha))))))

(defun nt-lookup (nt)
  (let ((pair (assoc (canonical-nt nt) the-grammar)))
    (cond ((null pair) (error "nt-lookup on %s failed" nt))
	  (t (cadr pair)))))

(defun pretty-alist (alist)   ;; Make pretty for printing purposes
  (mapcar #'(lambda (pair) (list (car pair) '-> (cdr pair))) alist))

(defun replace-subterm (t1 path t2)
  (cond ((null path) t2)
        ((or (atom t1) (eq (first t1) 'quote))
         (error "replace-subterm overlong path %s" path))
        ((or (< (first path) 1) (> (first path) (length t1)))
         (error "replace-subterm path %s element out of range" path))
        (t (do ((x t1 (rest x))
                (n 0 (+ n 1))
                (result '() (cons (if (= n (first path)) 
                                      (replace-subterm (first x) (rest path) t2)
                                    (first x))
                                  result)))
               ((null x) (reverse result))))))

(defun term-vars (term)
  (cond ((atom term) (list term))
	((eq (first term) 'quote) '())
	(t (reduce #'union (mapcar #'term-vars (rest term)) :initial-value '()))))

(defstruct joinresult N sigma1 sigma2)

;;; On success, return 3-list (N, sigma1, sigma2) such that N = unify(t1, t2), sigma1(t1)=N, and sigma2(t2)=N.
;;; (When matching metavariables, prefers metavariables from t2 for use in the joined term N.)
;;; On failure, return nil.
(defun joinable (t1 t2)
  (unless t1 (error "joinable: null term t1"))
  (unless t2 (error "joinable: null term t2"))
  (cond ((and (atom t1) (atom t2) (eq (canonical-nt t1) (canonical-nt t2)))
         (make-joinresult :N t2 :sigma1 (list (cons t1 t2)) :sigma2 '()))
        ((atom t1)
         (let ((sj (find-if #'identity (mapcar #'(lambda (opt1) (joinable opt1 t2)) (nt-lookup t1)))))
           (and sj (make-joinresult :N t2 :sigma1 (list (cons t1 t2)) :sigma2 '()))))
        ((atom t2)
         (let ((sj (find-if #'identity (mapcar #'(lambda (opt2) (joinable t1 opt2)) (nt-lookup t2)))))
           (and sj (make-joinresult :N t1 :sigma1 '() :sigma2 (list (cons t2 t1))))))
        ((eq (first t1) (first t2))
         (cond ((eq (first t1) 'quote)
                (and (eq (second t1) (second t2))
                     (make-joinresult :N t1 :sigma1 '() :sigma2 '())))
	       (t (let ((sjs (cl-mapcar #'joinable (rest t1) (rest t2))))
                    (and (every #'identity sjs)
                         (make-joinresult :N (cons (first t1) (mapcar #'joinresult-N sjs))
					  :sigma1 (apply #'append (mapcar #'joinresult-sigma1 sjs))
					  :sigma2 (apply #'append (mapcar #'joinresult-sigma2 sjs))))))))
	(t nil)))

(defstruct critpair rule1 rule2 path1 sigma1 sigma2 term term1 term2)

(defun submatches (M rule1 rule2 path1 eqok)
  (let ((name1 (rule-name rule1)) (alpha1 (rule-lhs rule1)) (beta1 (rule-rhs rule1))
        (name2 (rule-name rule2)) (alpha2 (rule-lhs rule2)) (beta2 (rule-rhs rule2)))
    (and (not (atom M))
         (not (atom alpha2))
         (eq (first M) (first alpha2))
         (append (and eqok
                      (let ((jn (joinable M alpha2)))
                        (and jn (let ((N (joinresult-N jn))
				      (sigma1 (joinresult-sigma1 jn))
                                      (sigma2 (joinresult-sigma2 jn)))
                                  (list (make-critpair :rule1 rule2
						       :rule2 rule1   ;Put rule1 second because it has the primes
						       :path1 path1
						       :sigma1 sigma2  ;Similarly swap the sigmas
						       :sigma2 sigma1
						       :term (replace-subterm alpha1 path1 N)
						       :term1 (sublis sigma1 beta1)
						       :term2 (replace-subterm (sublis sigma1 alpha1) path1 (sublis sigma2 beta2))))))))
		 (and (not (eq (first M) 'quote))
                      (do ((z2 (rest M) (rest z2))
                           (k 1 (+ k 1))
                           (matches '() (append matches (submatches (first z2) rule1 rule2 (append path1 (list k)) t))))
                          ((null z2) matches)))))))

(defun add-primes-to-rule (rule vars-to-avoid)
  (make-rule :name (rule-name rule) 
	     :lhs (add-primes (rule-lhs rule) vars-to-avoid)
	     :rhs (add-primes (rule-rhs rule) vars-to-avoid)
	     :cond (and (rule-cond rule) (add-primes (rule-cond rule) vars-to-avoid))))

(defun all-submatches (rule1 rule2 same)
  (let ((rc1 (rule-cond rule1))
	(rc2 (rule-cond rule2)))
    (cond ((and (not (atom rc1))
		(not (atom rc2))
		(eq (first rc1) 'if)
		(eq (first rc2) 'if)
		(or (and (eq (first (second rc1)) 'not)
			 (equal (second (second rc1)) (second rc2)))
		    (and (eq (first (second rc2)) 'not)
			 (equal (second (second rc2)) (second rc1)))))
	   ;; Rules handle logically complementary cases, so no overlapping applications
	   '())
	  (t (let ((rulehat1 (add-primes-to-rule rule1 (union (term-vars (rule-lhs rule2)) (term-vars (rule-rhs rule2))) ))
		   (rulehat2 (add-primes-to-rule rule2 (union (term-vars (rule-lhs rule1)) (term-vars (rule-rhs rule1))))))
	       (cond (same (submatches (rule-lhs rulehat2) rulehat2 rule1 '() nil)) 
		     ((> (rule-priority rule1) (rule-priority rule2))
		      (append (submatches (rule-lhs rulehat1) rulehat1 rule2 '() t)
			      (submatches (rule-lhs rulehat2) rulehat2 rule1 '() nil)))
		     ((> (rule-priority rule2) (rule-priority rule1))
		      (append (submatches (rule-lhs rulehat2) rulehat2 rule1 '() t)
			      (submatches (rule-lhs rulehat1) rulehat1 rule2 '() nil)))
		     (t (append (submatches (rule-lhs rulehat2) rulehat2 rule1 '() t)
				(submatches (rule-lhs rulehat1) rulehat1 rule2 '() nil)))))))))
	       
(defun all-critical-pairs ()
  (do ((z the-rules (rest z))
       (result '() (append result (do ((y z (rest y))
                                       (same t nil)
				       (subresult '() (append subresult (all-submatches (first z) (first y) same))))
				      ((null y) subresult)
				    ;; (print (list (first z) (first y)))
				    ))))
      ((null z) result)))

(defun verify-subst-consistency (substitution)
  (do ((s substitution (rest s)))
      ((null s) t)
    (do ((z (rest s) (rest z)))
	((null z))
      (when (eq (car (first s)) (car (first z)))
	(unless (equal (cdr (first s)) (cdr (first z)))
	  (return nil))))))

;;; Here sigma is the substitution for variables, theta is the substitution for other contexts,
;;; and context is the built-up context being matched.
(defstruct matchresult sigma theta context)

;;; Finds out whether the term matches the pattern (an extended term), producing a substitution that will turn the pattern into the term.
;;; On failure, return nil. Note that a contradictory substitution can be one reason for failure.
(defun matches (term pattern)
  (let ((result (recursive-match term pattern)))
    (and result
	 (verify-subst-consistency (matchresult-sigma result))
	 (verify-subst-consistency (matchresult-theta result))
	 result)))

(defun recursive-match (term pattern)
  ;; (princ (format "*** matching %s to %s\n" term pattern))
  (unless term (error "recursive-match: null term"))
  (unless pattern (error "recursive-match: null pattern"))
  (cond ((and (atom term) (atom pattern) (eq (canonical-nt term) (canonical-nt pattern)))
         (make-matchresult :sigma (list (cons pattern term))))
        ((atom pattern)
         (let ((sj (find-if #'identity (mapcar #'(lambda (pattern-option) (recursive-match term pattern-option)) (nt-lookup pattern)))))
	   ;; (unless sj (princ (format "\nFAILED SUBMATCH of %s to %s\n" term pattern)))
           (and sj (make-matchresult :sigma (list (cons pattern term))))))
	((eq (first pattern) 'context)
	 (match-context term (context-lookup (second pattern)) (third pattern)))
        ((atom term) nil)
        ((eq (first term) (first pattern))
         (cond ((eq (first term) 'quote)
                (and (eq (second term) (second pattern))
                     (make-matchresult :sigma '())))
	       (t (let ((ms (cl-mapcar #'recursive-match (rest term) (rest pattern))))
                    (and (every #'identity ms)
                         (make-matchresult :sigma (apply #'append (mapcar #'matchresult-sigma ms))
					   :theta (apply #'append (mapcar #'matchresult-theta ms))))))))
	(t nil)))

(defun canonical-path (path)
  (apply #'append (mapcar #'(lambda (item)
			      (cond ((numberp item) (list item))
				    ((atom item) (error "Unknown path item %s" item))
				    ((eq (first item) 'low-exclusive) (list))
				    ((eq (first item) 'high-exclusive) (list (second item)))
				    ((eq (first item) 'low-inclusive) (list (second item)))
				    ((eq (first item) 'high-exclusive) (list (second item) (second item)))
				    (t (error "Unknown path item %s" item))))
			  path)))

(defun apply-rewrite-rule (rule term path)
  ;; (princ (format "$$$$ Applying rewrite rule %s to term %s / %s" (rule-name rule) term path))
  (let ((res (try-rewrite-rule rule term (canonical-path path))))
    (or res (error "Applying rewrite rule %s to term %s / %s failed" (rule-name rule) term path))))

(defun try-rewrite-rule (rule term path)
  (cond ((null path)
	 (let ((m (matches term (rule-lhs rule))))
	   ;; (unless m (princ (format "\nFAILED MATCH of %s to %s of %s\n" term (rule-lhs rule) (rule-name rule))))
	   (and m (sublis (matchresult-sigma m) (rule-rhs rule)))))
	((atom term) nil)
	(t (let ((subterm (try-rewrite-rule rule (nth (first path) term) (rest path))))
	     (and subterm
		  (append (subseq term 0 (first path))
			  (list subterm)
			  (subseq term (+ (first path) 1))))))))

(defun format-path (path)
  (cond ((null path) "\\emptypath")
	(t (format-partial-path path))))

(defun format-partial-path (path)
  (apply #'concat
	 (mapcar #'(lambda (item)
		     (cond ((numberp item) (format "\\%s" item))
			   ((atom item) (error "Unknown path item %s" item))
			   ((eq (first item) 'low-exclusive) (format "\\%s^{0}" (second item)))
			   ((eq (first item) 'high-exclusive) (format "\\%s^{n-1}" (second item)))
			   ((eq (first item) 'low-inclusive) (format "\\%s^{1}" (second item)))
			   ((eq (first item) 'high-exclusive) (format "\\%s^{n}" (second item)))
			   (t (error "Unknown path item %s" item))))
		 path)))

(defun format-term (term)
  (concat "|" (format-subterm term t) "|"))

(defun format-term-big-parens (term)
  (concat "|" (format-subterm term 'big) "|"))

(defun format-rule-term (term)
  (concat "|" (format-subterm term nil) "|"))

(defun format-subterm (term parens)
  (cond ((atom term) (format-nt term))
	((eq (first term) 'quote)
	 (format "%s" (second term)))
	((or (not parens)
	     (eq (first term) '=)
	     (eq (first term) 'one)
	     (eq (first term) 'all)
	     )
	 (format-compound-subterm term))
	((eq parens 'big) (concat "\\bigl(" (format-compound-subterm term) "\\bigr)"))
	(t (concat "(" (format-compound-subterm term) ")"))))

(defun format-compound-subterm (term)
  (cond ((eq (first term) 'seq)
	 (format "%s; %s"
		 (format-subterm (second term) t)
		 (format-subterm (third term) (and (not (atom (third term)))
						   (or (not (eq (first (third term)) 'seq))
						       (atom (second (third term)))
						       (not (eq (first (second (third term))) '=))
						       (not (member (second (second (third term))) '(vn vnprime))))
						   ))))
	((eq (first term) '=)
	 (cond ((and (member (second term) '(vn vnprime)) (member (third term) '(vn vnprime)))
		;; Special-case hack for a pair of equalities that stand in for a general n-sequence of equalities
		(format "xdots %s = %s" (format-subterm (second term) nil) (format-subterm (third term) nil)))
	       (t (format "%s = %s" (format-subterm (second term) t) (format-subterm (third term) t)))))
	((eq (first term) 'exists)
	 (format "def %s (%s)" (format-subterm (second term) t) (format-subterm (third term) t)))
	((eq (first term) 'choice)
	 (cond ((and (member (second term) '(v1 v1prime)) (member (third term) '(vn vnprime)))
		;; Special-case hack for a 2-choice that stands in for a general n-way choice (for "all")
		(format "%s `choice` xdots `choice` %s" (format-subterm (second term) nil) (format-subterm (third term) nil)))
	       (t (format "%s `choice` %s" (format-subterm (second term) t) (format-subterm (third term) t)))))
	((eq (first term) 'app)
	 (format "%s %s" (format-subterm (second term) t) (format-subterm (third term) t)))
	((eq (first term) 'one)
	 (format "one{%s}" (format-subterm (second term) nil)))
	((eq (first term) 'all)
	 (format "all{%s}" (format-subterm (second term) nil)))
	((eq (first term) 'lam)
	 (format "lam %s %s" (format-subterm (second term) nil) (format-subterm (third term) t)))
	((eq (first term) 'tup0)
	 (format "tup ()"))
	((eq (first term) 'tup1)
	 (format "tup (%s)" (format-subterm (second term) nil)))
	((eq (first term) 'tup2)
	 (cond ((and (member (second term) '(v1 v1prime)) (member (third term) '(vn vnprime)))
		;; Special-case hack for a 2-tuple that stands in for a general n-tuple
		(format "tup (%s,xdots,%s)" (format-subterm (second term) nil) (format-subterm (third term) nil)))
	       (t (format "tup (%s,%s)" (format-subterm (second term) nil) (format-subterm (third term) nil)))))
	((eq (first term) 'tup3)
	 (format "tup (%s,%s,%s)" (format-subterm (second term) nil) (format-subterm (third term) nil) (format-subterm (fourth term) nil)))
	((eq (first term) 'tup4)
	 (format "tup (%s,%s,%s,%s)" (format-subterm (second term) nil) (format-subterm (third term) nil) (format-subterm (fourth term) nil) (format-subterm (fifth term) nil)))
	(t (error "format-compound-term: unknown term type %s" (first term)))))

(defun format-rule-condition (rc)
  (cond ((atom rc) (error "format-rule-condition: unknown atomic condition %s" rc))
	((eq (first rc) 'compute)
	 (unless (= (length rc) 3)  (error "format-rule-condition: wrong number of arguments %s" rc))
	 (format "\\text{where $%s=%s$}" (format-rule-condition-expression (second rc)) (format-rule-condition-expression (third rc))))
	((eq (first rc) 'fresh)
	 (unless (= (length rc) 2)  (error "format-rule-condition: wrong number of arguments %s" rc))
	 (format "\\text{fresh $%s$}" (format-rule-condition-expression (second rc))))
	((eq (first rc) 'if)
	 (unless (= (length rc) 2)  (error "format-rule-condition: wrong number of arguments %s" rc))
	 (format "\\text{if $%s$}" (format-rule-condition-expression (second rc))))
	(t (error "format-rule-condition: unknown condition %s" rc))))

(defun format-rule-condition-expression (rce)
  (cond ((atom rce) (format "|%s|" (format-nt rce)))
	((eq (first rce) 'fvs)
	 (format "\\freevars{%s}" (mapconcat #'format-rule-condition-expression (rest rce) ",")))
	((eq (first rce) '=) (format "%s=%s" (format-rule-condition-expression (second rce)) (format-rule-condition-expression (third rce))))
	((eq (first rce) '>) (format "%s>%s" (format-rule-condition-expression (second rce)) (format-rule-condition-expression (third rce))))
	((eq (first rce) '+) (format "%s+%s" (format-rule-condition-expression (second rce)) (format-rule-condition-expression (third rce))))
	((eq (first rce) 'and) (format "%s\\logand %s" (format-rule-condition-expression (second rce)) (format-rule-condition-expression (third rce))))
	((eq (first rce) 'or) (format "%s\\logor %s" (format-rule-condition-expression (second rce)) (format-rule-condition-expression (third rce))))
	((eq (first rce) 'elt) (format "%s\\in %s" (format-rule-condition-expression (second rce)) (format-rule-condition-expression (third rce))))
	((eq (first rce) 'not)
	 (cond ((eq (first (second rce)) '>)
		(format "%s\\leq %s" (format-rule-condition-expression (second (second rce))) (format-rule-condition-expression (third (second rce)))))
	       ((eq (first (second rce)) 'elt)
		(format "%s\\not\\in %s" (format-rule-condition-expression (second (second rce))) (format-rule-condition-expression (third (second rce)))))
	       (t (format "\\neg(%s)" (format-rule-condition-expression (second rce))))))
	(t (error "format-rule-condition-expression: unknown expression %s" rce))))
		
(defun format-nt (nt)
  (format-metavar (symbol-name nt)))

(defun format-metavar (str)
  (let ((n (length str)))
    (cond ((and (> n 5) (string= (downcase (substring str (- n 5) n)) "prime"))
	   (concat (format-metavar (substring str 0 (- n 5))) "'"))
          (t str))))

(defun print-rule-line (prefix name alpha beta cond linebreak)
  (princ (format "\\hbox to 5em{%s\\hfill}\\hbox to 6em{\\rulename{%s}\\hfill}\\hbox to 8em{\\hss %s}\\quad$\\movesto$\\quad %s"
		 prefix name (format-rule-term alpha) (format-rule-term beta)))
  (when cond
    (princ (format "\\hfill %s" (format-rule-condition cond))))
  (princ (format "\\relax%s\n" linebreak)))

;;; Returns a 3-list of (formatted-rewrites1 final-term formatted-rewrites2).
(defun format-rewrites-pair (rws1 R rws2 P)
  (let ((fr1 (format-rewrites rws1 nil R))
	(fr2 (format-rewrites rws2 t P)))
    (cond ((equal (second fr1) (second fr2))
	   (list (first fr1) (second fr1) (first fr2)))
	  (t (error "rewrites pair from %s and %s failed to converge: %s and %s" R P (second fr1) (second fr2))))))

;;; Applies a list of rewrites to a term.
;;; Returns a pair of formatted text and the final term produced.
(defun format-rewrites (rws rev term)
  (do ((z rws (rest z))
       (tm term (and term    ;Yes, "term", not "tm" here, in the "and"
		     (apply-rewrite-rule (rule-lookup (rewrite-rulename (first z))) tm (rewrite-path (first z)))))
       (result "" (concat result (format-one-rewrite (first z) rev tm))))
      ((null z) (list result tm))))

(defun format-one-rewrite (rw rev term)
  (let ((base-format
	 (format "\\xrn%sgosup{%s}{%s}"
		 (if rev "un" "")
		 (rewrite-rulename rw)
		 (format-path (rewrite-path rw)))))
    (let ((extra (if (rewrite-ellipsis rw) "\\mydots" (format-term term))))
      (if rev (concat base-format extra) (concat extra base-format)))))

(defun format-sigma (sigma)
  (cond ((null sigma) "\\{\\,\\}")
	(t (concat "\\{\\,"
		   (mapconcat #'(lambda (substpair)
				  (format "%s\\mapsto %s"
					  (format-rule-term (car substpair))
					  (format-rule-term (cdr substpair))))
			      sigma
			      ",\\,")
		   "\\,\\}"))))

(defun print-critical-pair (cp k)
  (let ((rule1 (critpair-rule1 cp))
	(rule2 (critpair-rule2 cp))
	(path1 (critpair-path1 cp))
	(sigma1 (critpair-sigma1 cp))
	(sigma2 (critpair-sigma2 cp))
	(P (critpair-term1 cp))
	(Q (critpair-term cp))
	(R (critpair-term2 cp)))
    (let ((name1 (rule-name rule1))
	  (name2 (rule-name rule2))
	  (alpha1 (rule-lhs rule1))
	  (alpha2 (rule-lhs rule2))
	  (beta1 (rule-rhs rule1))
	  (beta2 (rule-rhs rule2))
	  (rc1 (rule-cond rule1))
	  (rc2 (rule-cond rule2))
	  (linebreak "\\vadjust{\\penalty1000}\\hfil\\break")) ;Use \\hfil here, and \\hfill in print-rule-line
      (princ (format "\\vskip 8pt plus 16pt\\noindent\n"))
      (let ((weirdtext "and{\\hskip0.5em}rule"))
	(print-rule-line (format "\\rlap{(%s)}\\hphantom{%s}\\llap{Rule}" k weirdtext) name1 alpha1 beta1 rc1 linebreak)
	(print-rule-line weirdtext name2 alpha2 beta2 rc2 linebreak))
      (princ (format "have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \\equiv %s$%s\n"
		     (format-term Q) linebreak))
      (princ (format "obtained from {\\color{blue}$\\sigma_1=%s$} and {\\color{blue}$\\sigma_2=%s$}:\\hfill%s\n"
		     (format-sigma sigma1) (format-sigma sigma2) linebreak))
      ;; Note that name1 is on the left edge; name2 is on the top edge.
      (princ (format "\\null\\hskip 2em minus 1.95em $|t_1| \\equiv %s \\xrnungosup{%s}{%s} |t| \\xrngosup{%s}{\\emptypath} %s \\equiv |t_2|$.%s\n"
		     (format-term R) name1 (format-path path1) name2 (format-term P) linebreak))
      (let ((pf (proof-lookup name1 name2 path1)))
	(cond ((null pf)
	       (princ (format "{\\color{red}Can they be joined? $|t_1| \\xrngosup{%s}{\\emptypath} \\bigl(|t'|\\bigr) \\xrnungosup{%s}{%s} |t_2|$.%s}\n%s\n%s\n%s\n"
			      name2 name1 (format-path path1) linebreak linebreak linebreak linebreak)))
	      ((and (eq (first (proof-rewrites1 pf)) 'X)
		    (eq (first (proof-rewrites2 pf)) 'X))
	       (princ (format "{\\color{purple}Can they be joined? $|t_1| %s \\bigl(|t'|\\bigr) %s |t_2|$.%s}\n%s\n%s\n%s\n"
			      (format-rewrites (rest (proof-rewrites1 pf)) nil nil) (format-rewrites (rest (proof-rewrites2 pf)) t nil) linebreak linebreak linebreak linebreak)))
	      (t (print-given-proof-rewrites "They can be joined" (proof-rewrites1 pf) R (proof-rewrites2 pf) P)
		 (when (or (proof-altrewrites1 pf) (proof-altrewrites2 pf)))
		 (print-tikzcd-diagram P Q R name1 name2 path1 (proof-rewrites1 pf) (proof-rewrites2 pf) k))))
      (princ (format "Therefore rules \\rulename{%s} and \\rulename{%s} have the XXX property.\\par\n" name1 name2)))))

(defun print-given-proof-rewrites (prefix rw1 R rw2 P)
  (let ((fr (format-rewrites-pair rw1 R rw2 P)))
    (cond ((> (length rw1) 1)
	   (princ (format "{%s: $|t_1| \\equiv %s {}$%s\\null\\hskip 2em minus 1.95em$\\bigl(%s\\bigr) %s \\equiv |t_2|$.%s}\n"
			    prefix (first fr) linebreak (format-rule-term (second fr)) (third fr) linebreak)))
	  ((> (length rw2) 1)
	   (princ (format "{%s: $|t_1| \\equiv %s \\bigl(%s\\bigr)$%s\\null\\hskip 2em minus 1.95em${} %s \\equiv |t_2|$.%s}\n"
			    prefix (first fr) (format-rule-term (second fr)) linebreak (third fr) linebreak)))
	  (t (princ (format "{%s: $|t_1| \\equiv %s \\bigl(%s\\bigr) %s \\equiv |t_2|$.%s}\n"
			    prefix (first fr) (format-rule-term (second fr)) (third fr) linebreak))))))

(defun proof-lookup (name1 name2 path1)
  (let ((result
	 (find-if #'(lambda (pf) (and (eq (proof-rulename1 pf) name1)
				      (eq (proof-rulename2 pf) name2)
				      (equal (proof-path1 pf) path1)))
		  the-proofs)))
    ;; (print (list (list 'proof-lookup name1 name2 path1) result))
    result))

(defun rule-lookup (rulename)
  (let ((result
	 (find-if #'(lambda (rule) (eq (rule-name rule) rulename)) the-rules)))
    ;; (print (list (list 'rule-lookup name) result))
    result))

(defun context-lookup (name)
  (let ((result
	 (find-if #'(lambda (ctx) (eq (context-name ctx) name)) the-contexts)))
    ;; (print (list (list 'context-lookup name) result))
    result))

(defun format-list (x) (if (null x) "()" x))

(defun format-rewrites-list (rws)
  (cond ((null rws) "(list)")
	(t (concat "(list "
		   (mapconcat #'(lambda (rw) (cond ((eq rw 'X) "'X")
						   (t (format "(make-rewrite :rulename '%s :path '%s)"
							      (rewrite-rulename rw)
							      (format-list (rewrite-path rw))))))
			      rws
			      " ")
		   ")"))))

(defun string-expt (str k)
  (cond ((= k 0) "")
	((oddp k) (concat str (string-expt str (- k 1))))
	(t (let ((res (string-expt str (ash k -1))))
	     (concat res res)))))

(defun rewrite-for-tikzcd (rw term)
  (cond ((eq (rewrite-rulename rw) 'TRIVIAL) term)
	(t (apply-rewrite-rule (rule-lookup (rewrite-rulename rw)) term (rewrite-path rw)))))

(defun print-tikzcd-diagram (P Q R rulename1 rulename2 path1 rw1 rw2 k)
;;   (cond ((< (length rw1) (length rw2))
;; 	 (print-least-wide-tikzcd-diagram P Q R rulename1 rulename2 path1 '() rw2 rw1 k))
;; 	(t (print-least-wide-tikzcd-diagram R Q P rulename2 rulename1 '() path1 rw2 rw1 k))))

;; (defun print-least-wide-tikzcd-diagram (P Q R rulename1 rulename2 path1 rw1 rw2 k)
  (princ "\\[\n")
  (princ "\\begin{tikzcd}\n")
  (let ((wd (max 1 (length rw1)))
	(ht (max 1 (length rw2)))
	(trivial (list (make-rewrite :rulename 'TRIVIAL))))
    (princ (format "  {%s} %s {%s} \\\\\n" (format-rule-term Q) (string-expt "&" (* wd 2)) (format-rule-term P)))
    (do ((z (or rw2 trivial) (rest z))
	 (j 0 (+ j 1))
	 (right-term P (rewrite-for-tikzcd (first z) right-term)))
	((null z)
	 (progn (princ "  ")
		(do ((y (or rw1 trivial) (rest y))
		     (bottom-term R (rewrite-for-tikzcd (first y) bottom-term)))
		    ((null y)
		     (unless (equal bottom-term right-term) (error "print-tikzcd-diagram: Rewrites failed to join %s" (list bottom-term right-term)))
		     (princ (format "{%s} \\\\\n" (format-rule-term bottom-term))))
		  (princ (format "{%s} && " (format-rule-term bottom-term))))))
      (when (> j 0)
	(princ (format "  %s %s %s %s \\\\\n"
		       (string-expt "&" wd)
		       (if (= (* j 2)) ht) (format "{(%s)}" k) ""
		       (string-expt "&" wd)
		       (format-rule-term right-term))))	;; XXX or vdots (later)
      (princ (format "  %s %s %s \\\\\n"
		     (string-expt "&" wd)
		     (if (= (+ (* j 2) 1) ht) (format "{(%s)}" k) "")
		     (string-expt "&" wd))))
    (let ((right-edge (+ (* wd 2) 1))
	  (bottom-edge (+ (* ht 2) 1)))
      (princ (format "\\arrow[\"{\\rotatebox{270}{\\hbox{\\rulename{%s}}}}\"', \"{\\rotatebox{0}{\\hbox{$u%s$}}}\", from=1-1, to=%s-1]\n"
		     rulename1 (format-partial-path path1) bottom-edge))
      (princ (format "\\arrow[\"\\rulename{%s}\"', \"{u%s}\", from=1-1, to=1-%s]\n"
		     rulename2 (format-partial-path '()) right-edge))
      (cond ((null rw1)
	     (princ (format "\\arrow[\"{\\equiv}\", dashed, from=%s-%s, to=%s-%s]\n"
			    bottom-edge 1 bottom-edge 3)))
	    (t (do ((z rw1 (rest z))
		    (j 0 (+ j 1)))
		   ((null z))
		 (princ (format "\\arrow[\"\\rulename{%s}\"', \"{u%s}\", dashed, from=%s-%s, to=%s-%s]\n"
				(rewrite-rulename (first z))
				(format-partial-path (rewrite-path (first z)))
				bottom-edge
				(+ (* j 2) 1)
				bottom-edge
				(+ (* j 2) 3))))))
      (cond ((null rw2)
	     (princ (format "\\arrow[\"{\\rotatebox{0}{\\hbox{$\\equiv$}}}\", dashed, from=%s-%s, to=%s-%s]\n"
			    1 right-edge 3 right-edge)))
	    (t (do ((y rw2 (rest y))
		    (j 0 (+ j 1)))
		   ((null y))
		 (princ (format "\\arrow[\"{\\rotatebox{270}{\\hbox{\\rulename{%s}}}}\"', \"{\\rotatebox{0}{\\hbox{$u%s$}}}\", dashed, from=%s-%s, to=%s-%s]\n"
				(rewrite-rulename (first y))
				(format-partial-path (rewrite-path (first y)))
				(+ (* j 2) 1)
				right-edge
				(+ (* j 2) 3)
				right-edge)))))
      ))
  (princ "\\end{tikzcd}\n")
  (princ "\\]\n"))
  
;; \begin{tikzcd}
;; 	t && {t_2} \\
;; 	& {(43)} \\
;; 	{t_1} && {t_f}
;; 	\arrow["top"', from=1-1, to=1-3]
;; 	\arrow["left"', from=1-1, to=3-1]
;; 	\arrow["right"', dashed, from=1-3, to=3-3]
;; 	\arrow["bottom"', dashed, from=3-1, to=3-3]
;; \end{tikzcd}

;; \begin{tikzcd}
;; 	t &&&& {t_2} \\
;; 	&& {(43)} \\
;; 	{t_1} && {t_3} && {t_f}
;; 	\arrow["top"', from=1-1, to=1-5]
;; 	\arrow["left"', from=1-1, to=3-1]
;; 	\arrow["bottom1"', dashed, from=3-1, to=3-3]
;; 	\arrow["right"', dashed, from=1-5, to=3-5]
;; 	\arrow["bottom2"', dashed, from=3-3, to=3-5]
;; \end{tikzcd}

;; \begin{tikzcd}
;; 	t &&&&&& {t_2} \\
;; 	&&& {(43)} \\
;; 	{t_1} && {t_3} && {t_5} && {t_f}
;; 	\arrow["top"', from=1-1, to=1-7]
;; 	\arrow["left"', from=1-1, to=3-1]
;; 	\arrow["bottom1"', dashed, from=3-1, to=3-3]
;; 	\arrow["right"', dashed, from=1-7, to=3-7]
;; 	\arrow["bottom2"', dashed, from=3-3, to=3-5]
;; 	\arrow["bottom3"', dashed, from=3-5, to=3-7]
;; \end{tikzcd}

;; \begin{tikzcd}
;; 	t &&&&&& {t_2} \\
;; 	\\
;; 	&&& {(43)} &&& {t_4} \\
;; 	\\
;; 	{t_1} && {t_3} && {t_5} && {t_f}
;; 	\arrow["top"', from=1-1, to=1-7]
;; 	\arrow["left"', from=1-1, to=5-1]
;; 	\arrow["bottom1"', dashed, from=5-1, to=5-3]
;; 	\arrow["bottom2"', dashed, from=5-3, to=5-5]
;; 	\arrow["bottom3"', dashed, from=5-5, to=5-7]
;; 	\arrow["right1"', dashed, from=1-7, to=3-7]
;; 	\arrow["right2"', dashed, from=3-7, to=5-7]
;; \end{tikzcd}

;; \begin{tikzcd}
;; 	t &&&&&& {t_2} \\
;; 	\\
;; 	&&&&&& {t_4} \\
;; 	&&& {(43)} \\
;; 	&&&&&& {t_6} \\
;; 	\\
;; 	{t_1} && {t_3} && {t_5} && {t_f}
;; 	\arrow["top"', from=1-1, to=1-7]
;; 	\arrow["left"', from=1-1, to=7-1]
;; 	\arrow["bottom1"', dashed, from=7-1, to=7-3]
;; 	\arrow["bottom2"', dashed, from=7-3, to=7-5]
;; 	\arrow["bottom3"', dashed, from=7-5, to=7-7]
;; 	\arrow["right1"', dashed, from=1-7, to=3-7]
;; 	\arrow["right2"', dashed, from=3-7, to=5-7]
;; 	\arrow["right3"', dashed, from=5-7, to=7-7]
;; \end{tikzcd}

;; \begin{tikzcd}
;; 	t &&&& {t_2} \\
;; 	\\
;; 	&&&& {t_4} \\
;; 	&& {(43)} \\
;; 	&&&& {t_6} \\
;; 	\\
;; 	{t_1} && {t_3} && {t_f}
;; 	\arrow["top"', from=1-1, to=1-5]
;; 	\arrow["left"', from=1-1, to=7-1]
;; 	\arrow["bottom1"', dashed, from=7-1, to=7-3]
;; 	\arrow["right1"', dashed, from=1-5, to=3-5]
;; 	\arrow["right2"', dashed, from=3-5, to=5-5]
;; 	\arrow["right3"', dashed, from=5-5, to=7-5]
;; 	\arrow["bottom2"', dashed, from=7-3, to=7-5]
;; \end{tikzcd}

(defun print-proof-skeletons (the-previous-proofs)
  (let ((cps (all-critical-pairs)))
    (princ (format "(setq the-proofs                ;%s proofs\n" (length cps)))
    (let ((prefix1 "      (list ")
	  (prefix2 "            "))
      (do ((z cps (rest z))
	   (k 1 (+ k 1))
	   (prefix prefix1 prefix2))
	  ((null z))
	(let* ((cp (first z))
	       (rule1 (critpair-rule1 cp))
	       (rule2 (critpair-rule2 cp))
	       (name1 (rule-name rule1))
	       (name2 (rule-name rule2))
	       (path1 (critpair-path1 cp))
	       (pf (proof-lookup name1 name2 path1)))
	  (princ (format "%s(make-proof :rulename1 '%s :rulename2 '%s :path1 '%s      ;Proof %s"
			 prefix name1 name2 (if (null path1) "()" path1) k))
	  (cond ((null pf)
		 (princ (format "\n%s            :rewrites1 (list 'X (make-rewrite :rulename '%s :path '()))"
				prefix2 name2))
		 (princ (format "\n%s            :rewrites2 (list 'X (make-rewrite :rulename '%s :path '()))"
				prefix2 name1)))
		(t (princ (format "\n%s            :rewrites1 %s" prefix2 (format-rewrites-list (proof-rewrites1 pf))))
		   (princ (format "\n%s            :rewrites2 %s" prefix2 (format-rewrites-list (proof-rewrites2 pf))))
		   (when (or (proof-altrewrites1 pf) (proof-altrewrites2 pf))
		     (princ (format "\n%s            :altrewrites1 %s" prefix2 (format-rewrites-list (proof-altrewrites1 pf))))
		     (princ (format "\n%s            :altrewrites2 %s" prefix2 (format-rewrites-list (proof-altrewrites2 pf)))))))
	  (princ (format ")%s" (if (rest z) "\n" ""))))))
    (princ "))\n"))
  'done)

(setq eval-expression-print-level (setq eval-expression-print-length nil))
(setq inhibit-debugger nil)



(let ((cp (all-critical-pairs))) (cons (length cp) cp))

(print-proof-skeletons)




(let ((cp (all-critical-pairs)))
  (princ "\nS--------------\n")
  (princ (format "The rules for \\versecalc{} have %s critical pairs, which are described here in detail.\\par\n" (length cp)))
  (do ((z cp (rest z))
       (k 1 (+ k 1)))
      ((null z))
    (print-critical-pair (first z) k))
  (princ "\nE--------------\n")
  'done)


S--------------
The rules for \versecalc{} have 66 critical pairs, which are described here in detail.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(1)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{u-lit}\hfill}\hbox to 8em{\hss |k1 = k2; e|}\quad$\movesto$\quad |e|\hfill \text{if $|k1|=|k2|$}\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-swap}\hfill}\hbox to 8em{\hss |eq; (x = v; e')|}\quad$\movesto$\quad |x = v; (eq; e')|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(k1 = k2; (x = v; e'))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |x = v; e'|\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |k1 = k2|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(x = v; e')| \xrnungosup{u-lit}{\emptypath} |t| \xrngosup{seq-swap}{\emptypath} |(x = v; (k1 = k2; e'))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{They can be joined: $|t_1| \equiv  \bigl(|x = v; e'|\bigr) \xrnungosup{u-lit}{\2}|(x = v; (k1 = k2; e'))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break}
\[
\begin{tikzcd}
  {|k1 = k2; (x = v; e')|} && {|x = v; (k1 = k2; e')|} \\
  & {(1)} & \\
  {|x = v; e'|} && {|x = v; e'|} \\
\arrow["{\rotatebox{270}{\hbox{\rulename{u-lit}}}}"', "{\rotatebox{0}{\hbox{$u$}}}", from=1-1, to=3-1]
\arrow["\rulename{seq-swap}"', "{u}", from=1-1, to=1-3]
\arrow["{\equiv}", dashed, from=3-1, to=3-3]
\arrow["{\rotatebox{270}{\hbox{\rulename{u-lit}}}}"', "{\rotatebox{0}{\hbox{$u\2$}}}", dashed, from=1-3, to=3-3]
\end{tikzcd}
\]
Therefore rules \rulename{u-lit} and \rulename{seq-swap} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(2)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{u-lit}\hfill}\hbox to 8em{\hss |k1 = k2; e|}\quad$\movesto$\quad |e|\hfill \text{if $|k1|=|k2|$}\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{exi-float-r}\hfill}\hbox to 8em{\hss |eq; (def x (e'))|}\quad$\movesto$\quad |def x ((eq; e'))|\hfill \text{if $|x|\not\in \freevars{|eq|}$}\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(k1 = k2; (def x (e')))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |def x (e')|\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |k1 = k2|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(def x (e'))| \xrnungosup{u-lit}{\emptypath} |t| \xrngosup{exi-float-r}{\emptypath} |(def x ((k1 = k2; e')))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{They can be joined: $|t_1| \equiv  \bigl(|def x (e')|\bigr) \xrnungosup{u-lit}{\2}|(def x ((k1 = k2; e')))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break}
\[
\begin{tikzcd}
  {|k1 = k2; (def x (e'))|} && {|def x ((k1 = k2; e'))|} \\
  & {(2)} & \\
  {|def x (e')|} && {|def x (e')|} \\
\arrow["{\rotatebox{270}{\hbox{\rulename{u-lit}}}}"', "{\rotatebox{0}{\hbox{$u$}}}", from=1-1, to=3-1]
\arrow["\rulename{exi-float-r}"', "{u}", from=1-1, to=1-3]
\arrow["{\equiv}", dashed, from=3-1, to=3-3]
\arrow["{\rotatebox{270}{\hbox{\rulename{u-lit}}}}"', "{\rotatebox{0}{\hbox{$u\2$}}}", dashed, from=1-3, to=3-3]
\end{tikzcd}
\]
Therefore rules \rulename{u-lit} and \rulename{exi-float-r} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(3)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{u-lit}\hfill}\hbox to 8em{\hss |k1 = k2; e|}\quad$\movesto$\quad |e|\hfill \text{if $|k1|=|k2|$}\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq; e1); e2|}\quad$\movesto$\quad |eq; (e1; e2)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((k1 = k2; e); e2)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |k1 = k2|,\,|e1|\mapsto |e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(e; e2)| \xrnungosup{u-lit}{\1} |t| \xrngosup{seq-assoc}{\emptypath} |(k1 = k2; (e; e2))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{They can be joined: $|t_1| \equiv  \bigl(|e; e2|\bigr) \xrnungosup{u-lit}{\emptypath}|(k1 = k2; (e; e2))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break}
\[
\begin{tikzcd}
  {|(k1 = k2; e); e2|} && {|k1 = k2; (e; e2)|} \\
  & {(3)} & \\
  {|e; e2|} && {|e; e2|} \\
\arrow["{\rotatebox{270}{\hbox{\rulename{u-lit}}}}"', "{\rotatebox{0}{\hbox{$u\1$}}}", from=1-1, to=3-1]
\arrow["\rulename{seq-assoc}"', "{u}", from=1-1, to=1-3]
\arrow["{\equiv}", dashed, from=3-1, to=3-3]
\arrow["{\rotatebox{270}{\hbox{\rulename{u-lit}}}}"', "{\rotatebox{0}{\hbox{$u$}}}", dashed, from=1-3, to=3-3]
\end{tikzcd}
\]
Therefore rules \rulename{u-lit} and \rulename{seq-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(4)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{u-tup}\hfill}\hbox to 8em{\hss |(tup (v1,xdots,vn)) = (tup (v1',xdots,vn')); e|}\quad$\movesto$\quad |v1 = v1'; xdots vn = vn'; e|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-swap}\hfill}\hbox to 8em{\hss |eq; (x = v; e')|}\quad$\movesto$\quad |x = v; (eq; e')|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((tup (v1,xdots,vn)) = (tup (v1',xdots,vn')); (x = v; e'))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |x = v; e'|\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |(tup (v1,xdots,vn)) = (tup (v1',xdots,vn'))|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(v1 = v1'; xdots vn = vn'; (x = v; e'))| \xrnungosup{u-tup}{\emptypath} |t| \xrngosup{seq-swap}{\emptypath} |(x = v; ((tup (v1,xdots,vn)) = (tup (v1',xdots,vn')); e'))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{They can be joined: $|t_1| \equiv |(v1 = v1'; xdots vn = vn'; (x = v; e'))|\xrngosup{seq-swap}{\2^{n-1}}\mydots\xrngosup{seq-swap}{\2^{0}} {}$\vadjust{\penalty1000}\hfil\break\null\hskip 2em minus 1.95em$\bigl(|x = v; (v1 = v1'; xdots vn = vn'; e')|\bigr) \xrnungosup{u-tup}{\2}|(x = v; ((tup (v1,xdots,vn)) = (tup (v1',xdots,vn')); e'))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break}
\[
\begin{tikzcd}
  {|(tup (v1,xdots,vn)) = (tup (v1',xdots,vn')); (x = v; e')|} &&&& {|x = v; ((tup (v1,xdots,vn)) = (tup (v1',xdots,vn')); e')|} \\
  && {(4)} && \\
  {|v1 = v1'; xdots vn = vn'; (x = v; e')|} && {|v1 = v1'; (x = v; xdots vn = vn'; e')|} && {|x = v; (v1 = v1'; xdots vn = vn'; e')|} \\
\arrow["{\rotatebox{270}{\hbox{\rulename{u-tup}}}}"', "{\rotatebox{0}{\hbox{$u$}}}", from=1-1, to=3-1]
\arrow["\rulename{seq-swap}"', "{u}", from=1-1, to=1-5]
\arrow["\rulename{seq-swap}"', "{u\2^{n-1}}", dashed, from=3-1, to=3-3]
\arrow["\rulename{seq-swap}"', "{u\2^{0}}", dashed, from=3-3, to=3-5]
\arrow["{\rotatebox{270}{\hbox{\rulename{u-tup}}}}"', "{\rotatebox{0}{\hbox{$u\2$}}}", dashed, from=1-5, to=3-5]
\end{tikzcd}
\]
Therefore rules \rulename{u-tup} and \rulename{seq-swap} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(5)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{u-tup}\hfill}\hbox to 8em{\hss |(tup (v1,xdots,vn)) = (tup (v1',xdots,vn')); e|}\quad$\movesto$\quad |v1 = v1'; xdots vn = vn'; e|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{exi-float-r}\hfill}\hbox to 8em{\hss |eq; (def x (e'))|}\quad$\movesto$\quad |def x ((eq; e'))|\hfill \text{if $|x|\not\in \freevars{|eq|}$}\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((tup (v1,xdots,vn)) = (tup (v1',xdots,vn')); (def x (e')))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |def x (e')|\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |(tup (v1,xdots,vn)) = (tup (v1',xdots,vn'))|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(v1 = v1'; xdots vn = vn'; (def x (e')))| \xrnungosup{u-tup}{\emptypath} |t| \xrngosup{exi-float-r}{\emptypath} |(def x (((tup (v1,xdots,vn)) = (tup (v1',xdots,vn')); e')))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{They can be joined: $|t_1| \equiv |(v1 = v1'; xdots vn = vn'; (def x (e')))|\xrngosup{exi-float-r}{\2^{n-1}}\mydots\xrngosup{exi-float-r}{\2^{0}} {}$\vadjust{\penalty1000}\hfil\break\null\hskip 2em minus 1.95em$\bigl(|def x ((v1 = v1'; xdots vn = vn'; e'))|\bigr) \xrnungosup{u-tup}{\2}|(def x (((tup (v1,xdots,vn)) = (tup (v1',xdots,vn')); e')))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break}
\[
\begin{tikzcd}
  {|(tup (v1,xdots,vn)) = (tup (v1',xdots,vn')); (def x (e'))|} &&&& {|def x (((tup (v1,xdots,vn)) = (tup (v1',xdots,vn')); e'))|} \\
  && {(5)} && \\
  {|v1 = v1'; xdots vn = vn'; (def x (e'))|} && {|v1 = v1'; (def x ((xdots vn = vn'; e')))|} && {|def x ((v1 = v1'; xdots vn = vn'; e'))|} \\
\arrow["{\rotatebox{270}{\hbox{\rulename{u-tup}}}}"', "{\rotatebox{0}{\hbox{$u$}}}", from=1-1, to=3-1]
\arrow["\rulename{exi-float-r}"', "{u}", from=1-1, to=1-5]
\arrow["\rulename{exi-float-r}"', "{u\2^{n-1}}", dashed, from=3-1, to=3-3]
\arrow["\rulename{exi-float-r}"', "{u\2^{0}}", dashed, from=3-3, to=3-5]
\arrow["{\rotatebox{270}{\hbox{\rulename{u-tup}}}}"', "{\rotatebox{0}{\hbox{$u\2$}}}", dashed, from=1-5, to=3-5]
\end{tikzcd}
\]
Therefore rules \rulename{u-tup} and \rulename{exi-float-r} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(6)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{u-tup}\hfill}\hbox to 8em{\hss |(tup (v1,xdots,vn)) = (tup (v1',xdots,vn')); e|}\quad$\movesto$\quad |v1 = v1'; xdots vn = vn'; e|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq; e1); e2|}\quad$\movesto$\quad |eq; (e1; e2)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(((tup (v1,xdots,vn)) = (tup (v1',xdots,vn')); e); e2)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |(tup (v1,xdots,vn)) = (tup (v1',xdots,vn'))|,\,|e1|\mapsto |e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |((v1 = v1'; xdots vn = vn'; e); e2)| \xrnungosup{u-tup}{\1} |t| \xrngosup{seq-assoc}{\emptypath} |((tup (v1,xdots,vn)) = (tup (v1',xdots,vn')); (e; e2))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{u-tup}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{u-tup} and \rulename{seq-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(7)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{u-fail-op-d}\hfill}\hbox to 8em{\hss |op = d; e|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{u-fail-d-op}\hfill}\hbox to 8em{\hss |d' = op'; e'|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(op = d; e)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|d'|\mapsto |op|,\,|op'|\mapsto |d|,\,|e'|\mapsto |e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |fail| \xrnungosup{u-fail-op-d}{\emptypath} |t| \xrngosup{u-fail-d-op}{\emptypath} |fail| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{They can be joined: $|t_1| \equiv  \bigl(|fail|\bigr)  \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break}
\[
\begin{tikzcd}
  {|op = d; e|} && {|fail|} \\
  & {(7)} & \\
  {|fail|} && {|fail|} \\
\arrow["{\rotatebox{270}{\hbox{\rulename{u-fail-op-d}}}}"', "{\rotatebox{0}{\hbox{$u$}}}", from=1-1, to=3-1]
\arrow["\rulename{u-fail-d-op}"', "{u}", from=1-1, to=1-3]
\arrow["{\equiv}", dashed, from=3-1, to=3-3]
\arrow["{\rotatebox{0}{\hbox{$\equiv$}}}", dashed, from=1-3, to=3-3]
\end{tikzcd}
\]
Therefore rules \rulename{u-fail-op-d} and \rulename{u-fail-d-op} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(8)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{u-fail-op-d}\hfill}\hbox to 8em{\hss |op = d; e|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-swap}\hfill}\hbox to 8em{\hss |eq; (x = v; e')|}\quad$\movesto$\quad |x = v; (eq; e')|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(op = d; (x = v; e'))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |x = v; e'|\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |op = d|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |fail| \xrnungosup{u-fail-op-d}{\emptypath} |t| \xrngosup{seq-swap}{\emptypath} |(x = v; (op = d; e'))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-swap}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{u-fail-op-d}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{u-fail-op-d} and \rulename{seq-swap} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(9)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{u-fail-op-d}\hfill}\hbox to 8em{\hss |op = d; e|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{exi-float-r}\hfill}\hbox to 8em{\hss |eq; (def x (e'))|}\quad$\movesto$\quad |def x ((eq; e'))|\hfill \text{if $|x|\not\in \freevars{|eq|}$}\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(op = d; (def x (e')))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |def x (e')|\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |op = d|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |fail| \xrnungosup{u-fail-op-d}{\emptypath} |t| \xrngosup{exi-float-r}{\emptypath} |(def x ((op = d; e')))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{exi-float-r}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{u-fail-op-d}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{u-fail-op-d} and \rulename{exi-float-r} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(10)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{u-fail-op-d}\hfill}\hbox to 8em{\hss |op = d; e|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq; e1); e2|}\quad$\movesto$\quad |eq; (e1; e2)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((op = d; e); e2)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |op = d|,\,|e1|\mapsto |e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(fail; e2)| \xrnungosup{u-fail-op-d}{\1} |t| \xrngosup{seq-assoc}{\emptypath} |(op = d; (e; e2))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{u-fail-op-d}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{u-fail-op-d} and \rulename{seq-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(11)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{u-fail-d-op}\hfill}\hbox to 8em{\hss |d = op; e|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-swap}\hfill}\hbox to 8em{\hss |eq; (x = v; e')|}\quad$\movesto$\quad |x = v; (eq; e')|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(d = op; (x = v; e'))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |x = v; e'|\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |d = op|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |fail| \xrnungosup{u-fail-d-op}{\emptypath} |t| \xrngosup{seq-swap}{\emptypath} |(x = v; (d = op; e'))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-swap}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{u-fail-d-op}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{u-fail-d-op} and \rulename{seq-swap} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(12)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{u-fail-d-op}\hfill}\hbox to 8em{\hss |d = op; e|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{exi-float-r}\hfill}\hbox to 8em{\hss |eq; (def x (e'))|}\quad$\movesto$\quad |def x ((eq; e'))|\hfill \text{if $|x|\not\in \freevars{|eq|}$}\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(d = op; (def x (e')))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |def x (e')|\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |d = op|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |fail| \xrnungosup{u-fail-d-op}{\emptypath} |t| \xrngosup{exi-float-r}{\emptypath} |(def x ((d = op; e')))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{exi-float-r}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{u-fail-d-op}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{u-fail-d-op} and \rulename{exi-float-r} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(13)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{u-fail-d-op}\hfill}\hbox to 8em{\hss |d = op; e|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq; e1); e2|}\quad$\movesto$\quad |eq; (e1; e2)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((d = op; e); e2)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |d = op|,\,|e1|\mapsto |e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(fail; e2)| \xrnungosup{u-fail-d-op}{\1} |t| \xrngosup{seq-assoc}{\emptypath} |(d = op; (e; e2))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{u-fail-d-op}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{u-fail-d-op} and \rulename{seq-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(14)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{u-fail-tup-k}\hfill}\hbox to 8em{\hss |(tup (v1,xdots,vn)) = k; e|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-swap}\hfill}\hbox to 8em{\hss |eq; (x = v; e')|}\quad$\movesto$\quad |x = v; (eq; e')|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((tup (v1,xdots,vn)) = k; (x = v; e'))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |x = v; e'|\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |(tup (v1,xdots,vn)) = k|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |fail| \xrnungosup{u-fail-tup-k}{\emptypath} |t| \xrngosup{seq-swap}{\emptypath} |(x = v; ((tup (v1,xdots,vn)) = k; e'))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-swap}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{u-fail-tup-k}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{u-fail-tup-k} and \rulename{seq-swap} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(15)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{u-fail-tup-k}\hfill}\hbox to 8em{\hss |(tup (v1,xdots,vn)) = k; e|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{exi-float-r}\hfill}\hbox to 8em{\hss |eq; (def x (e'))|}\quad$\movesto$\quad |def x ((eq; e'))|\hfill \text{if $|x|\not\in \freevars{|eq|}$}\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((tup (v1,xdots,vn)) = k; (def x (e')))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |def x (e')|\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |(tup (v1,xdots,vn)) = k|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |fail| \xrnungosup{u-fail-tup-k}{\emptypath} |t| \xrngosup{exi-float-r}{\emptypath} |(def x (((tup (v1,xdots,vn)) = k; e')))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{exi-float-r}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{u-fail-tup-k}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{u-fail-tup-k} and \rulename{exi-float-r} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(16)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{u-fail-tup-k}\hfill}\hbox to 8em{\hss |(tup (v1,xdots,vn)) = k; e|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq; e1); e2|}\quad$\movesto$\quad |eq; (e1; e2)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(((tup (v1,xdots,vn)) = k; e); e2)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |(tup (v1,xdots,vn)) = k|,\,|e1|\mapsto |e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(fail; e2)| \xrnungosup{u-fail-tup-k}{\1} |t| \xrngosup{seq-assoc}{\emptypath} |((tup (v1,xdots,vn)) = k; (e; e2))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{u-fail-tup-k}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{u-fail-tup-k} and \rulename{seq-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(17)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{u-fail-k-tup}\hfill}\hbox to 8em{\hss |k = (tup (v1,xdots,vn)); e|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-swap}\hfill}\hbox to 8em{\hss |eq; (x = v; e')|}\quad$\movesto$\quad |x = v; (eq; e')|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(k = (tup (v1,xdots,vn)); (x = v; e'))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |x = v; e'|\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |k = (tup (v1,xdots,vn))|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |fail| \xrnungosup{u-fail-k-tup}{\emptypath} |t| \xrngosup{seq-swap}{\emptypath} |(x = v; (k = (tup (v1,xdots,vn)); e'))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-swap}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{u-fail-k-tup}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{u-fail-k-tup} and \rulename{seq-swap} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(18)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{u-fail-k-tup}\hfill}\hbox to 8em{\hss |k = (tup (v1,xdots,vn)); e|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{exi-float-r}\hfill}\hbox to 8em{\hss |eq; (def x (e'))|}\quad$\movesto$\quad |def x ((eq; e'))|\hfill \text{if $|x|\not\in \freevars{|eq|}$}\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(k = (tup (v1,xdots,vn)); (def x (e')))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |def x (e')|\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |k = (tup (v1,xdots,vn))|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |fail| \xrnungosup{u-fail-k-tup}{\emptypath} |t| \xrngosup{exi-float-r}{\emptypath} |(def x ((k = (tup (v1,xdots,vn)); e')))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{exi-float-r}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{u-fail-k-tup}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{u-fail-k-tup} and \rulename{exi-float-r} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(19)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{u-fail-k-tup}\hfill}\hbox to 8em{\hss |k = (tup (v1,xdots,vn)); e|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq; e1); e2|}\quad$\movesto$\quad |eq; (e1; e2)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((k = (tup (v1,xdots,vn)); e); e2)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |k = (tup (v1,xdots,vn))|,\,|e1|\mapsto |e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(fail; e2)| \xrnungosup{u-fail-k-tup}{\1} |t| \xrngosup{seq-assoc}{\emptypath} |(k = (tup (v1,xdots,vn)); (e; e2))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{u-fail-k-tup}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{u-fail-k-tup} and \rulename{seq-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(20)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{hnf-swap}\hfill}\hbox to 8em{\hss |hnf = x; e|}\quad$\movesto$\quad |x = hnf; e|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-swap}\hfill}\hbox to 8em{\hss |eq; (x' = v; e')|}\quad$\movesto$\quad |x' = v; (eq; e')|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(hnf = x; (x' = v; e'))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |x' = v; e'|\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |hnf = x|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(x = hnf; (x' = v; e'))| \xrnungosup{hnf-swap}{\emptypath} |t| \xrngosup{seq-swap}{\emptypath} |(x' = v; (hnf = x; e'))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-swap}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{hnf-swap}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{hnf-swap} and \rulename{seq-swap} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(21)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{hnf-swap}\hfill}\hbox to 8em{\hss |hnf = x; e|}\quad$\movesto$\quad |x = hnf; e|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{exi-float-r}\hfill}\hbox to 8em{\hss |eq; (def x' (e'))|}\quad$\movesto$\quad |def x' ((eq; e'))|\hfill \text{if $|x'|\not\in \freevars{|eq|}$}\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(hnf = x; (def x' (e')))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |def x' (e')|\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |hnf = x|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(x = hnf; (def x' (e')))| \xrnungosup{hnf-swap}{\emptypath} |t| \xrngosup{exi-float-r}{\emptypath} |(def x' ((hnf = x; e')))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{exi-float-r}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{hnf-swap}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{hnf-swap} and \rulename{exi-float-r} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(22)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{hnf-swap}\hfill}\hbox to 8em{\hss |hnf = x; e|}\quad$\movesto$\quad |x = hnf; e|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq; e1); e2|}\quad$\movesto$\quad |eq; (e1; e2)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((hnf = x; e); e2)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |hnf = x|,\,|e1|\mapsto |e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |((x = hnf; e); e2)| \xrnungosup{hnf-swap}{\1} |t| \xrngosup{seq-assoc}{\emptypath} |(hnf = x; (e; e2))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{hnf-swap}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{hnf-swap} and \rulename{seq-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(23)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{var-swap}\hfill}\hbox to 8em{\hss |x1 = x2; e|}\quad$\movesto$\quad |x2 = x1; e|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-swap}\hfill}\hbox to 8em{\hss |eq; (x = v; e')|}\quad$\movesto$\quad |x = v; (eq; e')|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(x1 = x2; (x = v; e'))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |x = v; e'|\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |x1 = x2|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(x2 = x1; (x = v; e'))| \xrnungosup{var-swap}{\emptypath} |t| \xrngosup{seq-swap}{\emptypath} |(x = v; (x1 = x2; e'))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-swap}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{var-swap}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{var-swap} and \rulename{seq-swap} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(24)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{var-swap}\hfill}\hbox to 8em{\hss |x1 = x2; e|}\quad$\movesto$\quad |x2 = x1; e|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-swap}\hfill}\hbox to 8em{\hss |eq; (x = v; e')|}\quad$\movesto$\quad |x = v; (eq; e')|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(eq; (x1 = x2; e))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|x|\mapsto |x1|,\,|v|\mapsto |x2|,\,|e'|\mapsto |e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(eq; (x2 = x1; e))| \xrnungosup{var-swap}{\2} |t| \xrngosup{seq-swap}{\emptypath} |(x1 = x2; (eq; e))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-swap}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{var-swap}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{var-swap} and \rulename{seq-swap} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(25)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{var-swap}\hfill}\hbox to 8em{\hss |x1 = x2; e|}\quad$\movesto$\quad |x2 = x1; e|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{exi-float-r}\hfill}\hbox to 8em{\hss |eq; (def x (e'))|}\quad$\movesto$\quad |def x ((eq; e'))|\hfill \text{if $|x|\not\in \freevars{|eq|}$}\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(x1 = x2; (def x (e')))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |def x (e')|\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |x1 = x2|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(x2 = x1; (def x (e')))| \xrnungosup{var-swap}{\emptypath} |t| \xrngosup{exi-float-r}{\emptypath} |(def x ((x1 = x2; e')))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{exi-float-r}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{var-swap}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{var-swap} and \rulename{exi-float-r} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(26)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{var-swap}\hfill}\hbox to 8em{\hss |x1 = x2; e|}\quad$\movesto$\quad |x2 = x1; e|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq; e1); e2|}\quad$\movesto$\quad |eq; (e1; e2)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((x1 = x2; e); e2)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |x1 = x2|,\,|e1|\mapsto |e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |((x2 = x1; e); e2)| \xrnungosup{var-swap}{\1} |t| \xrngosup{seq-assoc}{\emptypath} |(x1 = x2; (e; e2))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{var-swap}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{var-swap} and \rulename{seq-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(27)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{seq-swap}\hfill}\hbox to 8em{\hss |eq; (x = v; e)|}\quad$\movesto$\quad |x = v; (eq; e)|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-swap}\hfill}\hbox to 8em{\hss |eq'; (x' = v'; e')|}\quad$\movesto$\quad |x' = v'; (eq'; e')|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(eq'; (x' = v'; (x = v; e)))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|eq|\mapsto |x' = v'|\,\}$} and {\color{blue}$\sigma_2=\{\,|e'|\mapsto |x = v; e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(eq'; (x = v; (x' = v'; e)))| \xrnungosup{seq-swap}{\2} |t| \xrngosup{seq-swap}{\emptypath} |(x' = v'; (eq'; (x = v; e)))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-swap}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{seq-swap}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{seq-swap} and \rulename{seq-swap} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(28)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{seq-swap}\hfill}\hbox to 8em{\hss |eq; (x = v; e)|}\quad$\movesto$\quad |x = v; (eq; e)|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{val-elim}\hfill}\hbox to 8em{\hss |v'; e'|}\quad$\movesto$\quad |e'|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(eq; (x = v; e))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|v'|\mapsto |eq|,\,|e'|\mapsto |x = v; e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(x = v; (eq; e))| \xrnungosup{seq-swap}{\emptypath} |t| \xrngosup{val-elim}{\emptypath} |(x = v; e)| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{val-elim}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{seq-swap}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{seq-swap} and \rulename{val-elim} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(29)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{seq-swap}\hfill}\hbox to 8em{\hss |eq; (x = v; e)|}\quad$\movesto$\quad |x = v; (eq; e)|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{fail-elim-eq}\hfill}\hbox to 8em{\hss |v' = fail; e'|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(v' = fail; (x = v; e))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|eq|\mapsto |v' = fail|\,\}$} and {\color{blue}$\sigma_2=\{\,|e'|\mapsto |x = v; e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(x = v; (v' = fail; e))| \xrnungosup{seq-swap}{\emptypath} |t| \xrngosup{fail-elim-eq}{\emptypath} |fail| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{fail-elim-eq}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{seq-swap}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{seq-swap} and \rulename{fail-elim-eq} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(30)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{seq-swap}\hfill}\hbox to 8em{\hss |eq; (x = v; e)|}\quad$\movesto$\quad |x = v; (eq; e)|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{fail-elim-l}\hfill}\hbox to 8em{\hss |fail; e'|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(fail; (x = v; e))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|eq|\mapsto |fail|\,\}$} and {\color{blue}$\sigma_2=\{\,|e'|\mapsto |x = v; e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(x = v; (fail; e))| \xrnungosup{seq-swap}{\emptypath} |t| \xrngosup{fail-elim-l}{\emptypath} |fail| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{fail-elim-l}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{seq-swap}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{seq-swap} and \rulename{fail-elim-l} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(31)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{seq-swap}\hfill}\hbox to 8em{\hss |eq; (x = v; e)|}\quad$\movesto$\quad |x = v; (eq; e)|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{exi-float-eq}\hfill}\hbox to 8em{\hss |v' = (def x' (e1)); e2|}\quad$\movesto$\quad |def x' ((v' = e1; e2))|\hfill \text{if $|x'|\not\in \freevars{|v'|,|e2|}$}\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(v' = (def x' (e1)); (x = v; e))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|eq|\mapsto |v' = (def x' (e1))|\,\}$} and {\color{blue}$\sigma_2=\{\,|e2|\mapsto |x = v; e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(x = v; (v' = (def x' (e1)); e))| \xrnungosup{seq-swap}{\emptypath} |t| \xrngosup{exi-float-eq}{\emptypath} |(def x' ((v' = e1; (x = v; e))))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{exi-float-eq}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{seq-swap}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{seq-swap} and \rulename{exi-float-eq} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(32)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{seq-swap}\hfill}\hbox to 8em{\hss |eq; (x = v; e)|}\quad$\movesto$\quad |x = v; (eq; e)|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{exi-float-l}\hfill}\hbox to 8em{\hss |(def x' (e1)); e2|}\quad$\movesto$\quad |def x' ((e1; e2))|\hfill \text{if $|x'|\not\in \freevars{|e2|}$}\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((def x' (e1)); (x = v; e))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|eq|\mapsto |def x' (e1)|\,\}$} and {\color{blue}$\sigma_2=\{\,|e2|\mapsto |x = v; e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(x = v; ((def x' (e1)); e))| \xrnungosup{seq-swap}{\emptypath} |t| \xrngosup{exi-float-l}{\emptypath} |(def x' ((e1; (x = v; e))))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{exi-float-l}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{seq-swap}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{seq-swap} and \rulename{exi-float-l} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(33)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{exi-float-r}\hfill}\hbox to 8em{\hss |eq; (def x (e))|}\quad$\movesto$\quad |def x ((eq; e))|\hfill \text{if $|x|\not\in \freevars{|eq|}$}\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-swap}\hfill}\hbox to 8em{\hss |eq'; (x' = v; e')|}\quad$\movesto$\quad |x' = v; (eq'; e')|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(eq'; (x' = v; (def x (e))))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|eq|\mapsto |x' = v|\,\}$} and {\color{blue}$\sigma_2=\{\,|e'|\mapsto |def x (e)|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(eq'; (def x ((x' = v; e))))| \xrnungosup{exi-float-r}{\2} |t| \xrngosup{seq-swap}{\emptypath} |(x' = v; (eq'; (def x (e))))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-swap}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{exi-float-r}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{exi-float-r} and \rulename{seq-swap} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(34)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{seq-swap}\hfill}\hbox to 8em{\hss |eq; (x = v; e)|}\quad$\movesto$\quad |x = v; (eq; e)|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{eqn-float}\hfill}\hbox to 8em{\hss |x' = (eq'; e1); e2|}\quad$\movesto$\quad |eq'; (v1 = v2; e2)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(x' = (eq'; e1); (x = v; e))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|eq|\mapsto |x' = (eq'; e1)|\,\}$} and {\color{blue}$\sigma_2=\{\,|e2|\mapsto |x = v; e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(x = v; (x' = (eq'; e1); e))| \xrnungosup{seq-swap}{\emptypath} |t| \xrngosup{eqn-float}{\emptypath} |(eq'; (v1 = v2; (x = v; e)))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{eqn-float}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{seq-swap}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{seq-swap} and \rulename{eqn-float} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(35)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{seq-swap}\hfill}\hbox to 8em{\hss |eq; (x = v; e)|}\quad$\movesto$\quad |x = v; (eq; e)|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq'; e1); e2|}\quad$\movesto$\quad |eq'; (e1; e2)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((eq'; e1); (x = v; e))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|eq|\mapsto |eq'; e1|\,\}$} and {\color{blue}$\sigma_2=\{\,|e2|\mapsto |x = v; e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(x = v; ((eq'; e1); e))| \xrnungosup{seq-swap}{\emptypath} |t| \xrngosup{seq-assoc}{\emptypath} |(eq'; (e1; (x = v; e)))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{seq-swap}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{seq-swap} and \rulename{seq-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(36)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{seq-swap}\hfill}\hbox to 8em{\hss |eq; (x = v; e)|}\quad$\movesto$\quad |x = v; (eq; e)|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq'; e1); e2|}\quad$\movesto$\quad |eq'; (e1; e2)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((eq; (x = v; e)); e2)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|eq'|\mapsto |eq|,\,|e1|\mapsto |x = v; e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |((x = v; (eq; e)); e2)| \xrnungosup{seq-swap}{\1} |t| \xrngosup{seq-assoc}{\emptypath} |(eq; ((x = v; e); e2))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{seq-swap}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{seq-swap} and \rulename{seq-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(37)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{val-elim}\hfill}\hbox to 8em{\hss |v; e|}\quad$\movesto$\quad |e|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{fail-elim-r}\hfill}\hbox to 8em{\hss |e'; fail|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(v; fail)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |fail|\,\}$} and {\color{blue}$\sigma_2=\{\,|e'|\mapsto |v|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |fail| \xrnungosup{val-elim}{\emptypath} |t| \xrngosup{fail-elim-r}{\emptypath} |fail| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{fail-elim-r}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{val-elim}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{val-elim} and \rulename{fail-elim-r} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(38)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{val-elim}\hfill}\hbox to 8em{\hss |v; e|}\quad$\movesto$\quad |e|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{exi-float-r}\hfill}\hbox to 8em{\hss |eq; (def x (e'))|}\quad$\movesto$\quad |def x ((eq; e'))|\hfill \text{if $|x|\not\in \freevars{|eq|}$}\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(v; (def x (e')))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |def x (e')|\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |v|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(def x (e'))| \xrnungosup{val-elim}{\emptypath} |t| \xrngosup{exi-float-r}{\emptypath} |(def x ((v; e')))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{exi-float-r}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{val-elim}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{val-elim} and \rulename{exi-float-r} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(39)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{val-elim}\hfill}\hbox to 8em{\hss |v; e|}\quad$\movesto$\quad |e|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq; e1); e2|}\quad$\movesto$\quad |eq; (e1; e2)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((v; e); e2)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |v|,\,|e1|\mapsto |e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(e; e2)| \xrnungosup{val-elim}{\1} |t| \xrngosup{seq-assoc}{\emptypath} |(v; (e; e2))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{val-elim}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{val-elim} and \rulename{seq-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(40)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{exi-elim}\hfill}\hbox to 8em{\hss |def x (e)|}\quad$\movesto$\quad |e|\hfill \text{if $|x|\not\in \freevars{|e|}$}\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{eqn-elim}\hfill}\hbox to 8em{\hss |def x' ((x' = v; e'))|}\quad$\movesto$\quad |e'|\hfill \text{if $|x'|\not\in \freevars{|v|,|e'|}$}\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(def x ((x' = v; e')))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |x' = v; e'|\,\}$} and {\color{blue}$\sigma_2=\{\,|x'|\mapsto |x|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(x' = v; e')| \xrnungosup{exi-elim}{\emptypath} |t| \xrngosup{eqn-elim}{\emptypath} |e'| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{eqn-elim}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{exi-elim}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{exi-elim} and \rulename{eqn-elim} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(41)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{exi-elim}\hfill}\hbox to 8em{\hss |def x (e)|}\quad$\movesto$\quad |e|\hfill \text{if $|x|\not\in \freevars{|e|}$}\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{exi-swap}\hfill}\hbox to 8em{\hss |def x1 ((def x2 (e')))|}\quad$\movesto$\quad |def x2 ((def x1 (e')))|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(def x ((def x2 (e'))))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |def x2 (e')|\,\}$} and {\color{blue}$\sigma_2=\{\,|x1|\mapsto |x|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(def x2 (e'))| \xrnungosup{exi-elim}{\emptypath} |t| \xrngosup{exi-swap}{\emptypath} |(def x2 ((def x (e'))))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{exi-swap}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{exi-elim}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{exi-elim} and \rulename{exi-swap} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(42)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{exi-elim}\hfill}\hbox to 8em{\hss |def x (e)|}\quad$\movesto$\quad |e|\hfill \text{if $|x|\not\in \freevars{|e|}$}\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{exi-swap}\hfill}\hbox to 8em{\hss |def x1 ((def x2 (e')))|}\quad$\movesto$\quad |def x2 ((def x1 (e')))|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(def x1 ((def x (e))))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|x2|\mapsto |x|,\,|e'|\mapsto |e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(def x1 (e))| \xrnungosup{exi-elim}{\2} |t| \xrngosup{exi-swap}{\emptypath} |(def x ((def x1 (e))))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{exi-swap}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{exi-elim}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{exi-elim} and \rulename{exi-swap} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(43)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{eqn-elim}\hfill}\hbox to 8em{\hss |def x ((x = v; e))|}\quad$\movesto$\quad |e|\hfill \text{if $|x|\not\in \freevars{|v|,|e|}$}\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{exi-swap}\hfill}\hbox to 8em{\hss |def x1 ((def x2 (e')))|}\quad$\movesto$\quad |def x2 ((def x1 (e')))|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(def x1 ((def x ((x = v; e)))))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|x2|\mapsto |x|,\,|e'|\mapsto |x = v; e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(def x1 (e))| \xrnungosup{eqn-elim}{\2} |t| \xrngosup{exi-swap}{\emptypath} |(def x ((def x1 ((x = v; e)))))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{exi-swap}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{eqn-elim}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{eqn-elim} and \rulename{exi-swap} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(44)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{fail-elim-eq}\hfill}\hbox to 8em{\hss |v = fail; e|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{exi-float-r}\hfill}\hbox to 8em{\hss |eq; (def x (e'))|}\quad$\movesto$\quad |def x ((eq; e'))|\hfill \text{if $|x|\not\in \freevars{|eq|}$}\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(v = fail; (def x (e')))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |def x (e')|\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |v = fail|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |fail| \xrnungosup{fail-elim-eq}{\emptypath} |t| \xrngosup{exi-float-r}{\emptypath} |(def x ((v = fail; e')))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{exi-float-r}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{fail-elim-eq}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{fail-elim-eq} and \rulename{exi-float-r} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(45)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{fail-elim-eq}\hfill}\hbox to 8em{\hss |v = fail; e|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq; e1); e2|}\quad$\movesto$\quad |eq; (e1; e2)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((v = fail; e); e2)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |v = fail|,\,|e1|\mapsto |e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(fail; e2)| \xrnungosup{fail-elim-eq}{\1} |t| \xrngosup{seq-assoc}{\emptypath} |(v = fail; (e; e2))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{fail-elim-eq}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{fail-elim-eq} and \rulename{seq-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(46)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{fail-elim-l}\hfill}\hbox to 8em{\hss |fail; e|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{fail-elim-r}\hfill}\hbox to 8em{\hss |e'; fail|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(fail; fail)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |fail|\,\}$} and {\color{blue}$\sigma_2=\{\,|e'|\mapsto |fail|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |fail| \xrnungosup{fail-elim-l}{\emptypath} |t| \xrngosup{fail-elim-r}{\emptypath} |fail| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{fail-elim-r}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{fail-elim-l}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{fail-elim-l} and \rulename{fail-elim-r} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(47)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{fail-elim-l}\hfill}\hbox to 8em{\hss |fail; e|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{exi-float-r}\hfill}\hbox to 8em{\hss |eq; (def x (e'))|}\quad$\movesto$\quad |def x ((eq; e'))|\hfill \text{if $|x|\not\in \freevars{|eq|}$}\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(fail; (def x (e')))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |def x (e')|\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |fail|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |fail| \xrnungosup{fail-elim-l}{\emptypath} |t| \xrngosup{exi-float-r}{\emptypath} |(def x ((fail; e')))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{exi-float-r}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{fail-elim-l}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{fail-elim-l} and \rulename{exi-float-r} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(48)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{fail-elim-l}\hfill}\hbox to 8em{\hss |fail; e|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq; e1); e2|}\quad$\movesto$\quad |eq; (e1; e2)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((fail; e); e2)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |fail|,\,|e1|\mapsto |e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(fail; e2)| \xrnungosup{fail-elim-l}{\1} |t| \xrngosup{seq-assoc}{\emptypath} |(fail; (e; e2))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{fail-elim-l}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{fail-elim-l} and \rulename{seq-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(49)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{fail-elim-r}\hfill}\hbox to 8em{\hss |e; fail|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{exi-float-l}\hfill}\hbox to 8em{\hss |(def x (e1)); e2|}\quad$\movesto$\quad |def x ((e1; e2))|\hfill \text{if $|x|\not\in \freevars{|e2|}$}\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((def x (e1)); fail)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |def x (e1)|\,\}$} and {\color{blue}$\sigma_2=\{\,|e2|\mapsto |fail|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |fail| \xrnungosup{fail-elim-r}{\emptypath} |t| \xrngosup{exi-float-l}{\emptypath} |(def x ((e1; fail)))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{exi-float-l}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{fail-elim-r}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{fail-elim-r} and \rulename{exi-float-l} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(50)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{fail-elim-r}\hfill}\hbox to 8em{\hss |e; fail|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq; e1); e2|}\quad$\movesto$\quad |eq; (e1; e2)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((eq; e1); fail)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |eq; e1|\,\}$} and {\color{blue}$\sigma_2=\{\,|e2|\mapsto |fail|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |fail| \xrnungosup{fail-elim-r}{\emptypath} |t| \xrngosup{seq-assoc}{\emptypath} |(eq; (e1; fail))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{fail-elim-r}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{fail-elim-r} and \rulename{seq-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(51)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{fail-elim-r}\hfill}\hbox to 8em{\hss |e; fail|}\quad$\movesto$\quad |fail|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq; e1); e2|}\quad$\movesto$\quad |eq; (e1; e2)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((e; fail); e2)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |e|,\,|e1|\mapsto |fail|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(fail; e2)| \xrnungosup{fail-elim-r}{\1} |t| \xrngosup{seq-assoc}{\emptypath} |(e; (fail; e2))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{fail-elim-r}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{fail-elim-r} and \rulename{seq-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(52)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{exi-float-eq}\hfill}\hbox to 8em{\hss |v = (def x (e1)); e2|}\quad$\movesto$\quad |def x ((v = e1; e2))|\hfill \text{if $|x|\not\in \freevars{|v|,|e2|}$}\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{exi-float-r}\hfill}\hbox to 8em{\hss |eq; (def x' (e))|}\quad$\movesto$\quad |def x' ((eq; e))|\hfill \text{if $|x'|\not\in \freevars{|eq|}$}\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(v = (def x (e1)); (def x' (e)))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e2|\mapsto |def x' (e)|\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |v = (def x (e1))|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(def x ((v = e1; (def x' (e)))))| \xrnungosup{exi-float-eq}{\emptypath} |t| \xrngosup{exi-float-r}{\emptypath} |(def x' ((v = (def x (e1)); e)))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{exi-float-r}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{exi-float-eq}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{exi-float-eq} and \rulename{exi-float-r} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(53)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{exi-float-eq}\hfill}\hbox to 8em{\hss |v = (def x (e1)); e2|}\quad$\movesto$\quad |def x ((v = e1; e2))|\hfill \text{if $|x|\not\in \freevars{|v|,|e2|}$}\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq; e1'); e2'|}\quad$\movesto$\quad |eq; (e1'; e2')|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((v = (def x (e1)); e2); e2')|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |v = (def x (e1))|,\,|e1'|\mapsto |e2|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |((def x ((v = e1; e2))); e2')| \xrnungosup{exi-float-eq}{\1} |t| \xrngosup{seq-assoc}{\emptypath} |(v = (def x (e1)); (e2; e2'))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{exi-float-eq}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{exi-float-eq} and \rulename{seq-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(54)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{exi-float-l}\hfill}\hbox to 8em{\hss |(def x (e1)); e2|}\quad$\movesto$\quad |def x ((e1; e2))|\hfill \text{if $|x|\not\in \freevars{|e2|}$}\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{exi-float-r}\hfill}\hbox to 8em{\hss |eq; (def x' (e))|}\quad$\movesto$\quad |def x' ((eq; e))|\hfill \text{if $|x'|\not\in \freevars{|eq|}$}\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((def x (e1)); (def x' (e)))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e2|\mapsto |def x' (e)|\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |def x (e1)|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(def x ((e1; (def x' (e)))))| \xrnungosup{exi-float-l}{\emptypath} |t| \xrngosup{exi-float-r}{\emptypath} |(def x' (((def x (e1)); e)))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{exi-float-r}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{exi-float-l}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{exi-float-l} and \rulename{exi-float-r} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(55)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{exi-float-l}\hfill}\hbox to 8em{\hss |(def x (e1)); e2|}\quad$\movesto$\quad |def x ((e1; e2))|\hfill \text{if $|x|\not\in \freevars{|e2|}$}\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq; e1'); e2'|}\quad$\movesto$\quad |eq; (e1'; e2')|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(((def x (e1)); e2); e2')|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|eq|\mapsto |def x (e1)|,\,|e1'|\mapsto |e2|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |((def x ((e1; e2))); e2')| \xrnungosup{exi-float-l}{\1} |t| \xrngosup{seq-assoc}{\emptypath} |((def x (e1)); (e2; e2'))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{exi-float-l}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{exi-float-l} and \rulename{seq-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(56)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{exi-float-r}\hfill}\hbox to 8em{\hss |eq; (def x (e))|}\quad$\movesto$\quad |def x ((eq; e))|\hfill \text{if $|x|\not\in \freevars{|eq|}$}\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{eqn-float}\hfill}\hbox to 8em{\hss |x' = (eq'; e1); e2|}\quad$\movesto$\quad |eq'; (v1 = v2; e2)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(x' = (eq'; e1); (def x (e)))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|eq|\mapsto |x' = (eq'; e1)|\,\}$} and {\color{blue}$\sigma_2=\{\,|e2|\mapsto |def x (e)|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(def x ((x' = (eq'; e1); e)))| \xrnungosup{exi-float-r}{\emptypath} |t| \xrngosup{eqn-float}{\emptypath} |(eq'; (v1 = v2; (def x (e))))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{eqn-float}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{exi-float-r}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{exi-float-r} and \rulename{eqn-float} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(57)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{exi-float-r}\hfill}\hbox to 8em{\hss |eq; (def x (e))|}\quad$\movesto$\quad |def x ((eq; e))|\hfill \text{if $|x|\not\in \freevars{|eq|}$}\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq'; e1); e2|}\quad$\movesto$\quad |eq'; (e1; e2)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((eq'; e1); (def x (e)))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|eq|\mapsto |eq'; e1|\,\}$} and {\color{blue}$\sigma_2=\{\,|e2|\mapsto |def x (e)|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(def x (((eq'; e1); e)))| \xrnungosup{exi-float-r}{\emptypath} |t| \xrngosup{seq-assoc}{\emptypath} |(eq'; (e1; (def x (e))))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{exi-float-r}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{exi-float-r} and \rulename{seq-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(58)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{exi-float-r}\hfill}\hbox to 8em{\hss |eq; (def x (e))|}\quad$\movesto$\quad |def x ((eq; e))|\hfill \text{if $|x|\not\in \freevars{|eq|}$}\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq'; e1); e2|}\quad$\movesto$\quad |eq'; (e1; e2)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((eq; (def x (e))); e2)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|eq'|\mapsto |eq|,\,|e1|\mapsto |def x (e)|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |((def x ((eq; e))); e2)| \xrnungosup{exi-float-r}{\1} |t| \xrngosup{seq-assoc}{\emptypath} |(eq; ((def x (e)); e2))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{exi-float-r}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{exi-float-r} and \rulename{seq-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(59)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{eqn-float}\hfill}\hbox to 8em{\hss |x = (eq; e1); e2|}\quad$\movesto$\quad |eq; (v1 = v2; e2)|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq'; e1'); e2'|}\quad$\movesto$\quad |eq'; (e1'; e2')|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((x = (eq; e1); e2); e2')|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|eq'|\mapsto |x = (eq; e1)|,\,|e1'|\mapsto |e2|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |((eq; (v1 = v2; e2)); e2')| \xrnungosup{eqn-float}{\1} |t| \xrngosup{seq-assoc}{\emptypath} |(x = (eq; e1); (e2; e2'))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{eqn-float}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{eqn-float} and \rulename{seq-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(60)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq; e1); e2|}\quad$\movesto$\quad |eq; (e1; e2)|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{seq-assoc}\hfill}\hbox to 8em{\hss |(eq'; e1'); e2'|}\quad$\movesto$\quad |eq'; (e1'; e2')|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(((eq; e1); e2); e2')|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|eq'|\mapsto |eq; e1|,\,|e1'|\mapsto |e2|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |((eq; (e1; e2)); e2')| \xrnungosup{seq-assoc}{\1} |t| \xrngosup{seq-assoc}{\emptypath} |((eq; e1); (e2; e2'))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{seq-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{seq-assoc}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{seq-assoc} and \rulename{seq-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(61)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{exi-swap}\hfill}\hbox to 8em{\hss |def x1 ((def x2 (e)))|}\quad$\movesto$\quad |def x2 ((def x1 (e)))|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{exi-swap}\hfill}\hbox to 8em{\hss |def x1' ((def x2' (e')))|}\quad$\movesto$\quad |def x2' ((def x1' (e')))|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(def x1' ((def x1 ((def x2 (e))))))|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|x2'|\mapsto |x1|,\,|e'|\mapsto |def x2 (e)|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(def x1' ((def x2 ((def x1 (e))))))| \xrnungosup{exi-swap}{\2} |t| \xrngosup{exi-swap}{\emptypath} |(def x1 ((def x1' ((def x2 (e))))))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{exi-swap}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{exi-swap}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{exi-swap} and \rulename{exi-swap} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(62)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{choose-r}\hfill}\hbox to 8em{\hss |fail `choice` e|}\quad$\movesto$\quad |e|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{choose-l}\hfill}\hbox to 8em{\hss |e' `choice` fail|}\quad$\movesto$\quad |e'|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(fail `choice` fail)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |fail|\,\}$} and {\color{blue}$\sigma_2=\{\,|e'|\mapsto |fail|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |fail| \xrnungosup{choose-r}{\emptypath} |t| \xrngosup{choose-l}{\emptypath} |fail| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{choose-l}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{choose-r}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{choose-r} and \rulename{choose-l} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(63)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{choose-r}\hfill}\hbox to 8em{\hss |fail `choice` e|}\quad$\movesto$\quad |e|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{choose-assoc}\hfill}\hbox to 8em{\hss |(e1 `choice` e2) `choice` e3|}\quad$\movesto$\quad |e1 `choice` (e2 `choice` e3)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((fail `choice` e) `choice` e3)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|e1|\mapsto |fail|,\,|e2|\mapsto |e|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(e `choice` e3)| \xrnungosup{choose-r}{\1} |t| \xrngosup{choose-assoc}{\emptypath} |(fail `choice` (e `choice` e3))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{choose-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{choose-r}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{choose-r} and \rulename{choose-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(64)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{choose-l}\hfill}\hbox to 8em{\hss |e `choice` fail|}\quad$\movesto$\quad |e|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{choose-assoc}\hfill}\hbox to 8em{\hss |(e1 `choice` e2) `choice` e3|}\quad$\movesto$\quad |e1 `choice` (e2 `choice` e3)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((e1 `choice` e2) `choice` fail)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,|e|\mapsto |e1 `choice` e2|\,\}$} and {\color{blue}$\sigma_2=\{\,|e3|\mapsto |fail|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(e1 `choice` e2)| \xrnungosup{choose-l}{\emptypath} |t| \xrngosup{choose-assoc}{\emptypath} |(e1 `choice` (e2 `choice` fail))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{choose-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{choose-l}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{choose-l} and \rulename{choose-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(65)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{choose-l}\hfill}\hbox to 8em{\hss |e `choice` fail|}\quad$\movesto$\quad |e|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{choose-assoc}\hfill}\hbox to 8em{\hss |(e1 `choice` e2) `choice` e3|}\quad$\movesto$\quad |e1 `choice` (e2 `choice` e3)|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |((e `choice` fail) `choice` e3)|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|e1|\mapsto |e|,\,|e2|\mapsto |fail|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |(e `choice` e3)| \xrnungosup{choose-l}{\1} |t| \xrngosup{choose-assoc}{\emptypath} |(e `choice` (fail `choice` e3))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{choose-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{choose-l}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{choose-l} and \rulename{choose-assoc} have the XXX property.\par
\vskip 8pt plus 16pt\noindent
\hbox to 5em{\rlap{(66)}\hphantom{and{\hskip0.5em}rule}\llap{Rule}\hfill}\hbox to 6em{\rulename{choose-assoc}\hfill}\hbox to 8em{\hss |(e1 `choice` e2) `choice` e3|}\quad$\movesto$\quad |e1 `choice` (e2 `choice` e3)|\relax\vadjust{\penalty1000}\hfil\break
\hbox to 5em{and{\hskip0.5em}rule\hfill}\hbox to 6em{\rulename{choose-assoc}\hfill}\hbox to 8em{\hss |(e1' `choice` e2') `choice` e3'|}\quad$\movesto$\quad |e1' `choice` (e2' `choice` e3')|\relax\vadjust{\penalty1000}\hfil\break
have a critical pair $(|t_1|, |t_2|)$ derived from the common term $|t| \equiv |(((e1 `choice` e2) `choice` e3) `choice` e3')|$\vadjust{\penalty1000}\hfil\break
obtained from {\color{blue}$\sigma_1=\{\,\}$} and {\color{blue}$\sigma_2=\{\,|e1'|\mapsto |e1 `choice` e2|,\,|e2'|\mapsto |e3|\,\}$}:\hfill\vadjust{\penalty1000}\hfil\break
\null\hskip 2em minus 1.95em $|t_1| \equiv |((e1 `choice` (e2 `choice` e3)) `choice` e3')| \xrnungosup{choose-assoc}{\1} |t| \xrngosup{choose-assoc}{\emptypath} |((e1 `choice` e2) `choice` (e3 `choice` e3'))| \equiv |t_2|$.\vadjust{\penalty1000}\hfil\break
{\color{purple}Can they be joined? $|t_1| (|nil|\xrngosup{choose-assoc}{\emptypath} nil) \bigl(|t'|\bigr) (\xrnungosup{choose-assoc}{\emptypath}|nil| nil) |t_2|$.\vadjust{\penalty1000}\hfil\break}
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
\vadjust{\penalty1000}\hfil\break
Therefore rules \rulename{choose-assoc} and \rulename{choose-assoc} have the XXX property.\par

E--------------
done
