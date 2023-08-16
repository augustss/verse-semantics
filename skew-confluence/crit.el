;;; TO DO
;;; contexts: match, join, and submatches
;;; Check decreasing diagrams
;;; Verify derivation of critical pairs deduced by joinable
;;; Add rewrite rule IDs
;;; Generate and print substituted conditions

;;; Temporary while switching over from cond to if/fresh
(setq use-if-fresh nil)


;;; Compute all critical pairs for the Verse Calculus

;;; It suffices to assume all variable-length tuples are length 2.
;;; We assume all rules are left-linear (makes matching trivial).

(setq the-grammar
      '((k ((quote 0)))			;integer
	(op ((quote add) (quote gt)))	;operation
	(d (k op (tup0) (tup2 v v)))	;data
	(hnf (d (lam x e)))		;head normal form
	(x ((quote var)))		;variable (may also use "y" or "z" or "f" or "g"---see function strip-decorations)
	(v (x hnf))			;value
	(eq (e (= v e)))		;expression or equation
	(e (v (seq eq e) (exists x e) (quote fail) (choice e e) (app v v) (one e) (all e) (replace e x x)))))			;expression

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

;;; Add "if" and "fresh"; later take out "cond"
(defstruct rule name lhs rhs cond if fresh)

;;; Note that primes are used only in rule U-TUP, and that is in conjunction with integer subscripts.
;;; This matters in function add-primes.
(setq the-rules
      (list (make-rule :name 'lam-alpha :lhs '(lam x e) :rhs '(replace (lam x e) x z) :cond '(fresh (not (elt z (fvs e)))) :fresh '(not (elt z (fvs e))))
            (make-rule :name 'exi-alpha :lhs '(exists x e) :rhs '(replace (exists x e) x z) :cond '(fresh (not (elt z (fvs e)))) :fresh '(not (elt z (fvs e))))
            (make-rule :name 'app-add :lhs '(app (quote add) (tup2 k1 k2)) :rhs 'k3 :cond '(if (= k3 (+ k1 k2))) :if '(= k3 (+ k1 k2)))
	    (make-rule :name 'app-gt :lhs '(app (quote gt) (tup2 k1 k2)) :rhs 'k1 :cond '(if (> k1 k2)) :if '(> k1 k2))
	    (make-rule :name 'app-gt-fail :lhs '(app (quote gt) (tup2 k1 k2)) :rhs '(quote fail) :cond '(if (not (> k1 k2))) :if '(not (> k1 k2)))
	    (make-rule :name 'app-beta :lhs '(app (lam x e) v) :rhs '(exists x (seq (= x v) e)) :cond '(if (not (elt x (fvs v)))) :if '(not (elt x (fvs v))))
	    (make-rule :name 'app-tup :lhs '(app (tup2 v0 v1) v) :rhs '(exists x (seq (= x v) (choice (seq (= x (quote 0)) v0) (seq (= x (quote 1)) v1)))) :cond '(fresh (not (elt x (fvs v v0 v1)))) :fresh '(not (elt x (fvs v v0 v1))))
	    (make-rule :name 'app-tup-0 :lhs '(app (tup0) v) :rhs '(quote fail))
	    (make-rule :name 'u-lit :lhs '(seq (= k1 k2) e) :rhs 'e :cond '(if (= k1 k2)) :if '(= k1 k2))
	    (make-rule :name 'u-tup :lhs '(seq (= (tup2 v1 vn) (tup2 v1prime vnprime)) e) :rhs '(seq (= v1 v1prime) (seq (= vn vnprime) e)))
	    (make-rule :name 'u-fail-op-d :lhs '(seq (= op d) e) :rhs '(quote fail))
	    (make-rule :name 'u-fail-d-op :lhs '(seq (= d op) e) :rhs '(quote fail))
	    (make-rule :name 'u-fail-tup-k :lhs '(seq (= (tup2 v1 vn) k) e) :rhs '(quote fail))
	    (make-rule :name 'u-fail-k-tup :lhs '(seq (= k (tup2 v1 vn)) e) :rhs '(quote fail))
	    ;; (make-rule :name 'u-occurs :lhs '() :rhs '())
	    ;; (make-rule :name 'unroll :lhs '() :rhs '())
	    ;; (make-rule :name 'subst :lhs '(seq (= x v) e) :rhs '(seq (= x v) (subst e v x)))
	    (make-rule :name 'hnf-swap :lhs '(seq (= hnf x) e) :rhs '(seq (= x hnf) e))
	    (make-rule :name 'var-swap :lhs '(seq (= x y) e) :rhs '(seq (= y x) e))
	    (make-rule :name 'seq-swap :lhs '(seq eq (seq (= x v) e)) :rhs '(seq (= x v) (seq eq e)))
	    (make-rule :name 'val-elim :lhs '(seq v e) :rhs 'e)
	    (make-rule :name 'exi-elim :lhs '(exists x e) :rhs 'e :cond '(if (not (elt x (fvs e)))) :if '(not (elt x (fvs e))))
	    (make-rule :name 'eqn-elim :lhs '(exists x (seq (= x v) e)) :rhs 'e :cond '(if (not (elt x (fvs v e)))) :if '(not (elt x (fvs v e))))
	    (make-rule :name 'fail-elim-eq :lhs '(seq (= v (quote fail)) e) :rhs '(quote fail))
	    (make-rule :name 'fail-elim-l :lhs '(seq (quote fail) e) :rhs '(quote fail))
	    (make-rule :name 'fail-elim-r :lhs '(seq eq (quote fail)) :rhs '(quote fail))
	    (make-rule :name 'exi-float-eq :lhs '(seq (= v (exists x e1)) e2) :rhs '(exists x (seq (= v e1) e2)) :cond '(if (not (elt x (fvs v e2)))) :if '(not (elt x (fvs v e2))))
	    (make-rule :name 'exi-float-l :lhs '(seq (exists x e1) e2) :rhs '(exists x (seq e1 e2)) :cond '(if (not (elt x (fvs e2)))) :if '(not (elt x (fvs e2))))
	    (make-rule :name 'exi-float-r :lhs '(seq eq (exists x e)) :rhs '(exists x (seq eq e)) :cond '(if (not (elt x (fvs eq)))) :if '(not (elt x (fvs eq))))
	    (make-rule :name 'eqn-float :lhs '(seq (= x (seq eq e1)) e2) :rhs '(seq eq (seq (= x e1) e2)))
	    (make-rule :name 'seq-assoc :lhs '(seq (seq eq e1) e2) :rhs '(seq eq (seq e1 e2)))
	    (make-rule :name 'exi-swap :lhs '(exists x (exists y e)) :rhs '(exists y (exists x e)))
	    (make-rule :name 'one-fail :lhs '(one (quote fail)) :rhs '(quote fail))
	    (make-rule :name 'one-value :lhs '(one v) :rhs 'v)
	    (make-rule :name 'one-choice :lhs '(one (choice v e)) :rhs 'v)
	    (make-rule :name 'all-fail :lhs '(all (quote fail)) :rhs '(tup0))
	    (make-rule :name 'all-value :lhs '(all v) :rhs '(tup1 v))
	    (make-rule :name 'all-choice-2 :lhs '(all (choice v1 vn)) :rhs '(tup2 v1 vn))
	    (make-rule :name 'all-choice-3 :lhs '(all (choice v1 (choice v2 v3))) :rhs '(tup3 v1 v2 v3))
	    (make-rule :name 'all-choice-4 :lhs '(all (choice v1 (choice v2 (choice v3 v4)))) :rhs '(tup4 v1 v2 v3 v4))
	    (make-rule :name 'split-fail :lhs '(split (quote fail) f g) :rhs '(app f (tup0)))
	    (make-rule :name 'split-value :lhs '(split v f g) :rhs '(app g (tup2 v (lam x (seq (= x (tup0)) (quote fail))))) :cond '(fresh x) :fresh 'x)
	    (make-rule :name 'split-choice :lhs '(split (choice v e) f g) :rhs '(app g (tup2 v (lam x (seq (= x (tup0)) e)))) :cond '(fresh (not (elt x (fvs e)))) :fresh '(not (elt x (fvs e))))
	    (make-rule :name 'choose-r :lhs '(choice (quote fail) e) :rhs 'e)
	    (make-rule :name 'choose-l :lhs '(choice e (quote fail)) :rhs 'e)
	    (make-rule :name 'choose-assoc :lhs '(choice (choice e1 e2) e3) :rhs '(choice e1 (choice e2 e3)))
	    ;; (make-rule :name 'choose :lhs '() :rhs '())
	    ))

;;; If members are added to this defstruct, be sure to update print-proof-skeletons and format-skeleton-rewrites-list.
(defstruct rewrite rulename path ellipsis id extra avoid)

;;; If members are added to this defstruct, be sure to update print-proof-skeletons.
(defstruct proof rulename1 rulename2 path1
	   id1 id2 extra1 extra2 cond
	   rowsep colsep flip-diagram difficult
	   rewrites1 rewrites2 altrewrites1 altrewrites2)

;;; Some of this was constructed automatically by function print-proof-skeletons (below).
;;(print-proof-skeletons the-proofs)

(setq the-proofs                ;120 proofs
      (list (make-proof :rulename1 'lam-alpha :rulename2 'app-beta :path1 '(1) :id1 1 :id2 2      ;Proof 1
			:extra1 'z
                        :rewrites1 (list (make-rewrite :rulename 'app-beta :path '() :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'exi-alpha :path '() :id 4 :extra 'z)))
            (make-proof :rulename1 'exi-alpha :rulename2 'exi-elim :path1 '() :id1 1 :id2 2      ;Proof 2
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-elim :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-alpha :path '())))
            (make-proof :rulename1 'exi-alpha :rulename2 'eqn-elim :path1 '() :id1 1 :id2 2      ;Proof 3
                        :rewrites1 (list 'X (make-rewrite :rulename 'eqn-elim :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-alpha :path '())))
            (make-proof :rulename1 'exi-alpha :rulename2 'exi-float-eq :path1 '(1 2) :id1 1 :id2 2      ;Proof 4
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-eq :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-alpha :path '())))
            (make-proof :rulename1 'exi-alpha :rulename2 'exi-float-l :path1 '(1) :id1 1 :id2 2      ;Proof 5
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-l :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-alpha :path '())))
            (make-proof :rulename1 'exi-alpha :rulename2 'exi-float-r :path1 '(2) :id1 1 :id2 2      ;Proof 6
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-alpha :path '())))
            (make-proof :rulename1 'exi-alpha :rulename2 'exi-swap :path1 '() :id1 1 :id2 2      ;Proof 7
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-swap :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-alpha :path '())))
            (make-proof :rulename1 'exi-alpha :rulename2 'exi-swap :path1 '(2) :id1 1 :id2 2      ;Proof 8
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-swap :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-alpha :path '())))
            (make-proof :rulename1 'app-gt :rulename2 'app-gt-fail :path1 '() :id1 1 :id2 2      ;Proof 9
                        :rewrites1 (list)
                        :rewrites2 (list))
            (make-proof :rulename1 'u-lit :rulename2 'seq-swap :path1 '() :id1 1 :id2 2      ;Proof 10
                        :rowsep "scriptsize"
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'u-lit :path '(2) :id 4)))
            (make-proof :rulename1 'u-lit :rulename2 'fail-elim-r :path1 '() :id1 1 :id2 2      ;Proof 11
                        :rowsep "scriptsize"
                        :rewrites1 (list)
                        :rewrites2 (list))
            (make-proof :rulename1 'u-lit :rulename2 'exi-float-r :path1 '() :id1 1 :id2 2      ;Proof 12
                        :rowsep "scriptsize"
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'u-lit :path '(2) :id 4)))
            (make-proof :rulename1 'u-lit :rulename2 'eqn-float :path1 '(1 2) :id1 1 :id2 2      ;Proof 13
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'u-lit :path '())))
            (make-proof :rulename1 'u-lit :rulename2 'seq-assoc :path1 '(1) :id1 1 :id2 2      ;Proof 14
                        :rowsep "scriptsize"
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'u-lit :path '() :id 4)))
            (make-proof :rulename1 'u-tup :rulename2 'seq-swap :path1 '() :id1 1 :id2 2      ;Proof 15
                        :rowsep "large" :colsep "normal" :flip-diagram t
                        :rewrites1 (list (make-rewrite :rulename 'seq-swap :path '((high-0-to-n-1 2)) :id 3) (make-rewrite :rulename 'seq-swap :path '((low-0-to-n-1 2)) :ellipsis t :id 5))
                        :rewrites2 (list (make-rewrite :rulename 'u-tup :path '(2) :id 4)))
            (make-proof :rulename1 'u-tup :rulename2 'fail-elim-r :path1 '() :id1 1 :id2 2      ;Proof 16
                        :rowsep "scriptsize" :colsep "large"
                        :rewrites1 (list (make-rewrite :rulename 'fail-elim-r :path '((high-0-to-n-1 2)) :id 3) (make-rewrite :rulename 'fail-elim-r :path '((low-0-to-n-1 2)) :ellipsis t :id 5))
                        :rewrites2 (list))
            (make-proof :rulename1 'u-tup :rulename2 'exi-float-r :path1 '() :id1 1 :id2 2      ;Proof 17
                        :rowsep "scriptsize"
                        :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '((high-0-to-n-1 2)) :id 3) (make-rewrite :rulename 'exi-float-r :path '((low-0-to-n-1 2)) :ellipsis t :id 5))
                        :rewrites2 (list (make-rewrite :rulename 'u-tup :path '(2) :id 4)))
            (make-proof :rulename1 'u-tup :rulename2 'eqn-float :path1 '(1 2) :id1 1 :id2 2      ;Proof 18
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'u-tup :path '())))
            (make-proof :rulename1 'u-tup :rulename2 'seq-assoc :path1 '(1) :id1 1 :id2 2      ;Proof 19
                        :rowsep "scriptsize"
                        :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '((low-0-to-n-1 2)) :id 3) (make-rewrite :rulename 'seq-assoc :path '((high-0-to-n-1 2)) :ellipsis t :id 5))
                        :rewrites2 (list (make-rewrite :rulename 'u-tup :path '() :id 4)))
            (make-proof :rulename1 'u-fail-op-d :rulename2 'u-fail-d-op :path1 '() :id1 1 :id2 2      ;Proof 20
                        :rowsep "large"
                        :rewrites1 (list)
                        :rewrites2 (list))
            (make-proof :rulename1 'u-fail-op-d :rulename2 'seq-swap :path1 '() :id1 1 :id2 2      ;Proof 21
                        :rowsep "large" :flip-diagram t
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'u-fail-op-d :path '(2) :id 4) (make-rewrite :rulename 'fail-elim-r :path '() :id 6)))
            (make-proof :rulename1 'u-fail-op-d :rulename2 'fail-elim-r :path1 '() :id1 1 :id2 2      ;Proof 22
                        :rowsep "normal"
                        :rewrites1 (list)
                        :rewrites2 (list))
            (make-proof :rulename1 'u-fail-op-d :rulename2 'exi-float-r :path1 '() :id1 1 :id2 2      ;Proof 23
                        :rowsep "large" :flip-diagram t
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'u-fail-op-d :path '(2) :id 4) (make-rewrite :rulename 'exi-elim :path '() :id 6)))
            (make-proof :rulename1 'u-fail-op-d :rulename2 'eqn-float :path1 '(1 2) :id1 1 :id2 2      ;Proof 24
                        :rewrites1 (list (make-rewrite :rulename 'fail-elim-eq :path '()))
                        :rewrites2 (list (make-rewrite :rulename 'u-fail-op-d :path '())))
            (make-proof :rulename1 'u-fail-op-d :rulename2 'seq-assoc :path1 '(1) :id1 1 :id2 2      ;Proof 25
                        :rowsep "normal"
                        :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '() :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'u-fail-op-d :path '() :id 4)))
            (make-proof :rulename1 'u-fail-d-op :rulename2 'seq-swap :path1 '() :id1 1 :id2 2      ;Proof 26
                        :rowsep "large" :colsep "large" :flip-diagram t
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'u-fail-d-op :path '(2) :id 4) (make-rewrite :rulename 'fail-elim-r :path '() :id 6)))
            (make-proof :rulename1 'u-fail-d-op :rulename2 'fail-elim-r :path1 '() :id1 1 :id2 2      ;Proof 27
                        :rowsep "large"
                        :rewrites1 (list)
                        :rewrites2 (list))
            (make-proof :rulename1 'u-fail-d-op :rulename2 'exi-float-r :path1 '() :id1 1 :id2 2      ;Proof 28
                        :rowsep "large" :colsep "large" :flip-diagram t
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'u-fail-d-op :path '(2) :id 4) (make-rewrite :rulename 'exi-elim :path '() :id 6)))
            (make-proof :rulename1 'u-fail-d-op :rulename2 'eqn-float :path1 '(1 2) :id1 1 :id2 2      ;Proof 29
                        :rewrites1 (list (make-rewrite :rulename 'fail-elim-eq :path '()))
                        :rewrites2 (list (make-rewrite :rulename 'u-fail-d-op :path '())))
            (make-proof :rulename1 'u-fail-d-op :rulename2 'seq-assoc :path1 '(1) :id1 1 :id2 2      ;Proof 30
                        :rowsep "large"
                        :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '() :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'u-fail-d-op :path '() :id 4)))
            (make-proof :rulename1 'u-fail-tup-k :rulename2 'seq-swap :path1 '() :id1 1 :id2 2      ;Proof 31
                        :rowsep "large" :colsep "large" :flip-diagram t
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'u-fail-tup-k :path '(2) :id 4) (make-rewrite :rulename 'fail-elim-r :path '() :id 6)))
            (make-proof :rulename1 'u-fail-tup-k :rulename2 'fail-elim-r :path1 '() :id1 1 :id2 2      ;Proof 32
                        :rowsep "large"
                        :rewrites1 (list)
                        :rewrites2 (list))
            (make-proof :rulename1 'u-fail-tup-k :rulename2 'exi-float-r :path1 '() :id1 1 :id2 2      ;Proof 33
                        :rowsep "large" :colsep "large" :flip-diagram t
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'u-fail-tup-k :path '(2) :id 4) (make-rewrite :rulename 'exi-elim :path '() :id 6)))
            (make-proof :rulename1 'u-fail-tup-k :rulename2 'eqn-float :path1 '(1 2) :id1 1 :id2 2      ;Proof 34
                        :rewrites1 (list (make-rewrite :rulename 'fail-elim-eq :path '()))
                        :rewrites2 (list (make-rewrite :rulename 'u-fail-tup-k :path '())))
            (make-proof :rulename1 'u-fail-tup-k :rulename2 'seq-assoc :path1 '(1) :id1 1 :id2 2      ;Proof 35
                        :rowsep "large"
                        :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '() :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'u-fail-tup-k :path '() :id 4)))
            (make-proof :rulename1 'u-fail-k-tup :rulename2 'seq-swap :path1 '() :id1 1 :id2 2      ;Proof 36
                        :rowsep "large" :colsep "large" :flip-diagram t
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'u-fail-k-tup :path '(2) :id 4) (make-rewrite :rulename 'fail-elim-r :path '() :id 6)))
            (make-proof :rulename1 'u-fail-k-tup :rulename2 'fail-elim-r :path1 '() :id1 1 :id2 2      ;Proof 37
                        :rowsep "large"
                        :rewrites1 (list)
                        :rewrites2 (list))
            (make-proof :rulename1 'u-fail-k-tup :rulename2 'exi-float-r :path1 '() :id1 1 :id2 2      ;Proof 38
                        :rowsep "large" :colsep "large" :flip-diagram t
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'u-fail-k-tup :path '(2) :id 4) (make-rewrite :rulename 'exi-elim :path '() :id 6)))
            (make-proof :rulename1 'u-fail-k-tup :rulename2 'eqn-float :path1 '(1 2) :id1 1 :id2 2      ;Proof 39
                        :rewrites1 (list (make-rewrite :rulename 'fail-elim-eq :path '()))
                        :rewrites2 (list (make-rewrite :rulename 'u-fail-k-tup :path '())))
            (make-proof :rulename1 'u-fail-k-tup :rulename2 'seq-assoc :path1 '(1) :id1 1 :id2 2      ;Proof 40
                        :rowsep "large"
                        :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '() :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'u-fail-k-tup :path '() :id 4)))
            (make-proof :rulename1 'hnf-swap :rulename2 'seq-swap :path1 '() :id1 1 :id2 2      ;Proof 41
                        :rowsep "normal"
                        :rewrites1 (list (make-rewrite :rulename 'seq-swap :path '() :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'hnf-swap :path '(2) :id 4)))
            (make-proof :rulename1 'hnf-swap :rulename2 'fail-elim-r :path1 '() :id1 1 :id2 2      ;Proof 42
                        :rowsep "normal"
                        :rewrites1 (list (make-rewrite :rulename 'fail-elim-r :path '() :id 3))
                        :rewrites2 (list))
            (make-proof :rulename1 'hnf-swap :rulename2 'exi-float-r :path1 '() :id1 1 :id2 2      ;Proof 43
                        :rowsep "normal"
                        :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '() :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'hnf-swap :path '(2) :id 4)))
            (make-proof :rulename1 'hnf-swap :rulename2 'eqn-float :path1 '(1 2) :id1 1 :id2 2      ;Proof 44
                        :rewrites1 (list (make-rewrite :rulename 'eqn-float :path '()))
                        :rewrites2 (list (make-rewrite :rulename 'hnf-swap :path '())))
            (make-proof :rulename1 'hnf-swap :rulename2 'seq-assoc :path1 '(1) :id1 1 :id2 2      ;Proof 45
                        :rowsep "normal"
                        :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '() :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'hnf-swap :path '() :id 4)))
            (make-proof :rulename1 'var-swap :rulename2 'seq-swap :path1 '() :id1 1 :id2 2      ;Proof 46
                        :rowsep "normal"
                        :rewrites1 (list (make-rewrite :rulename 'seq-swap :path '() :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'var-swap :path '(2) :id 4)))
            (make-proof :rulename1 'var-swap :rulename2 'seq-swap :path1 '(2) :id1 1 :id2 2      ;Proof 47
                        :rowsep "normal"
                        :rewrites1 (list (make-rewrite :rulename 'seq-swap :path '() :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'var-swap :path '() :id 4)))
            (make-proof :rulename1 'var-swap :rulename2 'eqn-elim :path1 '(2) :id1 1 :id2 2      ;Proof 48
                        :rewrites1 (list 'X (make-rewrite :rulename 'eqn-elim :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'var-swap :path '())))
            (make-proof :rulename1 'var-swap :rulename2 'fail-elim-r :path1 '() :id1 1 :id2 2      ;Proof 49
                        :rowsep "normal"
                        :rewrites1 (list (make-rewrite :rulename 'fail-elim-r :path '() :id 3))
                        :rewrites2 (list))
            (make-proof :rulename1 'var-swap :rulename2 'exi-float-r :path1 '() :id1 1 :id2 2      ;Proof 50
                        :rowsep "normal"
                        :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '() :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'var-swap :path '(2) :id 4)))
            (make-proof :rulename1 'var-swap :rulename2 'eqn-float :path1 '(1 2) :id1 1 :id2 2      ;Proof 51
                        :rewrites1 (list (make-rewrite :rulename 'eqn-float :path '()))
                        :rewrites2 (list (make-rewrite :rulename 'var-swap :path '())))
            (make-proof :rulename1 'var-swap :rulename2 'seq-assoc :path1 '(1) :id1 1 :id2 2      ;Proof 52
                        :rowsep "normal"
                        :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '() :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'var-swap :path '() :id 4)))
            (make-proof :rulename1 'seq-swap :rulename2 'seq-swap :path1 '(2) :id1 1 :id2 2      ;Proof 53
                        :rowsep "large" :flip-diagram t
                        :rewrites1 (list (make-rewrite :rulename 'seq-swap :path '(2) :id 3) (make-rewrite :rulename 'seq-swap :path '() :id 5))
                        :rewrites2 (list))
            (make-proof :rulename1 'seq-swap :rulename2 'val-elim :path1 '() :id1 1 :id2 2      ;Proof 54
                        :rowsep "normal"
                        :rewrites1 (list (make-rewrite :rulename 'val-elim :path '(2) :id 3))
                        :rewrites2 (list))
            (make-proof :rulename1 'seq-swap :rulename2 'eqn-elim :path1 '(2) :id1 1 :id2 2      ;Proof 55
                        :rewrites1 (list 'X (make-rewrite :rulename 'eqn-elim :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
            (make-proof :rulename1 'seq-swap :rulename2 'fail-elim-eq :path1 '() :id1 1 :id2 2      ;Proof 56
                        :rowsep "normal"
                        :rewrites1 (list (make-rewrite :rulename 'fail-elim-eq :path '(2) :id 3) (make-rewrite :rulename 'fail-elim-r :path '() :id 5))
                        :rewrites2 (list))
            (make-proof :rulename1 'seq-swap :rulename2 'fail-elim-l :path1 '() :id1 1 :id2 2      ;Proof 57
                        :rowsep "normal" :colsep "large"
                        :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '(2) :id 3) (make-rewrite :rulename 'fail-elim-r :path '() :id 5))
                        :rewrites2 (list))
            (make-proof :rulename1 'fail-elim-r :rulename2 'seq-swap :path1 '(2) :id1 1 :id2 2      ;Proof 58
                        :rowsep "large" :colsep "large" :flip-diagram t
                        :rewrites1 (list (make-rewrite :rulename 'fail-elim-r :path '() :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'fail-elim-r :path '(2) :id 4) (make-rewrite :rulename 'fail-elim-r :path '() :id 6)))
            (make-proof :rulename1 'seq-swap :rulename2 'exi-float-eq :path1 '() :id1 1 :id2 2      ;Proof 59
                        :rowsep "large" :colsep "normal" :flip-diagram t
                        :rewrites1 (list (make-rewrite :rulename 'exi-float-eq :path '(2) :id 3) (make-rewrite :rulename 'exi-float-r :path '() :id 5))
                        :rewrites2 (list (make-rewrite :rulename 'seq-swap :path '(2) :id 4)))
            (make-proof :rulename1 'seq-swap :rulename2 'exi-float-l :path1 '() :id1 1 :id2 2      ;Proof 60
                        :rowsep "large" :colsep "large"
                        :rewrites1 (list (make-rewrite :rulename 'exi-float-l :path '(2) :id 3) (make-rewrite :rulename 'exi-float-r :path '() :id 5))
                        :rewrites2 (list (make-rewrite :rulename 'seq-swap :path '(2) :id 4)))
            (make-proof :rulename1 'exi-float-r :rulename2 'seq-swap :path1 '(2) :id1 1 :id2 2      ;Proof 61
                        :rowsep "large" :colsep "large"
                        :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '() :id 3) (make-rewrite :rulename 'seq-swap :path '(2) :id 5))
                        :rewrites2 (list (make-rewrite :rulename 'exi-float-r :path '(2) :id 4) (make-rewrite :rulename 'exi-float-r :path '() :id 6)))
            (make-proof :rulename1 'seq-swap :rulename2 'eqn-float :path1 '() :id1 1 :id2 2      ;Proof 62
                        :rowsep "large"
                        :rewrites1 (list (make-rewrite :rulename 'eqn-float :path '(2) :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'seq-swap :path '(2) :id 4) (make-rewrite :rulename 'seq-swap :path '() :id 6)))
            (make-proof :rulename1 'seq-swap :rulename2 'eqn-float :path1 '(1 2) :id1 1 :id2 2      ;Proof 63
                        :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
            (make-proof :rulename1 'seq-swap :rulename2 'seq-assoc :path1 '() :id1 1 :id2 2      ;Proof 64
                        :rowsep "large"
                        :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '(2) :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'seq-swap :path '(2) :id 4) (make-rewrite :rulename 'seq-swap :path '() :id 6)))
            (make-proof :rulename1 'seq-swap :rulename2 'seq-assoc :path1 '(1) :id1 1 :id2 2      ;Proof 65
                        :rowsep "large"
                        :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '() :id 3) (make-rewrite :rulename 'seq-assoc :path '(2) :id 5))
                        :rewrites2 (list (make-rewrite :rulename 'seq-assoc :path '(2) :id 4) (make-rewrite :rulename 'seq-swap :path '() :id 6)))
            (make-proof :rulename1 'val-elim :rulename2 'fail-elim-r :path1 '() :id1 1 :id2 2      ;Proof 66
                        :rowsep "normal"
                        :rewrites1 (list)
                        :rewrites2 (list))
            (make-proof :rulename1 'val-elim :rulename2 'exi-float-r :path1 '() :id1 1 :id2 2      ;Proof 67
                        :rowsep "normal"
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'val-elim :path '(2) :id 4)))
            (make-proof :rulename1 'val-elim :rulename2 'eqn-float :path1 '(1 2) :id1 1 :id2 2      ;Proof 68
                        :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'val-elim :path '())))
            (make-proof :rulename1 'val-elim :rulename2 'seq-assoc :path1 '(1) :id1 1 :id2 2      ;Proof 69
                        :rowsep "normal"
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'val-elim :path '() :id 4)))
            (make-proof :rulename1 'exi-elim :rulename2 'eqn-elim :path1 '() :id1 1 :id2 2      ;Proof 70
                        :rowsep "large"
                        :rewrites1 (list)
                        :rewrites2 (list))
            (make-proof :rulename1 'exi-elim :rulename2 'exi-float-eq :path1 '(1 2) :id1 1 :id2 2      ;Proof 71
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-eq :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-elim :path '())))
            (make-proof :rulename1 'exi-elim :rulename2 'exi-float-l :path1 '(1) :id1 1 :id2 2      ;Proof 72
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-l :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-elim :path '())))
            (make-proof :rulename1 'exi-elim :rulename2 'exi-float-r :path1 '(2) :id1 1 :id2 2      ;Proof 73
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-elim :path '())))
            (make-proof :rulename1 'exi-elim :rulename2 'exi-swap :path1 '() :id1 1 :id2 2      ;Proof 74
                        :rowsep "large" :flip-diagram t
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'exi-swap :path '() :id 4) (make-rewrite :rulename 'exi-elim :path '() :id 6)))
            (make-proof :rulename1 'exi-elim :rulename2 'exi-swap :path1 '(2) :id1 1 :id2 2      ;Proof 75
                        :rowsep "large" :flip-diagram t
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'exi-swap :path '() :id 4) (make-rewrite :rulename 'exi-elim :path '(2) :id 6)))
            (make-proof :rulename1 'fail-elim-r :rulename2 'eqn-elim :path1 '(2) :id1 1 :id2 2      ;Proof 76
                        :rewrites1 (list 'X (make-rewrite :rulename 'eqn-elim :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-r :path '())))
            (make-proof :rulename1 'eqn-elim :rulename2 'exi-float-eq :path1 '(1 2) :id1 1 :id2 2      ;Proof 77
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-eq :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'eqn-elim :path '())))
            (make-proof :rulename1 'eqn-elim :rulename2 'exi-float-l :path1 '(1) :id1 1 :id2 2      ;Proof 78
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-l :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'eqn-elim :path '())))
            (make-proof :rulename1 'eqn-elim :rulename2 'exi-float-r :path1 '(2) :id1 1 :id2 2      ;Proof 79
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'eqn-elim :path '())))
            (make-proof :rulename1 'exi-float-r :rulename2 'eqn-elim :path1 '(2) :id1 1 :id2 2      ;Proof 80
                        :rewrites1 (list 'X (make-rewrite :rulename 'eqn-elim :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-r :path '())))
            (make-proof :rulename1 'eqn-elim :rulename2 'exi-swap :path1 '(2) :id1 1 :id2 2      ;Proof 81
                        :rowsep "large" :flip-diagram t
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'exi-swap :path '() :id 4) (make-rewrite :rulename 'eqn-elim :path '(2) :id 6)))
            (make-proof :rulename1 'fail-elim-eq :rulename2 'fail-elim-r :path1 '() :id1 1 :id2 2      ;Proof 82
                        :rowsep "large"
                        :rewrites1 (list)
                        :rewrites2 (list))
            (make-proof :rulename1 'fail-elim-eq :rulename2 'exi-float-r :path1 '() :id1 1 :id2 2      ;Proof 83
                        :rowsep "large" :colsep "large" :flip-diagram t
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'fail-elim-eq :path '(2) :id 4) (make-rewrite :rulename 'exi-elim :path '() :id 6)))
            (make-proof :rulename1 'fail-elim-eq :rulename2 'eqn-float :path1 '(1 2) :id1 1 :id2 2      ;Proof 84
                        :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-eq :path '())))
            (make-proof :rulename1 'fail-elim-eq :rulename2 'seq-assoc :path1 '(1) :id1 1 :id2 2      ;Proof 85
                        :rowsep "large"
                        :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '() :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'fail-elim-eq :path '() :id 4)))
            (make-proof :rulename1 'fail-elim-l :rulename2 'fail-elim-r :path1 '() :id1 1 :id2 2      ;Proof 86
                        :rowsep "large"
                        :rewrites1 (list)
                        :rewrites2 (list))
            (make-proof :rulename1 'fail-elim-l :rulename2 'exi-float-r :path1 '() :id1 1 :id2 2      ;Proof 87
                        :rowsep "large" :colsep "large" :flip-diagram t
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'fail-elim-l :path '(2) :id 4) (make-rewrite :rulename 'exi-elim :path '() :id 6)))
            (make-proof :rulename1 'fail-elim-l :rulename2 'eqn-float :path1 '(1 2) :id1 1 :id2 2      ;Proof 88
                        :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-l :path '())))
            (make-proof :rulename1 'fail-elim-l :rulename2 'seq-assoc :path1 '(1) :id1 1 :id2 2      ;Proof 89
                        :rowsep "large"
                        :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '() :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'fail-elim-l :path '() :id 4)))
            (make-proof :rulename1 'fail-elim-r :rulename2 'exi-float-eq :path1 '() :id1 1 :id2 2      ;Proof 90
                        :rowsep "large" :colsep "large" :flip-diagram t
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'fail-elim-r :path '(2) :id 4) (make-rewrite :rulename 'exi-elim :path '() :id 6)))
            (make-proof :rulename1 'fail-elim-r :rulename2 'exi-float-l :path1 '() :id1 1 :id2 2      ;Proof 91
                        :rowsep "large" :colsep "large" :flip-diagram t
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'fail-elim-r :path '(2) :id 4) (make-rewrite :rulename 'exi-elim :path '() :id 6)))
            (make-proof :rulename1 'fail-elim-r :rulename2 'eqn-float :path1 '() :id1 1 :id2 2      ;Proof 92
                        :rowsep "large" :colsep "large" :flip-diagram t
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'fail-elim-r :path '(2) :id 4) (make-rewrite :rulename 'fail-elim-r :path '() :id 6)))
            (make-proof :rulename1 'fail-elim-r :rulename2 'eqn-float :path1 '(1 2) :id1 1 :id2 2      ;Proof 93
                        :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-r :path '())))
            (make-proof :rulename1 'fail-elim-r :rulename2 'seq-assoc :path1 '() :id1 1 :id2 2      ;Proof 94
                        :rowsep "large" :colsep "large" :flip-diagram t
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'fail-elim-r :path '(2) :id 4) (make-rewrite :rulename 'fail-elim-r :path '() :id 6)))
            (make-proof :rulename1 'fail-elim-r :rulename2 'seq-assoc :path1 '(1) :id1 1 :id2 2      ;Proof 95
                        :rowsep "large" :colsep "large" :flip-diagram t
                        :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '() :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'fail-elim-l :path '(2) :id 4) (make-rewrite :rulename 'fail-elim-r :path '() :id 6)))
            (make-proof :rulename1 'exi-float-eq :rulename2 'exi-float-r :path1 '() :id1 1 :id2 2      ;Proof 96
                        :rowsep "large" :colsep "large"
                        :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '(2) :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'exi-float-eq :path '(2) :id 4) (make-rewrite :rulename 'exi-swap :path '() :id 6)))
            (make-proof :rulename1 'exi-float-eq :rulename2 'eqn-float :path1 '(1 2) :id1 1 :id2 2      ;Proof 97
                        :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-eq :path '())))
            (make-proof :rulename1 'exi-float-eq :rulename2 'seq-assoc :path1 '(1) :id1 1 :id2 2      ;Proof 98
                        :rowsep "large" :colsep "large"
                        :rewrites1 (list (make-rewrite :rulename 'exi-float-l :path '() :id 3) (make-rewrite :rulename 'seq-assoc :path '(2) :id 5))
                        :rewrites2 (list (make-rewrite :rulename 'exi-float-eq :path '() :id 4)))
            (make-proof :rulename1 'exi-swap :rulename2 'exi-float-eq :path1 '(1 2) :id1 1 :id2 2      ;Proof 99
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-eq :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-swap :path '())))
            (make-proof :rulename1 'exi-float-l :rulename2 'exi-float-r :path1 '() :id1 1 :id2 2      ;Proof 100
                        :rowsep "large" :colsep "large" :flip-diagram t
                        :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '(2) :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'exi-float-l :path '(2) :id 4) (make-rewrite :rulename 'exi-swap :path '() :id 6)))
            (make-proof :rulename1 'exi-float-l :rulename2 'eqn-float :path1 '(1 2) :id1 1 :id2 2      ;Proof 101
                        :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-l :path '())))
            (make-proof :rulename1 'exi-float-l :rulename2 'seq-assoc :path1 '(1) :id1 1 :id2 2      ;Proof 102
                        :rowsep "large" :colsep "large"
                        :rewrites1 (list (make-rewrite :rulename 'exi-float-l :path '() :id 3) (make-rewrite :rulename 'seq-assoc :path '(2) :id 5))
                        :rewrites2 (list (make-rewrite :rulename 'exi-float-l :path '() :id 4)))
            (make-proof :rulename1 'exi-swap :rulename2 'exi-float-l :path1 '(1) :id1 1 :id2 2      ;Proof 103
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-l :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-swap :path '())))
            (make-proof :rulename1 'exi-float-r :rulename2 'eqn-float :path1 '() :id1 1 :id2 2      ;Proof 104
                        :rowsep "large"
                        :rewrites1 (list (make-rewrite :rulename 'eqn-float :path '(2) :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'exi-float-r :path '(2) :id 4) (make-rewrite :rulename 'exi-float-r :path '() :id 6)))
            (make-proof :rulename1 'exi-float-r :rulename2 'eqn-float :path1 '(1 2) :id1 1 :id2 2      ;Proof 105
                        :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-r :path '())))
            (make-proof :rulename1 'exi-float-r :rulename2 'seq-assoc :path1 '() :id1 1 :id2 2      ;Proof 106
                        :rowsep "large" :colsep "large" :flip-diagram t
                        :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '(2) :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'exi-float-r :path '(2) :id 4) (make-rewrite :rulename 'exi-float-r :path '() :id 6)))
            (make-proof :rulename1 'exi-float-r :rulename2 'seq-assoc :path1 '(1) :id1 1 :id2 2      ;Proof 107
                        :rowsep "large" :colsep "large"
                        :rewrites1 (list (make-rewrite :rulename 'exi-float-l :path '() :id 3) (make-rewrite :rulename 'seq-assoc :path '(2) :id 5))
                        :rewrites2 (list (make-rewrite :rulename 'exi-float-l :path '(2) :id 4) (make-rewrite :rulename 'exi-float-r :path '() :id 6)))
            (make-proof :rulename1 'exi-swap :rulename2 'exi-float-r :path1 '(2) :id1 1 :id2 2      ;Proof 108
                        :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'exi-swap :path '())))
            (make-proof :rulename1 'eqn-float :rulename2 'eqn-float :path1 '(1 2) :id1 1 :id2 2      ;Proof 109
                        :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'eqn-float :path '())))
            (make-proof :rulename1 'eqn-float :rulename2 'seq-assoc :path1 '(1) :id1 1 :id2 2      ;Proof 110
                        :rowsep "large"
                        :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '() :id 3) (make-rewrite :rulename 'seq-assoc :path '(2) :id 5))
                        :rewrites2 (list (make-rewrite :rulename 'eqn-float :path '() :id 4)))
            (make-proof :rulename1 'seq-assoc :rulename2 'eqn-float :path1 '(1 2) :id1 1 :id2 2      ;Proof 111
                        :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
                        :rewrites2 (list 'X (make-rewrite :rulename 'seq-assoc :path '())))
            (make-proof :rulename1 'seq-assoc :rulename2 'seq-assoc :path1 '(1) :id1 1 :id2 2      ;Proof 112
                        :rowsep "large"
                        :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '() :id 3) (make-rewrite :rulename 'seq-assoc :path '(2) :id 5))
                        :rewrites2 (list (make-rewrite :rulename 'seq-assoc :path '() :id 4)))
            (make-proof :rulename1 'exi-swap :rulename2 'exi-swap :path1 '(2) :id1 1 :id2 2      ;Proof 113
                        :rowsep "normal"
                        :rewrites1 (list (make-rewrite :rulename 'exi-swap :path '(2) :id 3))
                        :rewrites2 (list (make-rewrite :rulename 'exi-swap :path '() :id 4)))
            (make-proof :rulename1 'choose-l :rulename2 'one-choice :path1 '(1) :id1 1 :id2 2      ;Proof 114
                        :rewrites1 (list (make-rewrite :rulename 'one-value :path '()))
                        :rewrites2 (list))
            (make-proof :rulename1 'choose-l :rulename2 'split-choice :path1 '(1) :id1 1 :id2 2      ;Proof 115
                        :rewrites1 (list (make-rewrite :rulename 'split-value :path '()))
                        :rewrites2 (list))
            (make-proof :rulename1 'choose-r :rulename2 'choose-l :path1 '() :id1 1 :id2 2      ;Proof 116
                        :rowsep "normal"
                        :rewrites1 (list)
                        :rewrites2 (list))
            (make-proof :rulename1 'choose-r :rulename2 'choose-assoc :path1 '(1) :id1 1 :id2 2      ;Proof 117
                        :rowsep "normal"
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'choose-r :path '() :id 4)))
            (make-proof :rulename1 'choose-l :rulename2 'choose-assoc :path1 '() :id1 1 :id2 2      ;Proof 118
                        :rowsep "normal"
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'choose-l :path '(2) :id 4)))
            (make-proof :rulename1 'choose-l :rulename2 'choose-assoc :path1 '(1) :id1 1 :id2 2      ;Proof 119
                        :rowsep "normal"
                        :rewrites1 (list)
                        :rewrites2 (list (make-rewrite :rulename 'choose-r :path '(2) :id 4)))
            (make-proof :rulename1 'choose-assoc :rulename2 'choose-assoc :path1 '(1) :id1 1 :id2 2      ;Proof 120
                        :rowsep "large" :colsep "large"
                        :rewrites1 (list (make-rewrite :rulename 'choose-assoc :path '() :id 3) (make-rewrite :rulename 'choose-assoc :path '(2) :id 5))
                        :rewrites2 (list (make-rewrite :rulename 'choose-assoc :path '() :id 4)))))


;; (setq the-proofs                ;119 proofs
;;       (list (make-proof :rulename1 'lam-alpha :rulename2 'app-beta :path1 '()      ;Proof 1
;;                         :rewrites1 (list (make-rewrite :rulename 'app-beta :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-alpha :path '())))
;;             (make-proof :rulename1 'exi-alpha :rulename2 'exi-elim :path1 '()      ;Proof 2
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-elim :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-alpha :path '())))
;;             (make-proof :rulename1 'exi-alpha :rulename2 'eqn-elim :path1 '()      ;Proof 3
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-elim :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-alpha :path '())))
;;             (make-proof :rulename1 'exi-alpha :rulename2 'exi-float-eq :path1 '(1 2)      ;Proof 4
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-eq :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-alpha :path '())))
;;             (make-proof :rulename1 'exi-alpha :rulename2 'exi-float-l :path1 '(1)      ;Proof 5
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-l :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-alpha :path '())))
;;             (make-proof :rulename1 'exi-alpha :rulename2 'exi-float-r :path1 '(2)      ;Proof 6
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-alpha :path '())))
;;             (make-proof :rulename1 'exi-alpha :rulename2 'exi-swap :path1 '()      ;Proof 7
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-alpha :path '())))
;;             (make-proof :rulename1 'exi-alpha :rulename2 'exi-swap :path1 '(2)      ;Proof 8
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-swap :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-alpha :path '())))
;;             (make-proof :rulename1 'app-gt :rulename2 'app-gt-fail :path1 '()      ;Proof 9
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-lit :rulename2 'seq-swap :path1 '()      ;Proof 10
;;                         :rowsep "scriptsize"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-lit :path '(2))))
;;             (make-proof :rulename1 'u-lit :rulename2 'fail-elim-r :path1 '()      ;Proof 11
;;                         :rowsep "scriptsize"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-lit :rulename2 'exi-float-r :path1 '()      ;Proof 12
;;                         :rowsep "scriptsize"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-lit :path '(2))))
;;             (make-proof :rulename1 'u-lit :rulename2 'eqn-float :path1 '(1 2)      ;Proof 13
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-lit :path '())))
;;             (make-proof :rulename1 'u-lit :rulename2 'seq-assoc :path1 '(1)      ;Proof 14
;;                         :rowsep "scriptsize"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-lit :path '())))
;;             (make-proof :rulename1 'u-tup :rulename2 'seq-swap :path1 '()      ;Proof 15
;;                         :rowsep "large" :colsep "normal" :flip-diagram t
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-swap :path '((high-0-to-n-1 2))) (make-rewrite :rulename 'seq-swap :path '((low-0-to-n-1 2)) :ellipsis t))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-tup :path '(2))))
;;             (make-proof :rulename1 'u-tup :rulename2 'fail-elim-r :path1 '()      ;Proof 16
;;                         :rowsep "scriptsize" :colsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-r :path '((high-0-to-n-1 2))) (make-rewrite :rulename 'fail-elim-r :path '((low-0-to-n-1 2)) :ellipsis t))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-tup :rulename2 'exi-float-r :path1 '()      ;Proof 17
;;                         :rowsep "scriptsize"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '((high-0-to-n-1 2))) (make-rewrite :rulename 'exi-float-r :path '((low-0-to-n-1 2)) :ellipsis t))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-tup :path '(2))))
;;             (make-proof :rulename1 'u-tup :rulename2 'eqn-float :path1 '(1 2)      ;Proof 18
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-tup :path '())))
;;             (make-proof :rulename1 'u-tup :rulename2 'seq-assoc :path1 '(1)      ;Proof 19
;;                         :rowsep "scriptsize"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '((low-0-to-n-1 2))) (make-rewrite :rulename 'seq-assoc :path '((high-0-to-n-1 2)) :ellipsis t))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-tup :path '())))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'u-fail-d-op :path1 '()      ;Proof 20
;;                         :rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'seq-swap :path1 '()      ;Proof 21
;;                         :rowsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-op-d :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'fail-elim-r :path1 '()      ;Proof 22
;;                         :rowsep "normal"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'exi-float-r :path1 '()      ;Proof 23
;;                         :rowsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-op-d :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'eqn-float :path1 '(1 2)      ;Proof 24
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-op-d :path '())))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'seq-assoc :path1 '(1)      ;Proof 25
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-op-d :path '())))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'seq-swap :path1 '()      ;Proof 26
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-d-op :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'fail-elim-r :path1 '()      ;Proof 27
;;                         :rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'exi-float-r :path1 '()      ;Proof 28
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-d-op :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'eqn-float :path1 '(1 2)      ;Proof 29
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-d-op :path '())))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'seq-assoc :path1 '(1)      ;Proof 30
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-d-op :path '())))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'seq-swap :path1 '()      ;Proof 31
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-tup-k :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'fail-elim-r :path1 '()      ;Proof 32
;;                         :rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'exi-float-r :path1 '()      ;Proof 33
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-tup-k :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'eqn-float :path1 '(1 2)      ;Proof 34
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-tup-k :path '())))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'seq-assoc :path1 '(1)      ;Proof 35
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-tup-k :path '())))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'seq-swap :path1 '()      ;Proof 36
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-k-tup :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'fail-elim-r :path1 '()      ;Proof 37
;;                         :rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'exi-float-r :path1 '()      ;Proof 38
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-k-tup :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'eqn-float :path1 '(1 2)      ;Proof 39
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'u-fail-k-tup :path '())))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'seq-assoc :path1 '(1)      ;Proof 40
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-k-tup :path '())))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'seq-swap :path1 '()      ;Proof 41
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'hnf-swap :path '(2))))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'fail-elim-r :path1 '()      ;Proof 42
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-r :path '()))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'exi-float-r :path1 '()      ;Proof 43
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'hnf-swap :path '(2))))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'eqn-float :path1 '(1 2)      ;Proof 44
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'hnf-swap :path '())))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'seq-assoc :path1 '(1)      ;Proof 45
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'hnf-swap :path '())))
;;             (make-proof :rulename1 'var-swap :rulename2 'seq-swap :path1 '()      ;Proof 46
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'var-swap :path '(2))))
;;             (make-proof :rulename1 'var-swap :rulename2 'seq-swap :path1 '(2)      ;Proof 47
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'var-swap :path '())))
;;             (make-proof :rulename1 'var-swap :rulename2 'eqn-elim :path1 '(2)      ;Proof 48
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-elim :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'var-swap :path '())))
;;             (make-proof :rulename1 'var-swap :rulename2 'fail-elim-r :path1 '()      ;Proof 49
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-r :path '()))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'var-swap :rulename2 'exi-float-r :path1 '()      ;Proof 50
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'var-swap :path '(2))))
;;             (make-proof :rulename1 'var-swap :rulename2 'eqn-float :path1 '(1 2)      ;Proof 51
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'var-swap :path '())))
;;             (make-proof :rulename1 'var-swap :rulename2 'seq-assoc :path1 '(1)      ;Proof 52
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'var-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'seq-swap :path1 '(2)      ;Proof 53
;;                         :rowsep "large" :flip-diagram t
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-swap :path '(2)) (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'seq-swap :rulename2 'val-elim :path1 '()      ;Proof 54
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'val-elim :path '(2)))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'seq-swap :rulename2 'eqn-elim :path1 '(2)      ;Proof 55
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-elim :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'fail-elim-eq :path1 '()      ;Proof 56
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-eq :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '()))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'seq-swap :rulename2 'fail-elim-l :path1 '()      ;Proof 57
;;                         :rowsep "normal" :colsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '()))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'seq-swap :path1 '(2)      ;Proof 58
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-r :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-r :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'exi-float-eq :path1 '()      ;Proof 59
;;                         :rowsep "large" :colsep "normal" :flip-diagram t
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-eq :path '(2)) (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'seq-swap :path '(2))))
;;             (make-proof :rulename1 'seq-swap :rulename2 'exi-float-l :path1 '()      ;Proof 60
;;                         :rowsep "large" :colsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-l :path '(2)) (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'seq-swap :path '(2))))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'seq-swap :path1 '(2)      ;Proof 61
;;                         :rowsep "large" :colsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '()) (make-rewrite :rulename 'seq-swap :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-r :path '(2)) (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'eqn-float :path1 '()      ;Proof 62
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'eqn-float :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'seq-swap :path '(2)) (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'eqn-float :path1 '(1 2)      ;Proof 63
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'seq-assoc :path1 '()      ;Proof 64
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'seq-swap :path '(2)) (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'seq-assoc :path1 '(1)      ;Proof 65
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '()) (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'seq-assoc :path '(2)) (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'val-elim :rulename2 'fail-elim-r :path1 '()      ;Proof 66
;;                         :rowsep "normal"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'val-elim :rulename2 'exi-float-r :path1 '()      ;Proof 67
;;                         :rowsep "normal"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'val-elim :path '(2))))
;;             (make-proof :rulename1 'val-elim :rulename2 'eqn-float :path1 '(1 2)      ;Proof 68
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'val-elim :path '())))
;;             (make-proof :rulename1 'val-elim :rulename2 'seq-assoc :path1 '(1)      ;Proof 69
;;                         :rowsep "normal"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'val-elim :path '())))
;;             (make-proof :rulename1 'exi-elim :rulename2 'eqn-elim :path1 '()      ;Proof 70
;;                         :rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'exi-elim :rulename2 'exi-float-eq :path1 '(1 2)      ;Proof 71
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-eq :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'exi-elim :rulename2 'exi-float-l :path1 '(1)      ;Proof 72
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-l :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'exi-elim :rulename2 'exi-float-r :path1 '(2)      ;Proof 73
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'exi-elim :rulename2 'exi-swap :path1 '()      ;Proof 74
;;                         :rowsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-swap :path '()) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'exi-elim :rulename2 'exi-swap :path1 '(2)      ;Proof 75
;;                         :rowsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-swap :path '()) (make-rewrite :rulename 'exi-elim :path '(2))))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'eqn-elim :path1 '(2)      ;Proof 76
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-elim :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'eqn-elim :rulename2 'exi-float-eq :path1 '(1 2)      ;Proof 77
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-eq :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'eqn-elim :path '())))
;;             (make-proof :rulename1 'eqn-elim :rulename2 'exi-float-l :path1 '(1)      ;Proof 78
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-l :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'eqn-elim :path '())))
;;             (make-proof :rulename1 'eqn-elim :rulename2 'exi-float-r :path1 '(2)      ;Proof 79
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'eqn-elim :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'eqn-elim :path1 '(2)      ;Proof 80
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-elim :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'eqn-elim :rulename2 'exi-swap :path1 '(2)      ;Proof 81
;;                         :rowsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-swap :path '()) (make-rewrite :rulename 'eqn-elim :path '(2))))
;;             (make-proof :rulename1 'fail-elim-eq :rulename2 'fail-elim-r :path1 '()      ;Proof 82
;;                         :rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'fail-elim-eq :rulename2 'exi-float-r :path1 '()      ;Proof 83
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-eq :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'fail-elim-eq :rulename2 'eqn-float :path1 '(1 2)      ;Proof 84
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-eq :path '())))
;;             (make-proof :rulename1 'fail-elim-eq :rulename2 'seq-assoc :path1 '(1)      ;Proof 85
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-eq :path '())))
;;             (make-proof :rulename1 'fail-elim-l :rulename2 'fail-elim-r :path1 '()      ;Proof 86
;;                         :rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'fail-elim-l :rulename2 'exi-float-r :path1 '()      ;Proof 87
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-l :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'fail-elim-l :rulename2 'eqn-float :path1 '(1 2)      ;Proof 88
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-l :path '())))
;;             (make-proof :rulename1 'fail-elim-l :rulename2 'seq-assoc :path1 '(1)      ;Proof 89
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-l :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'exi-float-eq :path1 '()      ;Proof 90
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-r :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'exi-float-l :path1 '()      ;Proof 91
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-r :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'eqn-float :path1 '()      ;Proof 92
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-r :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'eqn-float :path1 '(1 2)      ;Proof 93
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'seq-assoc :path1 '()      ;Proof 94
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-r :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'seq-assoc :path1 '(1)      ;Proof 95
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-l :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'exi-float-eq :rulename2 'exi-float-r :path1 '()      ;Proof 96
;;                         :rowsep "large" :colsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-eq :path '(2)) (make-rewrite :rulename 'exi-swap :path '())))
;;             (make-proof :rulename1 'exi-float-eq :rulename2 'eqn-float :path1 '(1 2)      ;Proof 97
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-eq :path '())))
;;             (make-proof :rulename1 'exi-float-eq :rulename2 'seq-assoc :path1 '(1)      ;Proof 98
;;                         :rowsep "large" :colsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-l :path '()) (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-eq :path '())))
;;             (make-proof :rulename1 'exi-swap :rulename2 'exi-float-eq :path1 '(1 2)      ;Proof 99
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-eq :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-swap :path '())))
;;             (make-proof :rulename1 'exi-float-l :rulename2 'exi-float-r :path1 '()      ;Proof 100
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-l :path '(2)) (make-rewrite :rulename 'exi-swap :path '())))
;;             (make-proof :rulename1 'exi-float-l :rulename2 'eqn-float :path1 '(1 2)      ;Proof 101
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-l :path '())))
;;             (make-proof :rulename1 'exi-float-l :rulename2 'seq-assoc :path1 '(1)      ;Proof 102
;;                         :rowsep "large" :colsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-l :path '()) (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-l :path '())))
;;             (make-proof :rulename1 'exi-swap :rulename2 'exi-float-l :path1 '(1)      ;Proof 103
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-l :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-swap :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'eqn-float :path1 '()      ;Proof 104
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'eqn-float :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-r :path '(2)) (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'eqn-float :path1 '(1 2)      ;Proof 105
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'seq-assoc :path1 '()      ;Proof 106
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-r :path '(2)) (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'seq-assoc :path1 '(1)      ;Proof 107
;;                         :rowsep "large" :colsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-l :path '()) (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-l :path '(2)) (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'exi-swap :rulename2 'exi-float-r :path1 '(2)      ;Proof 108
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'exi-swap :path '())))
;;             (make-proof :rulename1 'eqn-float :rulename2 'eqn-float :path1 '(1 2)      ;Proof 109
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'eqn-float :path '())))
;;             (make-proof :rulename1 'eqn-float :rulename2 'seq-assoc :path1 '(1)      ;Proof 110
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '()) (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'eqn-float :path '())))
;;             (make-proof :rulename1 'seq-assoc :rulename2 'eqn-float :path1 '(1 2)      ;Proof 111
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'eqn-float :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'seq-assoc :path '())))
;;             (make-proof :rulename1 'seq-assoc :rulename2 'seq-assoc :path1 '(1)      ;Proof 112
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '()) (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'seq-assoc :path '())))
;;             (make-proof :rulename1 'exi-swap :rulename2 'exi-swap :path1 '(2)      ;Proof 113
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-swap :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-swap :path '())))
;;             (make-proof :rulename1 'choose-l :rulename2 'one-choice :path1 '(1)      ;Proof 114
;;                         :rewrites1 (list 'X (make-rewrite :rulename 'one-choice :path '()))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'choose-l :path '())))
;;             (make-proof :rulename1 'choose-r :rulename2 'choose-l :path1 '()      ;Proof 115
;;                         :rowsep "normal"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'choose-r :rulename2 'choose-assoc :path1 '(1)      ;Proof 116
;;                         :rowsep "normal"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'choose-r :path '())))
;;             (make-proof :rulename1 'choose-l :rulename2 'choose-assoc :path1 '()      ;Proof 117
;;                         :rowsep "normal"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'choose-l :path '(2))))
;;             (make-proof :rulename1 'choose-l :rulename2 'choose-assoc :path1 '(1)      ;Proof 118
;;                         :rowsep "normal"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'choose-r :path '(2))))
;;             (make-proof :rulename1 'choose-assoc :rulename2 'choose-assoc :path1 '(1)      ;Proof 119
;;                         :rowsep "large" :colsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'choose-assoc :path '()) (make-rewrite :rulename 'choose-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'choose-assoc :path '())))))

;; (setq the-proofs                ;79 proofs
;;       (list (make-proof :rulename1 'app-gt :rulename2 'app-gt-fail :path1 '()      ;Proof 1
;;                         :impossible "Rule \\rulename{app-gt} can apply to the common term only if $|k1 > k2|$,
;;                                      but rule \\rulename{app-gt-fail} can apply to the common term only if $|k1 <= k2|$."
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-lit :rulename2 'seq-swap :path1 '()      ;Proof 2
;; 			:rowsep "scriptsize"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-lit :path '(2))))
;;             (make-proof :rulename1 'u-lit :rulename2 'fail-elim-r :path1 '()      ;Proof 3
;; 			:rowsep "scriptsize"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-lit :rulename2 'exi-float-r :path1 '()      ;Proof 4
;; 			:rowsep "scriptsize"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-lit :path '(2))))
;;             (make-proof :rulename1 'u-lit :rulename2 'seq-assoc :path1 '(1)      ;Proof 5
;; 			:rowsep "scriptsize"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-lit :path '())))
;;             (make-proof :rulename1 'u-tup :rulename2 'seq-swap :path1 '()      ;Proof 6
;; 			:rowsep "large" :colsep "normal" :flip-diagram t
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-swap :path '((high-0-to-n-1 2))) (make-rewrite :rulename 'seq-swap :path '((low-0-to-n-1 2)) :ellipsis t))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-tup :path '(2))))
;;             (make-proof :rulename1 'u-tup :rulename2 'fail-elim-r :path1 '()      ;Proof 7
;; 			:rowsep "scriptsize" :colsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-r :path '((high-0-to-n-1 2))) (make-rewrite :rulename 'fail-elim-r :path '((low-0-to-n-1 2)) :ellipsis t))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-tup :rulename2 'exi-float-r :path1 '()      ;Proof 8
;; 			:rowsep "scriptsize"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '((high-0-to-n-1 2))) (make-rewrite :rulename 'exi-float-r :path '((low-0-to-n-1 2)) :ellipsis t))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-tup :path '(2))))
;;             (make-proof :rulename1 'u-tup :rulename2 'seq-assoc :path1 '(1)      ;Proof 9
;; 			:rowsep "scriptsize"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '((low-0-to-n-1 2))) (make-rewrite :rulename 'seq-assoc :path '((high-0-to-n-1 2)) :ellipsis t))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-tup :path '())))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'u-fail-d-op :path1 '()      ;Proof 10
;;                         :rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'seq-swap :path1 '()      ;Proof 11
;;                         :rowsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-op-d :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'fail-elim-r :path1 '()      ;Proof 12
;; 			:rowsep "normal"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'exi-float-r :path1 '()      ;Proof 13
;;                         :rowsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-op-d :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'seq-assoc :path1 '(1)      ;Proof 14
;; 			:rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-op-d :path '())))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'seq-swap :path1 '()      ;Proof 15
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-d-op :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'fail-elim-r :path1 '()      ;Proof 16
;;                         :rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'exi-float-r :path1 '()      ;Proof 17
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-d-op :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'seq-assoc :path1 '(1)      ;Proof 18
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-d-op :path '())))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'seq-swap :path1 '()      ;Proof 19
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-tup-k :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'fail-elim-r :path1 '()      ;Proof 20
;;                         :rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'exi-float-r :path1 '()      ;Proof 21
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-tup-k :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'seq-assoc :path1 '(1)      ;Proof 22
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-tup-k :path '())))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'seq-swap :path1 '()      ;Proof 23
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-k-tup :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'fail-elim-r :path1 '()      ;Proof 24
;;                         :rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'exi-float-r :path1 '()      ;Proof 25
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-k-tup :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'seq-assoc :path1 '(1)      ;Proof 26
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-k-tup :path '())))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'seq-swap :path1 '()      ;Proof 27
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'hnf-swap :path '(2))))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'fail-elim-r :path1 '()      ;Proof 28
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-r :path '()))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'exi-float-r :path1 '()      ;Proof 29
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'hnf-swap :path '(2))))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'seq-assoc :path1 '(1)      ;Proof 30
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'hnf-swap :path '())))
;;             (make-proof :rulename1 'var-swap :rulename2 'seq-swap :path1 '()      ;Proof 31
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'var-swap :path '(2))))
;;             (make-proof :rulename1 'var-swap :rulename2 'seq-swap :path1 '(2)      ;Proof 32
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'var-swap :path '())))
;;             (make-proof :rulename1 'var-swap :rulename2 'fail-elim-r :path1 '()      ;Proof 33
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-r :path '()))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'var-swap :rulename2 'exi-float-r :path1 '()      ;Proof 34
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'var-swap :path '(2))))
;;             (make-proof :rulename1 'var-swap :rulename2 'seq-assoc :path1 '(1)      ;Proof 35
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'var-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'seq-swap :path1 '(2)      ;Proof 36
;;                         :rowsep "large" :flip-diagram t
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-swap :path '(2)) (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'seq-swap :rulename2 'val-elim :path1 '()      ;Proof 37
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'val-elim :path '(2)))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'seq-swap :rulename2 'fail-elim-eq :path1 '()      ;Proof 38
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-eq :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '()))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'seq-swap :rulename2 'fail-elim-l :path1 '()      ;Proof 39
;;                         :rowsep "normal" :colsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '()))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'seq-swap :path1 '(2)      ;Proof 40
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-r :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-r :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'exi-float-eq :path1 '()      ;Proof 41
;;                         :rowsep "large" :colsep "normal" :flip-diagram t
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-eq :path '(2)) (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'seq-swap :path '(2))))
;;             (make-proof :rulename1 'seq-swap :rulename2 'exi-float-l :path1 '()      ;Proof 42
;;                         :rowsep "large" :colsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-l :path '(2)) (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'seq-swap :path '(2))))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'seq-swap :path1 '(2)      ;Proof 43
;;                         :rowsep "large" :colsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '()) (make-rewrite :rulename 'seq-swap :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-r :path '(2)) (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'eqn-float :path1 '()      ;Proof 44
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'eqn-float :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'seq-swap :path '(2)) (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'seq-assoc :path1 '()      ;Proof 45
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'seq-swap :path '(2)) (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'seq-assoc :path1 '(1)      ;Proof 46
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '()) (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'seq-assoc :path '(2)) (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'val-elim :rulename2 'fail-elim-r :path1 '()      ;Proof 47
;;                         :rowsep "normal"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'val-elim :rulename2 'exi-float-r :path1 '()      ;Proof 48
;;                         :rowsep "normal"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'val-elim :path '(2))))
;;             (make-proof :rulename1 'val-elim :rulename2 'seq-assoc :path1 '(1)      ;Proof 49
;;                         :rowsep "normal"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'val-elim :path '())))
;;             (make-proof :rulename1 'exi-elim :rulename2 'eqn-elim :path1 '()      ;Proof 50
;;                         :rowsep "large"
;;                         :impossible "Rule \\rulename{EQN-ELIM} can apply to the common term only if $|x| \\equiv |x'|$,
;;                                      but rule \rulename{exi-elim} can apply to the common term only if $|x| \\not\\equiv |x'|$."
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'exi-elim :rulename2 'exi-swap :path1 '()      ;Proof 51
;;                         :rowsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-swap :path '()) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'exi-elim :rulename2 'exi-swap :path1 '(2)      ;Proof 52
;;                         :rowsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-swap :path '()) (make-rewrite :rulename 'exi-elim :path '(2))))
;;             (make-proof :rulename1 'eqn-elim :rulename2 'exi-swap :path1 '(2)      ;Proof 53
;;                         :rowsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-swap :path '()) (make-rewrite :rulename 'eqn-elim :path '(2))))
;;             (make-proof :rulename1 'fail-elim-eq :rulename2 'fail-elim-r :path1 '()      ;Proof 54
;;                         :rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'fail-elim-eq :rulename2 'exi-float-r :path1 '()      ;Proof 55
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-eq :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'fail-elim-eq :rulename2 'seq-assoc :path1 '(1)      ;Proof 56
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-eq :path '())))
;;             (make-proof :rulename1 'fail-elim-l :rulename2 'fail-elim-r :path1 '()      ;Proof 57
;;                         :rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'fail-elim-l :rulename2 'exi-float-r :path1 '()      ;Proof 58
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-l :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'fail-elim-l :rulename2 'seq-assoc :path1 '(1)      ;Proof 59
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-l :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'exi-float-eq :path1 '()      ;Proof 60
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-r :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'exi-float-l :path1 '()      ;Proof 61
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-r :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'eqn-float :path1 '()      ;Proof 62
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-r :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'seq-assoc :path1 '()      ;Proof 63
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-r :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'seq-assoc :path1 '(1)      ;Proof 64
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-l :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'exi-float-eq :rulename2 'exi-float-r :path1 '()      ;Proof 65
;;                         :rowsep "large" :colsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-eq :path '(2)) (make-rewrite :rulename 'exi-swap :path '())))
;;             (make-proof :rulename1 'exi-float-eq :rulename2 'seq-assoc :path1 '(1)      ;Proof 66
;;                         :rowsep "large" :colsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-l :path '()) (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-eq :path '())))
;;             (make-proof :rulename1 'exi-float-l :rulename2 'exi-float-r :path1 '()      ;Proof 67
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-l :path '(2)) (make-rewrite :rulename 'exi-swap :path '())))
;;             (make-proof :rulename1 'exi-float-l :rulename2 'seq-assoc :path1 '(1)      ;Proof 68
;;                         :rowsep "large" :colsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-l :path '()) (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-l :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'eqn-float :path1 '()      ;Proof 69
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'eqn-float :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-r :path '(2)) (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'seq-assoc :path1 '()      ;Proof 70
;;                         :rowsep "large" :colsep "large" :flip-diagram t
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-r :path '(2)) (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'seq-assoc :path1 '(1)      ;Proof 71
;;                         :rowsep "large" :colsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-l :path '()) (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-l :path '(2)) (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'eqn-float :rulename2 'seq-assoc :path1 '(1)      ;Proof 72
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '()) (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'eqn-float :path '())))
;;             (make-proof :rulename1 'seq-assoc :rulename2 'seq-assoc :path1 '(1)      ;Proof 73
;;                         :rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '()) (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'seq-assoc :path '())))
;;             (make-proof :rulename1 'exi-swap :rulename2 'exi-swap :path1 '(2)      ;Proof 74
;;                         :rowsep "normal"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-swap :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-swap :path '())))
;;             (make-proof :rulename1 'choose-r :rulename2 'choose-l :path1 '()      ;Proof 75
;;                         :rowsep "normal"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'choose-r :rulename2 'choose-assoc :path1 '(1)      ;Proof 76
;;                         :rowsep "normal"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'choose-r :path '())))
;;             (make-proof :rulename1 'choose-l :rulename2 'choose-assoc :path1 '()      ;Proof 77
;;                         :rowsep "normal"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'choose-l :path '(2))))
;;             (make-proof :rulename1 'choose-l :rulename2 'choose-assoc :path1 '(1)      ;Proof 78
;;                         :rowsep "normal"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'choose-r :path '(2))))
;;             (make-proof :rulename1 'choose-assoc :rulename2 'choose-assoc :path1 '(1)      ;Proof 79
;;                         :rowsep "large" :colsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'choose-assoc :path '()) (make-rewrite :rulename 'choose-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'choose-assoc :path '())))))

;; (setq the-proofs                ;78 proofs
;;       (list (make-proof :rulename1 'u-lit :rulename2 'seq-swap :path1 '()      ;Proof 1
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-lit :path '(2))))
;;             (make-proof :rulename1 'u-lit :rulename2 'fail-elim-r :path1 '()      ;Proof 2
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-lit :rulename2 'exi-float-r :path1 '()      ;Proof 3
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-lit :path '(2))))
;;             (make-proof :rulename1 'u-lit :rulename2 'seq-assoc :path1 '(1)      ;Proof 4
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-lit :path '())))
;;             (make-proof :rulename1 'u-tup :rulename2 'seq-swap :path1 '()      ;Proof 5
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-swap :path '((high-0-to-n-1 2))) (make-rewrite :rulename 'seq-swap :path '((low-0-to-n-1 2)) :ellipsis t))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-tup :path '(2))))
;;             (make-proof :rulename1 'u-tup :rulename2 'fail-elim-r :path1 '()      ;Proof 6
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-r :path '((high-0-to-n-1 2))) (make-rewrite :rulename 'fail-elim-r :path '((low-0-to-n-1 2)) :ellipsis t))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-tup :rulename2 'exi-float-r :path1 '()      ;Proof 7
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '((high-0-to-n-1 2))) (make-rewrite :rulename 'exi-float-r :path '((low-0-to-n-1 2)) :ellipsis t))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-tup :path '(2))))
;;             (make-proof :rulename1 'u-tup :rulename2 'seq-assoc :path1 '(1)      ;Proof 8
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '((low-0-to-n-1 2))) (make-rewrite :rulename 'seq-assoc :path '((high-0-to-n-1 2)) :ellipsis t))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-tup :path '())))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'u-fail-d-op :path1 '()      ;Proof 9
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'seq-swap :path1 '()      ;Proof 10
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-op-d :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'fail-elim-r :path1 '()      ;Proof 11
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'exi-float-r :path1 '()      ;Proof 12
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-op-d :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'u-fail-op-d :rulename2 'seq-assoc :path1 '(1)      ;Proof 13
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-op-d :path '())))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'seq-swap :path1 '()      ;Proof 14
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-d-op :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'fail-elim-r :path1 '()      ;Proof 15
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'exi-float-r :path1 '()      ;Proof 16
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-d-op :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'u-fail-d-op :rulename2 'seq-assoc :path1 '(1)      ;Proof 17
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-d-op :path '())))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'seq-swap :path1 '()      ;Proof 18
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-tup-k :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'fail-elim-r :path1 '()      ;Proof 19
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'exi-float-r :path1 '()      ;Proof 20
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-tup-k :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'u-fail-tup-k :rulename2 'seq-assoc :path1 '(1)      ;Proof 21
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-tup-k :path '())))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'seq-swap :path1 '()      ;Proof 22
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-k-tup :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'fail-elim-r :path1 '()      ;Proof 23
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'exi-float-r :path1 '()      ;Proof 24
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-k-tup :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'u-fail-k-tup :rulename2 'seq-assoc :path1 '(1)      ;Proof 25
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'u-fail-k-tup :path '())))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'seq-swap :path1 '()      ;Proof 26
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'hnf-swap :path '(2))))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'fail-elim-r :path1 '()      ;Proof 27
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-r :path '()))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'exi-float-r :path1 '()      ;Proof 28
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'hnf-swap :path '(2))))
;;             (make-proof :rulename1 'hnf-swap :rulename2 'seq-assoc :path1 '(1)      ;Proof 29
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'hnf-swap :path '())))
;;             (make-proof :rulename1 'var-swap :rulename2 'seq-swap :path1 '()      ;Proof 30
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'var-swap :path '(2))))
;;             (make-proof :rulename1 'var-swap :rulename2 'seq-swap :path1 '(2)      ;Proof 31
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'var-swap :path '())))
;;             (make-proof :rulename1 'var-swap :rulename2 'fail-elim-r :path1 '()      ;Proof 32
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-r :path '()))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'var-swap :rulename2 'exi-float-r :path1 '()      ;Proof 33
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'var-swap :path '(2))))
;;             (make-proof :rulename1 'var-swap :rulename2 'seq-assoc :path1 '(1)      ;Proof 34
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'var-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'seq-swap :path1 '(2)      ;Proof 35
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-swap :path '(2)) (make-rewrite :rulename 'seq-swap :path '()))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'seq-swap :rulename2 'val-elim :path1 '()      ;Proof 36
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'val-elim :path '(2)))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'seq-swap :rulename2 'fail-elim-eq :path1 '()      ;Proof 37
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-eq :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '()))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'seq-swap :rulename2 'fail-elim-l :path1 '()      ;Proof 38
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '()))
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'seq-swap :path1 '(2)      ;Proof 39
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-r :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-r :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'exi-float-eq :path1 '()      ;Proof 40
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-eq :path '(2)) (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'seq-swap :path '(2))))
;;             (make-proof :rulename1 'seq-swap :rulename2 'exi-float-l :path1 '()      ;Proof 41
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-l :path '(2)) (make-rewrite :rulename 'exi-float-r :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'seq-swap :path '(2))))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'seq-swap :path1 '(2)      ;Proof 42
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '()) (make-rewrite :rulename 'seq-swap :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-r :path '(2)) (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'eqn-float :path1 '()      ;Proof 43
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'eqn-float :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'seq-swap :path '(2)) (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'seq-assoc :path1 '()      ;Proof 44
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'seq-swap :path '(2)) (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'seq-swap :rulename2 'seq-assoc :path1 '(1)      ;Proof 45
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '()) (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'seq-assoc :path '(2)) (make-rewrite :rulename 'seq-swap :path '())))
;;             (make-proof :rulename1 'val-elim :rulename2 'fail-elim-r :path1 '()      ;Proof 46
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'val-elim :rulename2 'exi-float-r :path1 '()      ;Proof 47
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'val-elim :path '(2))))
;;             (make-proof :rulename1 'val-elim :rulename2 'seq-assoc :path1 '(1)      ;Proof 48
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'val-elim :path '())))
;;             (make-proof :rulename1 'exi-elim :rulename2 'eqn-elim :path1 '()      ;Proof 49
;; 			:rowsep "large"
;; 			:impossible "Rule \\rulename{EQN-ELIM} can apply to the common term only if $|x| \\equiv |x'|$,
;;                                      but rule \\rulename{exi-elim} can apply to the common term only if $|x| \\not\\equiv |x'|$."
;;                         :rewrites1 (list (make-rewrite :rulename 'eqn-elim :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'exi-elim :rulename2 'exi-swap :path1 '()      ;Proof 50
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-swap :path '()) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'exi-elim :rulename2 'exi-swap :path1 '(2)      ;Proof 51
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-swap :path '()) (make-rewrite :rulename 'exi-elim :path '(2))))
;;             (make-proof :rulename1 'eqn-elim :rulename2 'exi-swap :path1 '(2)      ;Proof 52
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-swap :path '()) (make-rewrite :rulename 'eqn-elim :path '(2))))
;;             (make-proof :rulename1 'fail-elim-eq :rulename2 'fail-elim-r :path1 '()      ;Proof 53
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'fail-elim-eq :rulename2 'exi-float-r :path1 '()      ;Proof 54
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-eq :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'fail-elim-eq :rulename2 'seq-assoc :path1 '(1)      ;Proof 55
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-eq :path '())))
;;             (make-proof :rulename1 'fail-elim-l :rulename2 'fail-elim-r :path1 '()      ;Proof 56
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'fail-elim-l :rulename2 'exi-float-r :path1 '()      ;Proof 57
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-l :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'fail-elim-l :rulename2 'seq-assoc :path1 '(1)      ;Proof 58
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-l :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'exi-float-eq :path1 '()      ;Proof 59
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-r :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'exi-float-l :path1 '()      ;Proof 60
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-r :path '(2)) (make-rewrite :rulename 'exi-elim :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'eqn-float :path1 '()      ;Proof 61
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-r :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'seq-assoc :path1 '()      ;Proof 62
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-r :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'fail-elim-r :rulename2 'seq-assoc :path1 '(1)      ;Proof 63
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'fail-elim-l :path '()))
;;                         :rewrites2 (list (make-rewrite :rulename 'fail-elim-l :path '(2)) (make-rewrite :rulename 'fail-elim-r :path '())))
;;             (make-proof :rulename1 'exi-float-eq :rulename2 'exi-float-r :path1 '()      ;Proof 64
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-eq :path '(2)) (make-rewrite :rulename 'exi-swap :path '())))
;;             (make-proof :rulename1 'exi-float-eq :rulename2 'seq-assoc :path1 '(1)      ;Proof 65
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-l :path '()) (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-eq :path '())))
;;             (make-proof :rulename1 'exi-float-l :rulename2 'exi-float-r :path1 '()      ;Proof 66
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-r :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-l :path '(2)) (make-rewrite :rulename 'exi-swap :path '())))
;;             (make-proof :rulename1 'exi-float-l :rulename2 'seq-assoc :path1 '(1)      ;Proof 67
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-l :path '()) (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-l :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'eqn-float :path1 '()      ;Proof 68
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'eqn-float :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-r :path '(2)) (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'seq-assoc :path1 '()      ;Proof 69
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-r :path '(2)) (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'exi-float-r :rulename2 'seq-assoc :path1 '(1)      ;Proof 70
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-float-l :path '()) (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-float-l :path '(2)) (make-rewrite :rulename 'exi-float-r :path '())))
;;             (make-proof :rulename1 'eqn-float :rulename2 'seq-assoc :path1 '(1)      ;Proof 71
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '()) (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list 'X (make-rewrite :rulename 'eqn-float :path '())))
;;             (make-proof :rulename1 'seq-assoc :rulename2 'seq-assoc :path1 '(1)      ;Proof 72
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'seq-assoc :path '()) (make-rewrite :rulename 'seq-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'seq-assoc :path '())))
;;             (make-proof :rulename1 'exi-swap :rulename2 'exi-swap :path1 '(2)      ;Proof 73
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'exi-swap :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'exi-swap :path '())))
;;             (make-proof :rulename1 'choose-r :rulename2 'choose-l :path1 '()      ;Proof 74
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list))
;;             (make-proof :rulename1 'choose-r :rulename2 'choose-assoc :path1 '(1)      ;Proof 75
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'choose-r :path '())))
;;             (make-proof :rulename1 'choose-l :rulename2 'choose-assoc :path1 '()      ;Proof 76
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'choose-l :path '(2))))
;;             (make-proof :rulename1 'choose-l :rulename2 'choose-assoc :path1 '(1)      ;Proof 77
;; 			:rowsep "large"
;;                         :rewrites1 (list)
;;                         :rewrites2 (list (make-rewrite :rulename 'choose-r :path '(2))))
;;             (make-proof :rulename1 'choose-assoc :rulename2 'choose-assoc :path1 '(1)      ;Proof 78
;; 			:rowsep "large"
;;                         :rewrites1 (list (make-rewrite :rulename 'choose-assoc :path '()) (make-rewrite :rulename 'choose-assoc :path '(2)))
;;                         :rewrites2 (list (make-rewrite :rulename 'choose-assoc :path '())))))

(defun canonical-nt (nt)
  (intern (strip-decorations (symbol-name nt))))

(defun strip-decorations (str)
  (let ((n (length str)))
    (cond ((string= str "vn") "v")   ; Support the special n-tuple hack
	  ((or (string= str "y")     ; Special hack for variables:
	       (string= str "z")     ;  "y" and "z" and "f" and "g"
	       (string= str "f")     ;  are considered to be "decorated"
	       (string= str "g"))    ;  versions of "x".
	   "x")
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

;;; Compare canonical nonterminals: Is c1 a strict subtype of c2?
(defun grammar-subtype (c1 c2)
  (some #'(lambda (alt2) (and (atom alt2)
			      (or (eq c1 alt2) (grammar-subtype c1 alt2))))
	(nt-lookup c2)))

(defstruct joinresult N sigma1 sigma2)

;;; On success, return 3-list (N, sigma1, sigma2) such that N = unify(t1, t2), sigma1(t1)=N, and sigma2(t2)=N.
;;; (When matching metavariables, prefers metavariables from t2 for use in the joined term N.)
;;; On failure, return nil.
;;; Cleans up renamings of variables in the substitutions.
(defun joinable (t1 t2)
  (let ((res (subjoinable t1 t2)))
    (and res
	 (let ((sigma1
		(remove-if #'(lambda (entry) (eq (car entry) (cdr entry)))
			   (mapcar #'(lambda (entry) (cons (car entry)
							   (sublis (joinresult-sigma2 res) (cdr entry))))
				   (joinresult-sigma1 res))))
	       (sigma2
		(mapcar #'(lambda (entry) (if (atom (cdr entry))
					      entry
					    (cons (car entry)
						  (sublis (joinresult-sigma1 res) (cdr entry)))))
			(joinresult-sigma2 res))))
	   ;; (print (list 'JOINABLE sigma1 sigma2 (joinresult-N res) (sublis sigma1 (sublis sigma2 (joinresult-N res)))))
	   (make-joinresult :N (sublis sigma1 (sublis sigma2 (joinresult-N res)))
			    :sigma1 sigma1
			    :sigma2 sigma2)))))

;;; On success, return 3-list (N, sigma1, sigma2) such that N = unify(t1, t2), sigma1(t1)=N, and sigma2(t2)=N.
;;; (When matching metavariables, prefers metavariables from t2 for use in the joined term N.)
;;; On failure, return nil.
(defun subjoinable (t1 t2)  
  (unless t1 (error "subjoinable: null term t1"))
  (unless t2 (error "subjoinable: null term t2"))
  (cond ((and (atom t1) (atom t2))
	 (let ((c1 (canonical-nt t1))
	       (c2 (canonical-nt t2)))
	   (cond ((eq c1 c2)
		  (make-joinresult :N t2 :sigma1 (list (cons t1 t2)) :sigma2 '()))
		 ((grammar-subtype c1 c2)
		  (make-joinresult :N t1 :sigma1 '() :sigma2 (list (cons t2 t1))))
		 ((grammar-subtype c2 c1)
		  (make-joinresult :N t2 :sigma1 (list (cons t1 t2)) :sigma2 '()))
		 (t nil))))
        ((atom t1)
         (let ((sj (find-if #'identity (mapcar #'(lambda (opt1) (subjoinable opt1 t2)) (nt-lookup t1)))))
           (and sj (make-joinresult :N t2 :sigma1 (list (cons t1 t2)) :sigma2 '()))))
        ((atom t2)
         (let ((sj (find-if #'identity (mapcar #'(lambda (opt2) (subjoinable t1 opt2)) (nt-lookup t2)))))
           (and sj (make-joinresult :N t1 :sigma1 '() :sigma2 (list (cons t2 t1))))))
        ((eq (first t1) (first t2))
         (cond ((eq (first t1) 'quote)
                (and (eq (second t1) (second t2))
                     (make-joinresult :N t1 :sigma1 '() :sigma2 '())))
	       (t (let ((sjs (cl-mapcar #'subjoinable (rest t1) (rest t2))))
                    (and (every #'identity sjs)
                         (make-joinresult :N (cons (first t1) (mapcar #'joinresult-N sjs))
					  :sigma1 (apply #'append (mapcar #'joinresult-sigma1 sjs))
					  :sigma2 (apply #'append (mapcar #'joinresult-sigma2 sjs))))))))
	(t nil)))

(defun do-every-replace-or-subst (term)
  (cond ((atom term) term)
	((eq (first term) 'quote) term)
	((eq (first term) 'replace)
	 (do-one-replace (second term) (third term) (fourth term)))
	((eq (first term) 'subst)
	 (do-one-subst (second term) (third term) (fourth term)))
	(t (cons (first term) (mapcar #'do-every-replace-or-subst (rest term))))))

(defun do-one-replace (term x y)
  (unless (atom x) (error "do-one-replace: non-atomic replacement variable %s" x))
  (cond ((atom term) (if (eq term x) y (list 'replace term x y)))
	((eq (first term) 'quote) term)
	((memq (first term) '(replace subst))
	 (error "do-one-replace: nested %s" term))
	(t (cons (first term) (mapcar #'(lambda (tm) (do-one-replace tm x y)) (rest term))))))

(defun do-one-subst (term x y)
  (unless (atom x) (error "do-one-subst: non-atomic replacement variable %s" x))
  (cond ((atom term) (if (eq term x) y (list 'subst term x y)))
	((eq (first term) 'quote) term)
	((memq (first term) '(replace subst))
	 (error "do-one-subst: nested %s" term))
	((memq (first term) '(lam exists))
	 (cond ((eq (second term) x) term)
	       (t (list 'subst term x y))))
	(t (cons (first term) (mapcar #'(lambda (tm) (do-one-subst tm x y)) (rest term))))))

(defstruct critpair rule1 rule2 path1 sigma1 sigma2 term term1 term2 cond1 cond2 if1 if2 fresh1 fresh2)

(defun submatches (M rule1 rule2 path1 eqok)
  (let ((name1 (rule-name rule1)) (alpha1 (rule-lhs rule1)) (beta1 (rule-rhs rule1)) (cond1 (rule-cond rule1)) (if1 (rule-if rule1)) (fresh1 (rule-fresh rule1))
        (name2 (rule-name rule2)) (alpha2 (rule-lhs rule2)) (beta2 (rule-rhs rule2)) (cond2 (rule-cond rule2)) (if2 (rule-if rule2)) (fresh2 (rule-fresh rule2)))
    (and (not (atom M))
         (not (atom alpha2))
         (append (and eqok
                      (let ((jn (joinable M alpha2)))
                        (and jn (let ((N (joinresult-N jn))
				      (sigma1 (joinresult-sigma1 jn))
                                      (sigma2 (joinresult-sigma2 jn)))
				  ;; (print (list 'CRITPAIR name1 name2 alpha1 N sigma1 sigma2 (replace-subterm alpha1 path1 N)))
                                  (list (make-critpair :rule1 rule2
						       :rule2 rule1   ;Put rule1 second because it has the primes
						       :path1 path1
						       :sigma1 sigma2  ;Similarly swap the sigmas
						       :sigma2 sigma1
						       :term (replace-subterm alpha1 path1 N)
						       :term1 (do-every-replace-or-subst (sublis sigma1 beta1))
						       :term2 (replace-subterm (sublis sigma1 alpha1) path1 (do-every-replace-or-subst (sublis sigma2 beta2)))
						       :cond1 (and cond2 (do-every-replace-or-subst (sublis sigma2 cond2))) ;Similarly swap the conds
						       :cond2 (and cond1 (do-every-replace-or-subst (sublis sigma1 cond1)))
						       :if1 (and if2 (do-every-replace-or-subst (sublis sigma2 if2))) ;Similarly swap the ifs
						       :if2 (and if1 (do-every-replace-or-subst (sublis sigma1 if1)))
						       :fresh1 (and fresh2 (do-every-replace-or-subst (sublis sigma2 fresh2))) ;Similarly swap the freshs
						       :fresh2 (and fresh1 (do-every-replace-or-subst (sublis sigma1 fresh1)))))))))
		 (and (not (eq (first M) 'quote))
                      (do ((z2 (rest M) (rest z2))
                           (k 1 (+ k 1))
                           (matches '() (append matches (submatches (first z2) rule1 rule2 (append path1 (list k)) t))))
                          ((null z2) matches)))))))

(defun add-primes-to-rule (rule vars-to-avoid)
  (make-rule :name (rule-name rule) 
	     :lhs (add-primes (rule-lhs rule) vars-to-avoid)
	     :rhs (add-primes (rule-rhs rule) vars-to-avoid)
	     :cond (and (rule-cond rule) (add-primes (rule-cond rule) vars-to-avoid))
	     :if (and (rule-if rule) (add-primes (rule-if rule) vars-to-avoid))
	     :fresh (and (rule-fresh rule) (add-primes (rule-fresh rule) vars-to-avoid))))

(defun all-submatches (rule1 rule2 same)
  (let ((rulehat1 (add-primes-to-rule rule1 (union (term-vars (rule-lhs rule2)) (term-vars (rule-rhs rule2))) ))
	(rulehat2 (add-primes-to-rule rule2 (union (term-vars (rule-lhs rule1)) (term-vars (rule-rhs rule1))))))
    (cond (same (submatches (rule-lhs rulehat2) rulehat2 rule1 '() nil))
	  (t (append (submatches (rule-lhs rulehat2) rulehat2 rule1 '() t)
		     (submatches (rule-lhs rulehat1) rulehat1 rule2 '() nil))))))

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
				    ((eq (first item) 'low-0-to-n-1) (list))
				    ((eq (first item) 'high-0-to-n-1) (list (second item)))
				    ((eq (first item) 'low-0-to-n-2) (list))
				    ((eq (first item) 'high-0-to-n-2) 'DUMMY)
				    ((eq (first item) 'low-1-to-n) (list (second item)))
				    ((eq (first item) 'high-1-to-n) (list (second item) (second item)))
				    (t (error "Unknown path item %s" item))))
			  path)))


;;; XXX put this into effect
(defstruct rewriting beta cond if fresh)

;;; Return just the substituted beta
(defun apply-rewrite-rule (rule term path)
  (let ((cp (canonical-path path)))
    (cond ((eq cp 'DUMMY) term)
	  (t (let ((res (try-rewrite-rule rule term cp)))
	       (unless res (error "Applying rewrite rule %s to term %s / %s failed" (rule-name rule) term path))
	       (do-every-replace-or-subst (first res)))))))

;;; Returns a "rewriting" structure
(defun apply-rewrite-rule-entire (rule term path)
  (let ((cp (canonical-path path)))
    (cond ((eq cp 'DUMMY) (list term nil))
	  (t (let ((res (try-rewrite-rule rule term cp)))
	       (unless res (error "Applying rewrite rule %s to term %s / %s (with cond) failed" (rule-name rule) term path))
	       (mapcar #'do-every-replace-or-subst res))))))

;;; Returns a "rewriting" structure
(defun try-rewrite-rule (rule term path)
  (cond ((null path)
	 (let ((m (matches term (rule-lhs rule))))
	   ;; (unless m (princ (format "\nFAILED MATCH of %s to %s of %s\n" term (rule-lhs rule) (rule-name rule))))
	   (and m (make-rewriting :beta (sublis (matchresult-sigma m) (rule-rhs rule))
				  :cond (and (rule-cond rule)
					     (sublis (matchresult-sigma m) (rule-cond rule)))
				  :if (and (rule-if rule)
					     (sublis (matchresult-sigma m) (rule-if rule)))
				  :fresh (and (rule-fresh rule)
					     (sublis (matchresult-sigma m) (rule-fresh rule)))))))
	((atom term) nil)
	(t (let ((res (try-rewrite-rule rule (nth (first path) term) (rest path))))
	     (and res
		  (make-rewriting :beta (append (subseq term 0 (first path))
						(list (first res))
						(subseq term (+ (first path) 1)))
				  :cond (rewriting-cond res)
				  :if (rewriting-if res)
				  :fresh (rewriting-fresh res)))))))

(defun format-path (path)
  (cond ((null path) "\\emptypath")
	(t (format-partial-path path))))

(defun format-partial-path (path)
  (apply #'concat
	 (mapcar #'(lambda (item)
		     (cond ((numberp item) (format "\\%s" item))
			   ((atom item) (error "Unknown path item %s" item))
			   ((eq (first item) 'low-0-to-n-1) (format "\\%s^{0}" (second item)))
			   ((eq (first item) 'high-0-to-n-1) (format "\\%s^{n-1}" (second item)))
			   ((eq (first item) 'low-0-to-n-1) (format "\\%s^{0}" (second item)))
			   ((eq (first item) 'high-0-to-n-1) (format "\\%s^{n-2}" (second item)))
			   ((eq (first item) 'low-1-to-n) (format "\\%s^{1}" (second item)))
			   ((eq (first item) 'high-1-to-n) (format "\\%s^{n}" (second item)))
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
  (cond ((eq (first term) 'replace)
	 (format "tsubst %s %s %s"
		 (format-subterm (second term) t)
		 (format-subterm (third term) nil)
		 (format-subterm (fourth term) nil)))
	((eq (first term) 'subst)
	 (format "subst %s %s %s"
		 (format-subterm (second term) t)
		 (format-subterm (third term) nil)
		 (format-subterm (fourth term) nil)))
	((eq (first term) 'seq)
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
	 (format "one (%s)" (format-subterm (second term) nil)))
	((eq (first term) 'all)
	 (format "all (%s)" (format-subterm (second term) nil)))
	((eq (first term) 'split)
	 (format "split (%s) %s %s" (format-subterm (second term) nil) (third term) (fourth term)))
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

;;; Format just the expression
(defun format-if-condition (rif)
  (format-rule-condition-expression rif))

(defun format-if-condition-expand-ands (rif)
  (cond ((and (not (atom rif)) (eq (first rif 'and)))
	 (mapconcat #'format-if-condition-expand-ands (rest rif) " and "))
	(t (format-rule-condition-expression rif<))))

;;; Format just the expression
(defun format-fresh-condition (rfresh)
  (format-rule-condition-expression rfresh))

;;; (Obsolete) Format expression with a possible text prefix
(defun format-rule-condition (rc)
  (format-rule-condition-with-prefixes rc "fresh " "if "))

;;; (Obsolete) Format expression with a possible text prefix
(defun format-condition-text (rc)
  (format-rule-condition-with-prefixes rc "fresh " ""))

;;; (Obsolete) Format expression with a possible text prefix
(defun format-rule-condition-with-prefixes (rc fresh-prefix if-prefix)
  (cond ((atom rc) (error "format-rule-condition: unknown atomic condition %s" rc))
	((eq (first rc) 'fresh)
	 (unless (= (length rc) 2)  (error "format-rule-condition: wrong number of arguments %s" rc))
	 (format "\\text{%s$%s$}" fresh-prefix (format-rule-condition-expression (second rc))))
	((eq (first rc) 'if)
	 (unless (= (length rc) 2)  (error "format-rule-condition: wrong number of arguments %s" rc))
	 (format "\\text{%s$%s$}" if-prefix (format-rule-condition-expression (second rc))))
	(t (error "format-rule-condition: unknown condition %s" rc))))

(defun format-rule-condition-expression (rce)
  (cond ((atom rce) (format "|%s|" (format-nt rce)))
	((eq (first rce) 'fvs)
	 (format "\\freevars{%s}" (mapconcat #'(lambda (term) (format-rule-term term)) (rest rce) ",")))
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

(defun print-rule-line (prefix name alpha beta cond rif rfresh linebreak)
  (princ (format "\\hbox to 5em{%s\\hfill}\\hbox to 6em{\\rulename{%s}\\hfill}\\hbox to 8em{\\hss %s}\\quad$\\movesto$\\quad %s"
		 prefix name (format-rule-term alpha) (format-rule-term beta)))
  (when cond
    (princ (format "\\hfill %s" (format-rule-condition cond))))
  (princ (format "\\relax%s\n" linebreak)))

(defun print-rule-line (prefix name alpha beta cond rif rfresh linebreak)
  (princ (format "\\hbox to 5em{%s\\hfill}\\hbox to 6em{\\rulename{%s}\\hfill}\\hbox to 8em{\\hss %s}\\quad$\\movesto$\\quad %s"
		 prefix name (format-rule-term alpha) (format-rule-term beta)))
  (cond (use-if-fresh
	 (cond (rif (princ (format "\\hfill if %s" (format-if-condition rif)))
		    (when rfresh	;not sure this is ever used
		      (princ (format "; fresh %s" (format-if-condition rif)))))
	       (rfresh (princ (format "\\hfill fresh %s" (format-if-condition rif))))))
	(t (when cond
	     (princ (format "\\hfill %s" (format-rule-condition cond))))))
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

(defun print-problem-line (R name1 path1 name2 P)
  (princ (format "\\leavevmode\\null\\hskip 2em minus 1.95em {\\color{green}$|t_1| \\equiv %s \\xrnungosup{%s}{%s} |t| \\xrngosup{%s}{\\emptypath} %s \\equiv |t_2|$}%s\n"
		 (format-term R) name1 (format-path path1) name2 (format-term P) "\\par")))

(defun print-critical-pair (cp k)
  (let ((rule1 (critpair-rule1 cp))
	(rule2 (critpair-rule2 cp))
	(path1 (critpair-path1 cp))
	(sigma1 (critpair-sigma1 cp))
	(sigma2 (critpair-sigma2 cp))
	(P (critpair-term1 cp))
	(Q (critpair-term cp))
	(R (critpair-term2 cp))
	(cond1 (critpair-cond1 cp))
	(cond2 (critpair-cond2 cp)))
    (let ((name1 (rule-name rule1))
	  (name2 (rule-name rule2))
	  (alpha1 (rule-lhs rule1))
	  (alpha2 (rule-lhs rule2))
	  (beta1 (rule-rhs rule1))
	  (beta2 (rule-rhs rule2))
	  (rc1 (rule-cond rule1))
	  (rc2 (rule-cond rule2))
	  (rif1 (rule-if rule1))
	  (rif2 (rule-if rule2))
	  (rfresh1 (rule-fresh rule1))
	  (rfresh2 (rule-fresh rule2))
	  (linebreak "\\vadjust{\\penalty1000}\\hfil\\break")) ;Use \\hfil here, and \\hfill in print-rule-line
      (princ (format "\\vskip 8pt plus 16pt\\noindent\n"))
      (let ((weirdtext (cond ((< k 10) "and{\\hskip0.2em}rule")
			     ((< k 100) "and{\\hskip0.5em}rule")
			     ((< k 1000) "and{\\hskip0.8em}rule")
			     (t "and{\\hskip0.1.1em}rule"))))
	(print-rule-line (format "\\rlap{(%s)}\\hphantom{%s}\\llap{Rule}" k weirdtext) name1 alpha1 beta1 rc1 rif1 rfresh1 linebreak)
	(print-rule-line weirdtext name2 alpha2 beta2 rc2 rif2 rfresh2 linebreak))
      (princ (format "have a critical pair derived from the common term {\\color{blue}$%s$}%s%s\n"
		     (format-term Q) (if (or cond1 cond2) "," "") linebreak))
      (cond ((and cond1 cond2)
	     (princ (format "assuming {\\color{blue}%s} (for \\rulename{%s}) and {\\color{blue}%s} (for \\rulename{%s}) are satisfied,%s\n"
			    (format-condition-text cond1) name1 (format-condition-text cond2) name2 linebreak)))
	    ((or cond1 cond2)
	     (princ (format "assuming the condition {\\color{blue}%s} (for \\rulename{%s}) is satisfied,%s\n"
			    (format-condition-text (or cond1 cond2)) (if cond1 name1 name2) linebreak))))
      (princ (format "using the substitutions {\\color{blue}$\\sigma_1=%s$} and {\\color{blue}$\\sigma_2=%s$}"
		     (format-sigma sigma1) (format-sigma sigma2)))
      ;; Note that punctuation has been left hanging at end of last line.
      ;; Note that name1 is on the left edge; name2 is on the top edge.
      (let ((pf (proof-lookup name1 name2 path1)))
	(cond ((null pf)
	       (princ ":\\par")
	       (print-problem-line R name1 path1 name2 P)
	       ;; (princ (format "{\\color{red}Can they be joined? $|t_1| \\xrngosup{%s}{\\emptypath} \\bigl(|t'|\\bigr) \\xrnungosup{%s}{%s} |t_2|$.%s}\n%s\n%s\n%s\n"
	       ;; 		      name2 name1 (format-path path1) linebreak linebreak linebreak linebreak))
	       (princ (format "{\\noindent\\color{red}Can they be joined?%s}\n%s\n%s\n%s\n"
			      linebreak linebreak linebreak linebreak))
	       )
	      ((and (eq (first (proof-rewrites1 pf)) 'X)
		    (eq (first (proof-rewrites2 pf)) 'X))
	       (princ ":\\par")
	       (print-problem-line R name1 path1 name2 P)
	       ;; (princ (format "{\\color{purple}Can they be joined? $|t_1| %s \\bigl(|t'|\\bigr) %s |t_2|$.%s}\n%s\n%s\n%s\n"
	       ;; 		      (format-rewrites (rest (proof-rewrites1 pf)) nil nil) (format-rewrites (rest (proof-rewrites2 pf)) t nil) linebreak linebreak linebreak linebreak))
	       (princ (format "\\noindent{\\color{purple}Can they be joined?%s}\n%s\n%s\n%s\n"
			      linebreak linebreak linebreak linebreak))
	       )
	      ((or (eq (first (proof-rewrites1 pf)) 'X)
		   (eq (first (proof-rewrites2 pf)) 'X))
	       (error "Proof has just one 'X entry"))
	      (t (let* ((assumptions (list (list name1 cond1) (list name2 cond2)))
			(ca-result (contradictory-assumptions assumptions)))
		   (cond (ca-result 
			  (princ (format ".%s\nBut %s, so this critical pair cannot occur in practice." linebreak ca-result)))
			 (t (cond ((proof-cond pf) (princ (format ". For the case when %s is true:\\par\n" (format-text-condition (proof-cond pf)))))
				  (t (princ ":\\par\n")))
			    ;; (print-given-proof-rewrites "They can be joined" (proof-rewrites1 pf) R (proof-rewrites2 pf) P)
			    (cond ((proof-flip-diagram pf)
				   ;; Note: rowsep and colsep should NOT be flipped.
				   (print-tikzcd-diagram R Q P name2 name1 '() path1 (proof-id2 pf) (proof-id1 pf) (proof-extra2 pf) (proof-extra1 pf)
							 (proof-rowsep pf) (proof-colsep pf) (proof-rewrites2 pf) (proof-rewrites1 pf) (- k)))
				  (t (print-tikzcd-diagram P Q R name1 name2 path1 '() (proof-id1 pf) (proof-id2 pf) (proof-extra1 pf) (proof-extra2 pf)
							   (proof-rowsep pf) (proof-colsep pf) (proof-rewrites1 pf) (proof-rewrites2 pf) k)))
			    (let ((consequents (append (consequent-conditions R (proof-rewrites1 pf))
						       (consequent-conditions P (proof-rewrites2 pf)))))
			      (print-new-fresh-conditions (new-fresh-conditions Q consequents) consequents)
			      (print-consequent-conditions consequents assumptions))
			    (princ (verify-decreasing-diagram cp))
			    (when (proof-cond pf) (princ (format "\\par For the case when %s is false:\\par\n" (format-text-condition (proof-cond pf)))))
			    (when (or (proof-altrewrites1 pf) (proof-altrewrites2 pf))
			      (cond ((proof-flip-diagram pf)
				     ;; Note: rowsep and colsep should NOT be flipped.
				     (print-tikzcd-diagram R Q P name2 name1 '() path1 (proof-id2 pf) (proof-id1 pf) (proof-extra2 pf) (proof-extra1 pf)
							   (proof-rowsep pf) (proof-colsep pf) (proof-altrewrites2 pf) (proof-altrewrites1 pf) (- k)))
				    (t (print-tikzcd-diagram P Q R name1 name2 path1 '() (proof-id1 pf) (proof-id2 pf) (proof-extra1 pf) (proof-extra2 pf)
							     (proof-rowsep pf) (proof-colsep pf) (proof-altrewrites1 pf) (proof-altrewrites2 pf) k))))))))))
      ;; (princ (format "Therefore rules \\rulename{%s} and \\rulename{%s} have the XXX property.\\par\n" name1 name2))
      (princ "\n")
      (princ (format "\\par\n"))
      )))

(defun is-a-simple-if-not-in-fvs-condition (cond)
  (and (not (atom cond))
       (eq (first cond) 'if)
       (not (atom (second cond)))
       (eq (first (second cond)) 'not)
       (not (atom (second (second cond))))
       (eq (first (second (second cond))) 'elt)
       (atom (second (second (second cond))))
       (not (atom (third (second (second cond)))))
       (eq (first (third (second (second cond)))) 'fvs)))

(defun is-a-simple-fresh-not-in-fvs-condition (cond)
  (and (not (atom cond))
       (eq (first cond) 'fresh)
       (not (atom (second cond)))
       (eq (first (second cond)) 'not)
       (not (atom (second (second cond))))
       (eq (first (second (second cond))) 'elt)
       (atom (second (second (second cond))))
       (not (atom (third (second (second cond)))))
       (eq (first (third (second (second cond)))) 'fvs)))

(defun contradictory-assumptions (assumptions)
  (or (some #'self-contradictory-assumption assumptions)
      (some #'(lambda (assump1) (some #'(lambda (assump2) (contradictory-assumption-pair assump1 assump2))
				      assumptions))
	    assumptions)))

(defun self-contradictory-assumption (assumption)
  (let ((assump (second assumption)))
    (and (not (atom assump))
	 (eq (first assump) 'if)
	 (not (atom (second assump)))
	 (eq (first (second assump)) 'not)
	 (not (atom (second (second assump))))
	 (eq (first (second (second assump))) 'elt)
	 (atom (second (second (second assump))))
	 (not (atom (third (second (second assump)))))
	 (eq (first (third (second (second assump)))) 'fvs)
	 (member (second (second (second assump)))
		 (apply #'append (mapcar #'term-vars (rest (third (second (second assump)))))))
	 (format "the assumption %s is always false" (format-condition-text assump)))))

(defun contradictory-assumption-pair (assumption1 assumption2)
  (let ((assump1 (second assumption1))
	(assump2 (second assumption2)))
    (and (not (atom assump1))
	 (not (atom assump2))
	 (eq (first assump1) 'if)
	 (eq (first assump2) 'if)
	 (not (atom (second assump1)))
	 (not (atom (second assump2)))
	 (or (and (eq (first (second assump1)) 'not)
		  (equal (second (second assump1)) (second assump2)))
	     (and (eq (first (second assump2)) 'not)
		  (equal (second (second assump2)) (second assump1))))
	 (format "the assumptions %s and %s cannot both be true"
		 (format-condition-text assump1) (format-condition-text assump2)))))


(defun consequent-conditions-trivially-follow (consequents)
  (let ((consequent-conds (mapcar #'second consequents)))
    (and (every #'is-a-simple-if-not-in-fvs-condition consequent-conds)
	 (every #'(lambda (cond)
		    (let ((cfvars (apply #'append (mapcar #'term-vars (rest (third (second (second cond))))))))
		      (null cfvars)))
		consequent-conds))))

(defun consequent-conditions-follow (consequents assumptions)
  ;; (progn (print (cons 'RAW-CONSEQUENTS consequents)) t)
  ;; (progn (print (cons 'RAW-ASSUMPTIONS assumptions)) t)
  (let ((consequent-conds (mapcar #'second consequents))
	(assumption-conds (mapcar #'second assumptions)))
    ;; (progn (print (cons 'RAW-CONSEQUENT-CONDS consequent-conds)) t)
    ;; (progn (print (cons 'RAW-ASSUMPTION-CONDS assumption-conds)) t)
    ;; What follows is not very general (and in particular really doesn't allow for different conditions strategies),
    ;; but it suffices for our purposes (it picks up "(= k1 k2)" when it needs to, and handles sets of fvs conditions).
    (or (every #'(lambda (cond) (find-if #'(lambda (assump) (equal assump cond)) assumption-conds)) consequent-conds)
	(and (every #'is-a-simple-if-not-in-fvs-condition consequent-conds)
	     ;; (progn (print (cons 'TESTED-CONSEQUENT-CONDS consequent-conds)) t)
	     (let ((not-in-fvs-assumption-conds (remove-if-not #'is-a-simple-if-not-in-fvs-condition assumption-conds)))
	       (and not-in-fvs-assumption-conds
		    ;; (progn (print (cons 'NOT-IN-FVS-ASSUMPTION-CONDS not-in-fvs-assumption-conds)) t)
		    (every #'(lambda (cond)
			       (let ((cvar (second (second (second cond))))
				     (cfvars (apply #'append (mapcar #'term-vars (rest (third (second (second cond))))))))
				 ;; (print (list 'CVAR cvar 'CFVARS cfvars))
				 (every #'(lambda (cfvar) (some #'(lambda (assumption)
								    (let ((avar (second (second (second assumption))))
									  (afvars (apply #'append (mapcar #'term-vars (rest (third (second (second assumption))))))))
								      ;; (print (list 'AVAR avar 'AFVARS afvars))
								      (and (eq avar cvar)
									   (member cfvar afvars))))
								not-in-fvs-assumption-conds))
					cfvars)))
			   consequent-conds)))))))


;;; XXX Should this be all conditions or just if conditions??
(defun consequent-conditions (term rw)
  (cond ((null rw) '())
	(t (let ((rulename (rewrite-rulename (first rw))))
	     (let ((res (apply-rewrite-rule-entire (rule-lookup rulename) term (rewrite-path (first rw)))))
	       ;; (print (list 'CONSEQUENT-CONDITIONS term rw res))
	       (let ((more (consequent-conditions (rewriting-beta res) (rest rw))))
		 (if (rewriting-cond res)
		     (cons (list rulename (rewriting-cond res)) more)
		   more)))))))

(defun new-fresh-conditions (Q consequents)
  (let ((new-fresh-vars
	 (remove-duplicates
	  (apply #'append
		 (mapcar #'(lambda (consequent)
			     (and (is-a-simple-fresh-not-in-fvs-condition (second consequent))
				  (list (second (second (second (second consequent)))))))
			 consequents)))))
    ;; (print (list 'NEW-FRESH-VARS new-fresh-vars))
    (mapcar #'(lambda (nfv) (list 'fresh (list 'not (list 'elt nfv (cons 'fvs (term-vars Q)))))) new-fresh-vars)))

(defun print-new-fresh-conditions (new-fresh-conditions consequents)
  (when new-fresh-conditions
    (let ((fresh-texts (apply #'append
			      (mapcar #'(lambda (cd)
					  (and (eq (first (second cd)) 'fresh)
					       (list (list (first cd) (format-condition-text (second cd))))))
				      consequents)))
	  (new-fresh-texts (mapcar #'format-condition-text new-fresh-conditions)))
      ;; (print (list 'NEW-FRESH new-fresh-conditions fresh-texts new-fresh-texts))
      (princ "Alpha-conversion introduces new assumptions ")
      (dolist (ft fresh-texts)
	(princ (format "{\\color{blue}%s} (for \\rulename{%s}) and " (second ft) (first ft))))
      (princ "(implicitly) ")
      (princ (mapconcat #'(lambda (nft) (format "{\\color{blue}%s}" nft)) new-fresh-texts " and "))
      (princ ".\n"))))      
  
;;; Each consequent or assumption is a 2-list (rulename cond).
(defun print-consequent-conditions (consequents assumptions)
  (when consequents
    (let ((texts (apply #'append
			(mapcar #'(lambda (cd)
				    (and (eq (first (second cd)) 'if)
					 (list (list (first cd) (format-condition-text (second cd))))))
				consequents))))
      (princ (if (= (length texts) 1) "The condition to be proved is " "Conditions to be proved are "))
      (princ (cond ((= (length texts) 1)
		    (format "{\\color{purple}%s} (for \\rulename{%s})"
			    (second (first texts)) (first (first texts))))
		   ((= (length texts) 2)
		    (format "{\\color{purple}%s} (for \\rulename{%s}) and {\\color{purple}%s} (for \\rulename{%s})"
			    (second (first texts)) (first (first texts)) (second (second texts)) (first (second texts))))
		   (t (let ((revtexts (reverse texts)))
			(concat (mapconcat #'(lambda (text) (format "{\\color{purple}%s} (for \\rulename{%s}), " (second text) (first text)))
					   (reverse (rest revtexts))
					   "")
				(format "and {\\color{purple}%s} (for \\rulename{%s})" (second (first revtexts)) (first (first revtexts))))))))
      (princ (cond ((consequent-conditions-trivially-follow consequents)
		     (if (= (length texts) 1)
			"; this is trivially true"
		      "; these are trivially true"))
		   ((consequent-conditions-follow consequents assumptions)
		    (if (= (length texts) 1)
			"; this follows easily from the assumptions"
		      "; these follow easily from the assumptions"))
		   (t "; {\\color{red}please provide the necessary proof}")))
      (princ ".\\par\n"))))

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

(defun string-expt (str k)
  (cond ((= k 0) "")
	((oddp k) (concat str (string-expt str (- k 1))))
	(t (let ((res (string-expt str (ash k -1))))
	     (concat res res)))))

(defun rewrite-for-tikzcd (rw term)
  (cond ((eq (rewrite-rulename rw) 'TRIVIAL) term)
	(t (apply-rewrite-rule (rule-lookup (rewrite-rulename rw)) term (rewrite-path rw)))))

;;; k negative means the diagram is flipped
(defun print-tikzcd-diagram (P Q R rulename1 rulename2 path1 path2 id1 id2 extra1 extra2 rowsep colsep rw1 rw2 k)
;;   (cond ((< (length rw1) (length rw2))
;; 	 (print-least-wide-tikzcd-diagram P Q R rulename1 rulename2 path1 '() rw2 rw1 k))
;; 	(t (print-least-wide-tikzcd-diagram R Q P rulename2 rulename1 '() path1 rw2 rw1 k))))

;; (defun print-least-wide-tikzcd-diagram (P Q R rulename1 rulename2 path1 rw1 rw2 k)
  (princ "\\begin{center}\n")
  (princ "\\begin{tikzcd}\n")
  (when (or rowsep colsep)
    (princ "  [")
    (when rowsep (princ (format "row sep=%s" rowsep)))
    (when (and rowsep colsep) (princ ", "))
    (when colsep (princ (format "column sep=%s" colsep)))
    (princ "]\n"))
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
		     ;; (unless  (error "print-tikzcd-diagram: Rewrites failed to join %s" (list bottom-term right-term)))
		     ;; The next expression carefully does NOT print a LaTeX "\\" to end the grid (it would make the diagram too tall).
		     (cond ((equal bottom-term right-term)
			    (princ (format "{%s}\n" (format-rule-term bottom-term))))
			   (t (princ (format "{\\color{red}%s\\not\\equiv%s}\n" (format-rule-term bottom-term) (format-rule-term right-term))))))
		  (cond ((rewrite-ellipsis (first y))
			 (princ "{\\mydots} && "))
			(t (princ (format "{%s} && " (format-rule-term bottom-term))))))))
      (when (> j 0)
	(princ (format "  %s %s %s "
		       (string-expt "&" wd)
		       (if (= (* j 2) ht) (format (if (< k 0) "{(%s')}" "{(%s)}") (abs k)) "")
		       (string-expt "&" wd)))
	(cond ((rewrite-ellipsis (first z))
	       (princ "{\\mytikzvdots} \\\\\n"))
	      (t (princ (format "%s \\\\\n" (format-rule-term right-term))))))
      (princ (format "  %s %s %s \\\\\n"
		     (string-expt "&" wd)
		     (if (= (+ (* j 2) 1) ht) (format (if (< k 0) "{(%s')}" "{(%s)}") (abs k)) "")
		     (string-expt "&" wd))))
    (let ((right-edge (+ (* wd 2) 1))
	  (bottom-edge (+ (* ht 2) 1)))
      ;; The arrow on the left edge
      (princ (format "\\arrow[\"{\\rotatebox{270}{\\hbox{\\rulename{%s}}}}\"', \"{\\hbox{|u|${%s}$}}\" {font=\\normalsize},"
		     rulename1 (format-partial-path path1)))
      (when id1 (princ (format " \"\\rewritelabel{%s}\" very near start," id1)))
      (when extra1 (princ (format " \"{\\hbox{%s}}\" {pos=0.833, font=\\normalsize}," (format-rule-term extra1))))
      (princ (format " from=1-1, to=%s-1]\n" bottom-edge))
      ;; The arrow on the top edge
      (princ (format "\\arrow[\"\\rulename{%s}\"', \"{\\hbox{|u|${%s}$}}\" {font=\\normalsize},"
		     rulename2 (format-partial-path path2)))
      (when id2 (princ (format " \"\\rewritelabel{%s}\" very near start," id2)))
      (when extra2 (princ (format " \"{\\hbox{%s}}\" {pos=0.833, font=\\normalsize}," (format-rule-term extra2))))
      (princ (format " from=1-1, to=1-%s]\n" right-edge))
      ;; Arrows on the bottom edge
      (cond ((null rw1)
	     (princ (format "\\arrow[\"{\\equiv}\", dashed, from=%s-%s, to=%s-%s]\n"
			    bottom-edge 1 bottom-edge 3)))
	    (t (do ((z rw1 (rest z))
		    (j 0 (+ j 1)))
		   ((null z))
		 (princ (format "\\arrow[\"\\rulename{%s}\"', \"{\\hbox{|u|${%s}$}}\" {font=\\normalsize}, dashed,"
				(rewrite-rulename (first z))
				(format-partial-path (rewrite-path (first z)))))
		 (when (rewrite-id (first z))
		   (princ (format " \"\\rewritelabel{%s}\" very near start," (rewrite-id (first z)))))
		 (when (rewrite-extra (first z))
		   (princ (format " \"{\\hbox{%s}}\" {pos=0.833, font=\\normalsize}," (format-rule-term (rewrite-extra (first z))))))
		 (princ (format " from=%s-%s, to=%s-%s]\n"
				bottom-edge
				(+ (* j 2) 1)
				bottom-edge
				(+ (* j 2) 3))))))
      ;; Arrows on the right edge
      (cond ((null rw2)
	     (princ (format "\\arrow[\"\\equiv\", dashed, from=%s-%s, to=%s-%s]\n"
			    1 right-edge 3 right-edge)))
	    (t (do ((y rw2 (rest y))
		    (j 0 (+ j 1)))
		   ((null y))
		 (princ (format "\\arrow[\"{\\rotatebox{270}{\\hbox{\\rulename{%s}}}}\"', \"{\\hbox{|u|${%s}$}}\" {font=\\normalsize}, dashed,"
				(rewrite-rulename (first y))
				(format-partial-path (rewrite-path (first y)))))
		 (when (rewrite-id (first y))
		   (princ (format " \"\\rewritelabel{%s}\" very near start," (rewrite-id (first y)))))
		 (when (rewrite-extra (first y))
		   (princ (format " \"{\\hbox{%s}}\" {pos=0.833, font=\\normalsize}," (format-rule-term (rewrite-extra (first y))))))
		 (princ (format " from=%s-%s, to=%s-%s]\n"
				(+ (* j 2) 1)
				right-edge
				(+ (* j 2) 3)
				right-edge)))))
      ))
  (princ "\\end{tikzcd}\\vskip6pt\n")
  (princ "\\end{center}\n"))
  
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

(defun repair-escapes (str)
  (mapconcat #'(lambda (ch) (if (= ch ?\\) "\\\\" (string ch))) str ""))

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
	  (princ (format "%s(make-proof :rulename1 '%s :rulename2 '%s :path1 '%s%s%s     ;Proof %s"
			 prefix name1 name2
			 (if (null path1) "()" path1)
			 (if (or (null pf) (null (proof-id1 pf))) "" (format " :id1 %S" (proof-id1 pf)))
			 (if (or (null pf) (null (proof-id2 pf))) "" (format " :id2 %S" (proof-id2 pf)))
			 (if (or (null pf) (null (proof-extra1 pf))) "" (format " :extra1 %S" (proof-extra1 pf)))
			 (if (or (null pf) (null (proof-extra2 pf))) "" (format " :extra2 %S" (proof-extra2 pf)))
			 (if (or (null pf) (null (proof-cond pf))) "" (format " :cond %S" (proof-cond pf)))
			 k))
	  (cond ((null pf)
		 (princ (format "\n%s            :rewrites1 (list 'X (make-rewrite :rulename '%s :path '() :id 3))"
				prefix2 name2))
		 (princ (format "\n%s            :rewrites2 (list 'X (make-rewrite :rulename '%s :path '() :id 4))"
				prefix2 name1)))
		(t (when (or (proof-rowsep pf) (proof-colsep pf) (proof-difficult pf))
		     (princ (format "\n%s           " prefix2))
		     (when (proof-rowsep pf) (princ (format " :rowsep \"%s\"" (proof-rowsep pf))))
		     (when (proof-colsep pf) (princ (format " :colsep \"%s\"" (proof-colsep pf))))
		     (when (proof-flip-diagram pf) (princ (format " :flip-diagram t")))
		     (when (proof-difficult pf) (princ (format " :difficult \"%s\"" (repair-escapes (proof-difficult pf))))))
		   (princ (format "\n%s            :rewrites1 %s" prefix2 (format-skeleton-rewrites-list (proof-rewrites1 pf))))
		   (princ (format "\n%s            :rewrites2 %s" prefix2 (format-skeleton-rewrites-list (proof-rewrites2 pf))))
		   (when (or (proof-altrewrites1 pf) (proof-altrewrites2 pf))
		     (princ (format "\n%s            :altrewrites1 %s" prefix2 (format-skeleton-rewrites-list (proof-altrewrites1 pf))))
		     (princ (format "\n%s            :altrewrites2 %s" prefix2 (format-skeleton-rewrites-list (proof-altrewrites2 pf)))))))
	  (princ (format ")%s" (if (rest z) "\n" ""))))))
    (princ "))\n"))
  'done)

(defun format-skeleton-rewrites-list (rws)
  (cond ((null rws) "(list)")
	(t (concat "(list "
		   (mapconcat #'(lambda (rw) (cond ((eq rw 'X) "'X")
						   (t (format "(make-rewrite :rulename '%s :path '%s%s%s%s)"
							      (rewrite-rulename rw)
							      (format-list (rewrite-path rw))
							      (if (rewrite-ellipsis rw) " :ellipsis t" "")
							      (if (rewrite-id rw) (format " :id %S" (rewrite-id rw)) "")
							      (if (rewrite-extra rw) (format " :extra '%S" (rewrite-extra rw)) "")
							      (if (rewrite-avoid rw) (format " :extra '%S" (rewrite-avoid rw)) "")
							      ))))
			      rws
			      " ")
		   ")"))))
  
(setq the-partial-order
      '((< fail-elim-r u-tup)
	(< seq-assoc u-tup)
	(< seq-swap u-tup)
	(< exi-float-r eqn-float)
	(< seq-swap eqn-float)
	(< fail-elim-r seq-assoc)
	(< exi-elim seq-assoc)
	(< fail-elim-l seq-assoc)
	(< exi-float-r seq-assoc)
	(< exi-float-r seq-swap)
	(< fail-elim-l seq-swap)
	(< exi-elim seq-swap)
	(< fail-elim-r exi-float-r)
	(< fail-elim-l exi-float-r)
	(< exi-swap exi-float-r)
	(< exi-elim exi-float-r)
	(< exi-float-l exi-float-r)
	(< exi-float-l exi-float-eq)
	(< exi-elim exi-float-l)))

(defun verify-partial-order ()
  ;; Ensure every entry well-formed, refers to existing rules, and is not part of a cycle
  (every #'(lambda (cmp)
	     (unless (and (= (length cmp) 3) (eq (first cmp) '<))
	       (error "verify-partial-order: Malformed entry %s" cmp))
	     (unless (rule-lookup (second cmp))
	       (error "verify-partial-order: Nonexistent rule %s" (second cmp)))
	     (unless (rule-lookup (third cmp))
	       (error "verify-partial-order: Nonexistent rule %s" (third cmp)))
	     (when (partial-order-cycle (list (second cmp)) (third cmp))
	       (error "verify-partial-order: Cycle from %s" cmp)))
	 the-partial-order))

(defun partial-order-cycle (froms to)
  (or (member to froms)
      (some #'(lambda (cmp)
		(and (eq (second cmp) to)
		     (partial-order-cycle (cons to froms) (third cmp))))
	    the-partial-order)))

;; Verify that the proof for a critical pair has a decreasing diagram
(defun verify-decreasing-diagram (cp)
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
	  (rif1 (rule-if rule1))
	  (rif2 (rule-if rule2))
	  (rfresh1 (rule-fresh rule1))
	  (rfresh2 (rule-fresh rule2)))
      (let ((pf (proof-lookup name1 name2 path1))
	    (needproof "{\\color{red}Need a proof that this diagram is decreasing.}"))
	(let ((rw1 (proof-rewrites1 pf))
	      (rw2 (proof-rewrites2 pf)))
	  (cond ((and (null rw1) (null rw2))
		 "This is a trivial decreasing diagram.")
		((and (null rw1) (null (rest rw2)))
		 (cond ((eq (rewrite-rulename (first rw2)) name1)
			"This is a simple decreasing diagram.")
		       (t needproof)))
		((and (null (rest rw1)) (null rw2))
		 (cond ((eq (rewrite-rulename (first rw1)) name2)
			"This is a simple decreasing diagram.")
		       (t needproof)))
		((and (null (rest rw1)) (null (rest rw2)))
		 (cond ((and (eq (rewrite-rulename (first rw1)) name2)
			     (eq (rewrite-rulename (first rw2)) name1))
			"This is a simple decreasing diagram.")
		       (t needproof)))
		(t needproof)))))))


(defun print-critical-pairs-text ()
  (verify-partial-order)
  (let ((cp (all-critical-pairs)))
    (princ (format "\nS%s\n" "--------------"))
    (princ (format "The rules for \\versecalc{} have %s critical pairs, which are described here in detail.\\par\n" (length cp)))
    (do ((z cp (rest z))
	 (k 1 (+ k 1)))
	((null z))
      (print-critical-pair (first z) k))
    (princ "\nE--------------\n"))
  'done)

(setq eval-expression-print-level (setq eval-expression-print-length nil))
(setq inhibit-debugger nil)

(print-critical-pairs-text)

