# List of Fixes for the Confluence Appendix

Three more things I spotted the proof of Lemma C.17:

(1) It might be a good idea to mention the base case of the induction explicitly (n=0, in which case the consequent to be proved reduces to X \downarrow_U X, which is trivially true).

(2) In the case analysis, the case of “h = x” appears not to be addressed. It may be simply matter of first applying rule HNF-SWAP to each side before applying SUBST, but this needs to be stated.

(3) In there analysis for cases “x = y” and “x = h”, I think that it is necessary to note that after SUBST is applied to each expression, it is necessary to rewrite each expression into an equivalent expression before applying the IH:

X_{\bar{u}}[x=y; \bar{u’{y/x} = v’{y/x}}    

can be rewritten as    

X'_{\bar{u'}}[\bar{u’{y/x} = v’{y/x}}     where     X’ = X[x=y; hole]

so that the number of equations decreases by 1.

—Guy

>     Once again I have been studying the proof of confluence in Appendix C of our ICFP submission. Ranjit, the amount of detail is just jaw-droppingly impressive.
>
>     I believe that I have found two minor problems, both easily corrected, in the proof of Lemma C.15.
>
>
>     First, at lines 2163–2164 is says “wlog the equations eq_1 and eq_2 are adjacent”. The problem is that presumably the intention is to use SEQ-SWAP to bring the equations together if necessary (this might be worth stating explicitly), but because a side condition has been introduced on SEQ-SWAP, namely “unless (eq is y=v’ and y \leq x)”, it may not be possible to use this rule to bring the equations together. For example, if we assume x < y < z, then in the sequence “x=1; y=2; z=3; e” it will not be possible to make “x=1” and “z=3” adjacent.
>
>     The obvious solution is to observe explicitly that the side condition on SEQ-SWAP is there for purposes of guiding the intended evaluation strategy rather than for correctness, and therefore we are justified in ignoring the side condition for purposes of the confluence proof. Therefore we really can make any two equations of the form “x=v” adjacent.
>
>
>     Second, at line 2166, for the case “x \equiv y”, the text proposes to use a context “X’ = X{z/x}[x=z]”; the problem is that this is not a context, because it has no hole. I believe the correct intent is to use a context “X’ = X{z/x}[x=z; box]”.
>
>     A related subproblem is that the hole in an X context cannot be an equation (or sequence of equations); it must be an expression. So in line 2163 we really need to say not “e \equiv X[x=v_1; y=v_2]” but rather “e \equiv X[x=v_1; y=v_2; emptytup]”
>
>     With that understanding, we can start the diagram at line 2169 with the expression “X[x=u; x=v; emptytup]” at the upper left; using eq_1 rewrites it to “X{u/x}[x=u; u=v; emptytup]” at the upper right, which can then be (algebraically) rewritten into the equivalent form “X’{u/z}[u=v; emptytup]”; and using eq_2 rewrites it to “X{v/x}[x=u; u=v; emptytup]” at the lower left, which can then be rewritten into the equivalent form “X’{v/z}[v=u; emptytup]”. I think it would be helpful to the reader to show those rewrites explicitly.Then Lemma C.18 can be applied (but the statement of Lemmas C.17 and C.18 should also be modified to use emptytup after the equation lists).
>

