theory CoreVerse
imports
  Main 
  Nominal2.Nominal2
  "HOL-Eisbach.Eisbach"
begin

atom_decl var

(*
nominal_datatype "exp" =
  Val val
| Seq exp exp
| Bar exp exp
| App val val
| Def x::var e::"exp" binds x in e
| One exp
| All exp
and val =
  Var var
| Const int
| Unit
| Tuple val val
| Lam x::var e::exp binds x in e
*)

nominal_datatype "exp" =
  Seq exp exp
| Bar exp exp
| App exp exp
| Def x::var e::"exp" binds x in e
| One exp
| All exp
(* Now the values *)
| Var var
| Const int
| Unit
| Tuple exp exp
| Lam x::var e::exp binds x in e


subsection \<open>Ad-hoc methods for nominal-functions over lambda terms\<close>

ML \<open>
fun graph_aux_tac ctxt =
  SUBGOAL (fn (subgoal, i) =>
    (case subgoal of
      Const (@{const_name Trueprop}, _) $ (Const (@{const_name eqvt}, _) $ Free (f, _)) =>
        full_simp_tac (
          ctxt addsimps [@{thm eqvt_def}, Proof_Context.get_thm ctxt (f ^ "_def")]) i
    | _ => no_tac))
\<close>

method_setup eqvt_graph_aux =
  \<open>Scan.succeed (fn ctxt : Proof.context => SIMPLE_METHOD' (graph_aux_tac ctxt))\<close>
  "show equivariance of auxilliary graph construction for nominal functions"

method without_alpha_lst methods m =
  (match termI in H [simproc del: alpha_lst]: _ \<Rightarrow> \<open>m\<close>)

method Abs_lst =
  (match premises in
    "atom ?x \<sharp> c" and P [thin]: "[[atom _]]lst. _ = [[atom _]]lst. _" for c :: "'a::fs" \<Rightarrow>
      \<open>rule Abs_lst1_fcb2' [where c = c, OF P]\<close>
  \<bar> P [thin]: "[[atom _]]lst. _ = [[atom _]]lst. _" \<Rightarrow> \<open>rule Abs_lst1_fcb2' [where c = "()", OF P]\<close>)

find_theorems name:strong_exhaust

method pat_comp_aux =
  (match premises in
    "x = (_ :: exp) \<Longrightarrow> _" for x \<Rightarrow> \<open>rule exp.strong_exhaust [where y = x and c = x]\<close>
  \<bar> "x = (Var _, _) \<Longrightarrow> _" for x :: "_ :: fs" \<Rightarrow>
    \<open>rule exp.strong_exhaust [where y = "fst x" and c = x]\<close>
  \<bar> "x = (_, Var _) \<Longrightarrow> _" for x :: "_ :: fs" \<Rightarrow>
    \<open>rule exp.strong_exhaust [where y = "snd x" and c = x]\<close>
  \<bar> "x = (_, _, Var _) \<Longrightarrow> _" for x :: "_ :: fs" \<Rightarrow>
    \<open>rule exp.strong_exhaust [where y = "snd (snd x)" and c = x]\<close>
)

method pat_comp = (pat_comp_aux; force simp: fresh_star_def fresh_Pair_elim)

method freshness uses fresh =
  (match conclusion in
    "_ \<sharp> _" \<Rightarrow> \<open>simp add: fresh_Unit fresh_Pair fresh\<close>
  \<bar> "_ \<sharp>* _" \<Rightarrow> \<open>simp add: fresh_star_def fresh_Unit fresh_Pair fresh\<close>)

method solve_eqvt_at =
  (simp add: eqvt_at_def; simp add: perm_supp_eq fresh_star_Pair)+

method nf uses fresh = without_alpha_lst \<open>
  eqvt_graph_aux, rule TrueI, pat_comp, auto, Abs_lst,
  auto simp: Abs_fresh_iff pure_fresh perm_supp_eq,
  (freshness fresh: fresh)+,
  solve_eqvt_at?\<close>



subsection \<open>Substitutions\<close>

nominal_function subst
where
  "subst x s (Seq t u) = Seq (subst x s t) (subst x s u)"
| "subst x s (Bar t u) = Bar (subst x s t) (subst x s u)"
| "subst x s (One e) = One (subst x s e)"
| "subst x s (All e) = All (subst x s e)"
| "atom y \<sharp> (x, s) \<Longrightarrow> subst x s (Def y e) = Def y (subst x s e)"
| "subst x s (App t u) = App (subst x s t) (subst x s u)"
| "subst x s (Var y) = (if x = y then s else Var y)"
| "atom y \<sharp> (x, s) \<Longrightarrow> subst x s (Lam y t) = Lam y (subst x s t)"
proof goal_cases
  case (3 P x)
  show ?case  sorry
next
  apply_end (auto simp add: eqvt_def subst_graph_def) 

qed (auto simp add: eqvt_def subst_graph_aux_def)

nominal_termination (eqvt) by lexicographic_order


end