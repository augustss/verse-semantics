theory Subst
  imports Syntax
begin

text \<open>To bootstrap things, we have to define bind for val, and that requires
lift for val. Then we can introduce the has_vars class, and from then on
deal with that only\<close>

fun liftE :: "nat \<Rightarrow> nat \<Rightarrow> exp \<Rightarrow> exp"
and liftV :: "nat \<Rightarrow> nat \<Rightarrow> val \<Rightarrow> val"
where
  "liftE n k (Val v) = Val (liftV n k v)"
| "liftE n k (Seq e1 e2) = (Seq (liftE n k e1) (liftE n k e2))"
| "liftE n k (Bar e1 e2) = (Bar (liftE n k e1) (liftE n k e2))"
| "liftE n k (Uni e1 e2) = (Uni (liftE n k e1) (liftE n k e2))"
| "liftE n k (App v1 v2) = (App (liftV n k v1) (liftV n k v2))"
| "liftE n k (Def e) = Def (liftE n (k+1) e)"
| "liftE n k (One e) = (One (liftE n k e))"
| "liftE n k (All e) = (All (liftE n k e))"
| "liftE n k Fail = Fail"
| "liftV n k (Var i) = Var (if i < k then i else i + n)"
| "liftV n k (Const c) = (Const c)"
| "liftV n k (Tup vs) = Tup (map (liftV n k) vs)"
| "liftV n k (Lam e) = Lam (liftE n (k+1) e)"
| "liftV n k (Op op) = Op op"

lemma liftE1_liftE[simp]:  "l \<le> k \<Longrightarrow> liftE (Suc 0) l (liftE n k e) = liftE n (Suc k) (liftE (Suc 0) l e)"
and   liftV1_liftV[simp]:  "l \<le> k \<Longrightarrow> liftV (Suc 0) l (liftV n k v) = liftV n (Suc k) (liftV (Suc 0) l v)"
  by (induction n k e and n k v arbitrary: l and l rule: liftE_liftV.induct) auto

definition lifted :: "nat \<Rightarrow> nat \<Rightarrow> (nat \<Rightarrow> val) \<Rightarrow> (nat \<Rightarrow> val)" where
  lifted_def_aux: "lifted n k f i =
      (if     i < k     then liftV n k (f i)
      else if i < k + n then Var i
      else                   liftV n k (f (i - n)))"


lemma lifted_Var[simp]: "lifted n k Var = Var"
  by (rule; auto simp add: lifted_def_aux)

lemma lifted1_lifted[simp]:
  "lifted (Suc 0) 0 (lifted n k f) = lifted n (Suc k) (lifted (Suc 0) 0 f)"
  by (rule; auto simp add: lifted_def_aux)

fun bindE :: "(nat \<Rightarrow> val) \<Rightarrow> exp \<Rightarrow> exp"
and bindV :: "(nat \<Rightarrow> val) \<Rightarrow> val \<Rightarrow> val"
where
  "bindE f (Val v1) = Val (bindV f v1)"
| "bindE f (Seq e1 e2) = Seq (bindE f e1) (bindE f e2)"
| "bindE f (Bar e1 e2) = Bar (bindE f e1) (bindE f e2)"
| "bindE f (Uni e1 e2) = Uni (bindE f e1) (bindE f e2)"
| "bindE f (App v1 v2) = App (bindV f v1) (bindV f v2)"
| "bindE f (Def e) = Def (bindE (lifted 1 0 f) e)"
| "bindE f (One e) = One (bindE f e)"
| "bindE f (All e) = All (bindE f e)"
| "bindE f Fail = Fail"
| "bindV f (Var i) = f i"
| "bindV f (Const k) = Const k"
| "bindV f (Tup vs) = Tup (map (bindV f) vs)"
| "bindV f (Lam e) = Lam (bindE (lifted 1 0 f) e)"
| "bindV f (Op k) = Op k"

lemma liftE_bindE[simp]: "liftE n k (bindE f e) = bindE (lifted n k f) (liftE n k e)"
  and liftV_bindV[simp]: "liftV n k (bindV f v) = bindV (lifted n k f) (liftV n k v)"
by (induction f e and f v arbitrary: k and k rule: bindE_bindV.induct) (auto simp add: lifted_def_aux)

lemma  lifted_bindV[simp]: "lifted n k (\<lambda>i. bindV f (g i)) = (\<lambda>i. bindV (lifted n k f) (lifted n k g i))"
  by (auto simp add: lifted_def_aux)

lemma bindE_bindE: "bindE f (bindE g e) = bindE (\<lambda> n. bindV f (g n)) e"
and bindV_bindV:   "bindV f (bindV g v) = bindV (\<lambda> n. bindV f (g n)) v"
  by (induction g e and g v arbitrary: f and f rule: bindE_bindV.induct) auto

lemma bindE_Var: "bindE Var e = e"
and bindV_Var:   "bindV Var v = v"
  by (induction e and v) (auto simp add: map_idI)

lemma liftE_is_bindE: "liftE n k e = bindE (\<lambda>i. Var (if i < k then i else i + n)) e"
and liftV_is_bindV: "liftV n k v = bindV (\<lambda>i. Var (if i < k then i else i + n)) v"
  by (induction n k e and n k v rule: liftE_liftV.induct)
     (auto 4 4 simp add: lifted_def_aux intro!: arg_cong2[where f= bindE])

fun occursE :: "nat \<Rightarrow> exp \<Rightarrow> bool"
and occursV :: "nat \<Rightarrow> val \<Rightarrow> bool"
where
  "occursE n (Val v) \<longleftrightarrow> occursV n v"
| "occursE n (Seq e1 e2) \<longleftrightarrow> occursE n e1 \<or> occursE n e2"
| "occursE n (Bar e1 e2) \<longleftrightarrow> occursE n e1 \<or> occursE n e2"
| "occursE n (Uni e1 e2) \<longleftrightarrow> occursE n e1 \<or> occursE n e2"
| "occursE n (App e1 e2) \<longleftrightarrow> occursV n e1 \<or> occursV n e2"
| "occursE n (Def e) \<longleftrightarrow> occursE (n+1) e"
| "occursE n (One e) \<longleftrightarrow> occursE n e"
| "occursE n (All e) \<longleftrightarrow> occursE n e"
| "occursE n Fail \<longleftrightarrow> False"
| "occursV n (Var i) \<longleftrightarrow> (i = n)"
| "occursV n (Const c) \<longleftrightarrow> False"
| "occursV n (Tup vs) \<longleftrightarrow> (\<exists> v \<in> set vs. occursV n v)"
| "occursV n (Lam e) \<longleftrightarrow> occursE (n+1) e"
| "occursV n (Op op) \<longleftrightarrow> False"

lemma occursE_liftE: "occursE n (liftE k j e) \<longleftrightarrow>
    (if n < j then occursE n e else if n < j + k then False else  occursE (n - k) e)"
  and occursV_liftV: "occursV n (liftV k j v) \<longleftrightarrow>
    (if n < j then occursV n v else if n < j + k then False else  occursV (n - k) v)"
  by (induction k j e and k j v arbitrary: n and n rule: liftE_liftV.induct)
     (auto simp add: Suc_diff_le)

lemma occursE_bindE: "occursE i (bindE f e) \<longleftrightarrow> (\<exists> n. occursE n e \<and> occursV i (f n))"
and   occursV_bindV: "occursV i (bindV f v) \<longleftrightarrow> (\<exists> n. occursV n v \<and> occursV i (f n))"
  apply (induction f e and f v arbitrary: i and i rule: bindE_bindV.induct)
               apply (auto simp add: occursV_liftV simp add: lifted_def_aux)
   apply force
  apply force
  done

lemma liftV_eq_iff[simp]:
  "liftV n k v = Var i \<longleftrightarrow> (i < k \<and> v = Var i) \<or> (i \<ge> k + n \<and> v = Var (i - n))"
  by (cases v) auto 

lemma lifted_eq_var_iff[simp]:
  "lifted n k f i = Var i \<longleftrightarrow> (i < k \<and> f i = Var i) \<or> (i \<ge> k \<and> i < k + n) \<or> (i \<ge> k + n \<and> f (i - n) = Var (i - n))"
  by (auto simp add: lifted_def_aux)

lemma map_idE[elim_format, elim]:
  "map f xs = xs \<Longrightarrow> x \<in> set xs \<Longrightarrow> f x = x"
  using map_eq_conv by fastforce

lemma bindE_id_iff:
  "bindE f e = e \<longleftrightarrow> (\<forall> n. occursE n e \<longrightarrow> f n = Var n)" 
and  bindV_id_iff:
  "bindV f v = v \<longleftrightarrow> (\<forall> n. occursV n v \<longrightarrow> f n = Var n)"
  by (induction f e and f v  rule: bindE_bindV.induct)
     (auto intro!:  map_idI elim!: map_idE[OF sym])


text \<open>Now we can define the has_vars class (we cannot do it earlier because we need bind for var)\<close>

class has_vars =
  fixes bind :: "(nat \<Rightarrow> val) \<Rightarrow> 'a \<Rightarrow> 'a"
  fixes occurs :: "nat \<Rightarrow> 'a \<Rightarrow> bool"
  assumes bind_bind_aux: "bind f (bind g x) = bind (\<lambda> n. bindV f (g n)) x"
  assumes occurs_bind_aux: "occurs i (bind f x) \<longleftrightarrow> (\<exists> n. occurs n x \<and> occursV i (f n))"
  assumes bind_id_iff: "bind f x = x \<longleftrightarrow> (\<forall> n. occurs n x \<longrightarrow> f n = Var n)"


text \<open>The instance for exp and val uses the definitions and lemmas above.\<close>

instantiation exp :: has_vars begin
  definition "bind = bindE"
  definition "occurs = occursE"
  instance
    by (standard; simp only: bind_exp_def occurs_exp_def
        bindE_bindE bindE_Var occursE_bindE bindE_id_iff)
end

instantiation val :: has_vars begin
  definition "bind = bindV"
  definition "occurs = occursV"
  instance
    by (standard; simp add: bind_val_def occurs_val_def
        bindV_bindV bindV_Var occursV_bindV bindV_id_iff)
end

text \<open>Substitution and renumbering is now derived from bind\<close>

definition renumber :: "(nat \<Rightarrow> nat) \<Rightarrow> 'a::has_vars \<Rightarrow> 'a" where
  "renumber f = bind (\<lambda> i. Var (f i))"

definition bumpAt :: "nat \<Rightarrow> nat \<Rightarrow> (nat \<Rightarrow> nat)" where
  "bumpAt n k i = (if i < k then i else i + n)"

definition liftedN :: "nat \<Rightarrow> nat \<Rightarrow> (nat \<Rightarrow> nat) \<Rightarrow> (nat \<Rightarrow> nat)" where
  "liftedN n k f i =
      (if     i < k     then bumpAt n k (f i)
      else if i < k + n then i
      else                   bumpAt n k (f (i - n)))"


abbreviation lift :: "'a::has_vars \<Rightarrow> 'a" ("\<up>") where
  "\<up> \<equiv> renumber Suc"

definition delN :: "nat \<Rightarrow> (nat \<Rightarrow> nat)" where
  "delN n i = (if i < n then i else i - 1)"
abbreviation del :: "nat \<Rightarrow> 'a::has_vars \<Rightarrow> 'a" where
  "del n \<equiv> renumber (delN n)"
abbreviation subst :: "nat \<Rightarrow> val \<Rightarrow> 'a::has_vars \<Rightarrow> 'a" where
  "subst n v \<equiv> bind (Var(n := v))"


text \<open>From now on we want to only use the overloaded variants for exp and var\<close>

lemmas bind_exp_simps[simp] = bindE.simps[folded bind_exp_def bind_val_def]
lemmas bind_val_simps[simp] = bindV.simps[folded bind_exp_def bind_val_def]
lemmas occurs_exp_simps[simp] = occursE.simps[folded occurs_exp_def occurs_val_def]
lemmas occurs_val_simps[simp] = occursV.simps[folded occurs_exp_def occurs_val_def]
lemmas bind_bind = bind_bind_aux[folded bind_val_def]
lemmas occurs_bind = occurs_bind_aux[folded occurs_val_def]

lemma liftV_eq_renumber: "liftV n k = renumber (bumpAt n k)"
  unfolding liftV_is_bindV renumber_def bind_val_def bumpAt_def
  by rule rule

lemmas lifted_def = lifted_def_aux[unfolded liftV_eq_renumber]


lemma has_varsI[case_names Bind_bind Occurs_bind Bind_id_iff]:
  fixes bind' occurs'
  assumes "\<And> f g x. bind' f (bind' g x) = bind' (\<lambda> n. bind f (g n)) x"
  assumes "\<And> i f e. occurs' i (bind' f e) \<longleftrightarrow> (\<exists> n. occurs' n e \<and> occurs i (f n))"
  assumes "\<And> f e. bind' f e = e \<longleftrightarrow> (\<forall>n. occurs' n e \<longrightarrow> f n = Var n)"
  shows "class.has_vars bind' occurs'"
  by (rule class.has_vars.intro) (intro assms[unfolded bind_val_def occurs_val_def])+

hide_const liftE bindE occursE
hide_const liftV bindV occursV

text \<open>Additional lemmas for exp and val\<close>

lemma bind_lifted_Var_f:
  "bind (lifted k l (\<lambda>n. Var (f n))) = renumber (liftedN k l f)"
  unfolding renumber_def lifted_def liftedN_def
  by (rule arg_cong[where f = bind]) auto

(* Make sure no bind remains *)
lemmas renumber_exp_simps[simp] =
  bind_exp_simps[where f = "\<lambda> n. Var (f n)" for f, folded renumber_def, unfolded bind_lifted_Var_f]
lemmas renumber_val_simps[simp] =
  bind_val_simps[where f = "\<lambda> n. Var (f n)" for f, folded renumber_def, unfolded bind_lifted_Var_f]

(*
lemma liftV_eq_Var[simp]:
    "(lift n k v = Var i) \<longleftrightarrow> (\<exists> l. (v = Var l \<and> i = (if l < k then l else l + n)))"
  by (cases v) auto
*)

lemma occurs_bars[simp]:
  "occurs n (bars es) \<longleftrightarrow> (\<exists> e \<in> set es. occurs n e)"
by (induction es rule: bars.induct) auto

lemma occurs_seqs[simp]:
  "occurs n (seqs es) \<longleftrightarrow> (\<exists> e \<in> set es. occurs n e)"
by (induction es rule: seqs.induct) auto

text "Derived lemmas"

lemma bind_id_iff2: "x = bind f x \<longleftrightarrow> (\<forall> n. occurs n x \<longrightarrow> f n = Var n)"
  by (metis bind_id_iff)

lemma bind_Var[simp]: "bind Var x = x"
  by (simp add: bind_id_iff)

lemma renumber_id[simp]: "renumber id x = x"
  unfolding renumber_def by simp

lemma bumpAt_0[simp]: "bumpAt 0 k = id"
  by rule (auto simp add: bumpAt_def)

lemma lifted_0[simp]: "lifted 0 k f = f"
  by (rule; auto simp add: lifted_def)
 
lemma occurs_renumber:
  "occurs n (renumber f e) = (\<exists> i. occurs i e \<and> n = f i)"
  unfolding renumber_def by (auto simp add: occurs_bind)

lemma occurs_del[simp]: "\<not> occurs n e \<Longrightarrow>
  occurs k (del n e) = (if k < n then occurs k e else occurs (Suc k) e)"
  unfolding delN_def occurs_renumber
  by (smt (verit, ccfv_SIG) diff_Suc_1 less_Suc_eq_0_disj less_imp_diff_less linorder_neqE_nat not_less_eq)

lemma occurs_lift[simp]: "occurs n (\<up> x) = (n > 0 \<and> occurs (n - 1) x)"
  unfolding  occurs_renumber by auto


lemma occurs_subst[simp]:
  "occurs i (subst n v e) \<longleftrightarrow> (i \<noteq> n \<and> occurs i e) \<or> (occurs n e \<and> occurs i v)"
 by (auto simp add: occurs_bind)

lemma lifted_bind[simp]: "lifted n k (\<lambda>i. bind f (g i)) = (\<lambda>i. bind (lifted n k f) (lifted n k g i))"
  by rule (auto intro!: arg_cong2[where f = bind] simp add: lifted_def renumber_def bind_bind bumpAt_def)

lemma bumpAt_1_0[simp]:
  "bumpAt (Suc 0) 0 = Suc"
  by rule (auto simp add: bumpAt_def)

lemma bumpAt_lt[simp]:
  "i < k \<Longrightarrow> bumpAt n k i = i"
  unfolding bumpAt_def by auto

lemma del_lift[simp]:
  "del 0 (\<up> e) = e"
  unfolding renumber_def delN_def
  by (simp add: bind_bind)


lemma liftedN_bumpAt[simp]:
  "liftedN l 0 (bumpAt n k) = bumpAt n (l + k)"
  unfolding liftedN_def bumpAt_def
  by rule auto

end
