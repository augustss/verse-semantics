theory Subst2
imports Syntax
begin

(*
This is a partial experiment to see if we can derive all opertions like
subst, lift, del, fvs, and their lemmas, from a single bind operation with simple laws.

The hope is that for each thing with names (expressions, values, the various contexs) only
a single function has to be defined.

Using a type class for these would then simplify a bunch of code.

It may be possible, but probably one would want to derive the equations for subst and fvs
again as lemmas, saving not much in the end.
*)

class has_bind =
  fixes bind :: "(nat \<Rightarrow> val) \<Rightarrow> 'a \<Rightarrow> 'a"

instance val :: has_bind
  sorry

definition subst :: "nat \<Rightarrow> val \<Rightarrow> 'a::has_bind \<Rightarrow> 'a" where
  "subst n v = bind (\<lambda> i. if i = n then v else Var i)"

definition renumber :: "(nat \<Rightarrow> nat) \<Rightarrow> 'a::has_bind \<Rightarrow> 'a" where
  "renumber f = bind (\<lambda> i. Var (f i))"

definition lift :: "nat \<Rightarrow> 'a::has_bind \<Rightarrow> 'a" where
  "lift n = renumber (\<lambda> i. n + i)"

definition del :: "nat \<Rightarrow> 'a::has_bind \<Rightarrow> 'a" where
  "del n = renumber (\<lambda> i. if i < n then i else i - 1)"

definition bumpsAt :: "nat set \<Rightarrow> (nat \<Rightarrow> nat)" where
  "bumpsAt S = (\<lambda> i. 2 * i + (if i \<in> S then 1 else 0))"

definition unBump :: "(nat \<Rightarrow> 'a) \<Rightarrow> (nat \<Rightarrow> 'a) \<Rightarrow> nat \<Rightarrow> 'a" where
  "unBump f g i = (if odd i then f (i div 2) else g (i div 2))"

lemma unBump_bumpsAt[simp]:
  "unBump f g (bumpsAt S i) = (if i \<in> S then f i else g i)"
  unfolding unBump_def bumpsAt_def by simp

definition unused :: "nat set \<Rightarrow> 'a::has_bind \<Rightarrow> bool" where
  "unused S x \<longleftrightarrow> renumber (bumpsAt S) x = renumber (bumpsAt {}) x"


lemma bind_Var[simp]: "bind f (Var n) = f n"
  sorry

lemma renumber_Var[simp]: "renumber f (Var n) = Var (f n)"
  unfolding renumber_def by simp

class has_vars = has_bind +
  assumes bind_bind: "bind f (bind g x) = bind (\<lambda> n. bind f (g n)) x"
  and bind_id[simp]: "bind Var x = x"

instance val :: has_vars
  sorry

lemma unused_Var[simp]:
  "unused S (Var i) \<longleftrightarrow> i \<notin> S"
  unfolding unused_def by (auto simp add: bumpsAt_def)

lemma bind_renumber[simp]:
  fixes x :: "'a::has_vars"
  shows "bind f (renumber g x) = bind (\<lambda> n. f (g n)) x"
  unfolding renumber_def by (simp add: bind_bind)

lemma bind_congr:
  fixes x :: "'a::has_vars"
  assumes "unused S x"
  assumes "\<And> x. x \<notin> S \<Longrightarrow> f x = g x"
  shows "bind f x = bind g x"
proof-
  let ?h = "unBump f g"
  from assms(2)
  have "f = (\<lambda>n. if n \<in> S then f n else g n)" by auto
  hence "bind f x = bind ?h (renumber (bumpsAt S) x)" by simp
  also have "... = bind ?h (renumber (bumpsAt {}) x)"
    using `unused S x` unfolding unused_def by simp
  also have "... = bind g x"  by (simp add: bind_bind)
  finally show ?thesis.
qed

lemma renumber_congr:
  fixes x :: "'a::has_vars"
  assumes "unused S x"
  assumes "\<And> x. x \<notin> S \<Longrightarrow> f x = g x"
  shows "renumber f x = renumber g x"
  using assms(1) unfolding renumber_def
  by (rule bind_congr) (simp add: assms(2))

(* The set of unused sets form an ideal *)
lemma unused_empty:
  fixes x :: "'a::has_vars"
  shows "unused {} x"
 unfolding unused_def..

lemma unused_subset:
  fixes x :: "'a::has_vars"
  assumes "unused S1 x"
  assumes "S2 \<subseteq> S1"
  shows "unused S2 x"
  using assms(2)
  by (auto intro!: renumber_congr[OF assms(1)] simp add: bumpsAt_def unused_def)

lemma unused_union:
  fixes x :: "'a::has_vars"
  assumes "unused S1 x"
  assumes "unused S2 x"
  shows "unused (S1 \<union> S2) x"
proof-
  have "renumber (bumpsAt (S1 \<union> S2)) x = renumber (bumpsAt S2) x"
    by (rule renumber_congr[OF assms(1)]) (auto simp add: bumpsAt_def)
  also have "\<dots> = renumber (bumpsAt {}) x"
    by (rule renumber_congr[OF assms(2)]) (auto simp add: bumpsAt_def)
  finally show ?thesis unfolding unused_def.
qed

(* But it is unclear how to go to the arbitrary union! *)

definition unuseds :: "'a :: has_bind \<Rightarrow> nat set" where
  "unuseds x = Union { S. unused S x}"

lemma unused_unuseds:
  fixes x :: "'a::has_vars"
  shows "unused (unuseds x) x"
  sorry

(* Can this be shown? Infinite unions are hard!

Probably it does not hold.
*)

definition fvs :: "'a :: has_bind \<Rightarrow> nat set" where
  "fvs x = { n. \<forall> S. unused S x \<longrightarrow> n \<notin> S}"

definition "closed x = unused UNIV x"

lemma closed_fvs[simp]:  "closed x \<Longrightarrow> fvs x = {}"
  unfolding closed_def fvs_def by auto

lemma closed_bind:
  fixes x :: "'a::has_vars"
  assumes "closed x"
  shows "bind f x = x"
proof-
  from assms
  have "bind f x = bind Var x"
    unfolding closed_def
    by (rule bind_congr) auto
  thus ?thesis by simp
qed

definition occurs :: "nat \<Rightarrow> 'a::has_bind \<Rightarrow> bool" where
  "occurs n x \<longleftrightarrow> n \<in> fvs x"

lemma fvs_Var[simp]:
  "fvs (Var i) = {i}"
  unfolding fvs_def 
  apply simp
  by (smt (verit, ccfv_threshold) equals0I in_mono mem_Collect_eq mk_disjoint_insert subset_insertI)

lemma occurs_Var[simp]:
  "occurs n (Var i) \<longleftrightarrow> i = n"
  unfolding occurs_def by auto

lemma fvs_bind:
  fixes x :: "'a :: has_vars"
  shows "fvs (bind f x) = (\<Union> i \<in> fvs x. fvs (f i))"
  unfolding fvs_def
  apply auto


lemma free_bindI:
  fixes x :: "'a :: has_vars"
  shows "occurs n (bind f x) \<longleftrightarrow> (\<exists> i. occurs i x \<and> occurs n (f i))"
proof
  
  
  

lemma free_bind[simp]:
  fixes x :: "'a :: has_vars"
  shows "free n (bind f x) \<longleftrightarrow> (\<forall> i. free i x \<or> free n (f i))"
  unfolding free_def renumber_def
  apply (auto simp add: bind_bind)
  sorry


lemma free_lift[simp]:
  fixes x :: "'a :: has_vars"
  shows "free n (lift k x) \<longleftrightarrow> (if n < k then True else free (n - k) x)"
  unfolding  lift_def
  apply (auto simp add: bind_bind)
  sledgehammer



end