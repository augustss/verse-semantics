theory CongruenceClosure
imports Contexts
begin

unbundle lattice_syntax


inductive congruent :: "red \<Rightarrow> bool"  where
  congruentI: "(\<And> x y C. R x y \<Longrightarrow> R (appEC C x) (appEC C y)) \<Longrightarrow> congruent R"

lemma congruentE[elim, consumes 2]:
  assumes "congruent R" and "R x y"
  shows "R (appEC C x) (appEC C y)"
  using assms
  by (simp add: congruent.simps)

lemma congruent_OO[simp]:
  assumes "congruent R" and "congruent S"
  shows "congruent (R OO S)"
  using assms
  by(auto intro!: congruentI)

lemma congruent_inv[simp]:
  assumes "congruent R"
  shows "congruent (R\<inverse>\<inverse>)"
  using assms by(auto intro!: congruentI)

lemma congruent_star[simp]:
  assumes "congruent R"
  shows "congruent R\<^sup>*\<^sup>*"
proof
  fix x y C
  assume "R\<^sup>*\<^sup>* x y"
  then show "R\<^sup>*\<^sup>* (appEC C x) (appEC C y)"
  proof (induction rule: converse_rtranclp_induct)
    case base
    then show ?case..
  next
    case (step x z)
    from `R x z`
    have "R (appEC C x) (appEC C z)" using  `congruent R`
      using congruentE by blast
    with `R\<^sup>*\<^sup>* (appEC C z) (appEC C y)`
    show ?case by auto
  qed
qed

inductive cc :: "red \<Rightarrow> red" for R where
  ccI: "R x y \<Longrightarrow> cc R (appEC C x) (appEC C y)"

lemma cc_rootI[intro]: "R x y \<Longrightarrow> cc R x y"
  by (drule ccI[of _ _ _ "[]"]) (simp add: appEC_def)

inductive cc' :: "red \<Rightarrow> red" for R where
  cc'I: "C \<noteq> [] \<Longrightarrow> R x y \<Longrightarrow> cc' R (appEC C x) (appEC C y)"


lemma congruent_cc[simp]:
  "congruent (cc R)"
  by (auto intro!: congruentI elim!:cc.cases 
           simp add: appEC_append[symmetric] intro: cc.intros)

section \<open>Reduction equivalence\<close>


definition "red_equiv R S \<longleftrightarrow>
  congruent S \<and>
  symp S \<and>
  reflp S \<and>
  R OO S \<le> S \<and>
  S OO R\<inverse>\<inverse> \<le> S"

end