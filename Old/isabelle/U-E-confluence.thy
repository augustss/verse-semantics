theory "U-E-confluence"
  imports Main
begin

definition "diag R1 S1 R2 S2 = (R1\<inverse> O S1 \<subseteq> S2 O R2\<inverse>)"
definition "comm R S = diag R S R S"
definition "diam R = comm R R"

lemma diag_symm:
  "diag R1 S1 R2 S2 \<Longrightarrow> diag S1 R1 S2 R2"
unfolding diag_def by blast

lemma comm_symm:
  "comm R S \<Longrightarrow> comm S R"
unfolding comm_def by (rule diag_symm)

lemma commutes_rtrancl:
  assumes "diag R S R (S\<^sup>*)"
  shows "comm R (S\<^sup>*)"
proof-
  {
    fix a b c assume "(a,c) \<in> S\<^sup>*" and "(a,b) \<in> R"
    then have "\<exists> d. (b,d) \<in> S\<^sup>* \<and> (c,d) \<in> R"
    proof(induction arbitrary: b rule: converse_rtrancl_induct)
      case base
      then show ?case by auto
    next
      case (step a e)
      from `(a, e) \<in> S` and `(a, b) \<in> R`
      obtain f where "(e, f) \<in> R" and "(b, f) \<in> S\<^sup>*"
          using `diag R S R (S\<^sup>*)` by (auto simp add: diag_def)
      from `(b, f) \<in> S\<^sup>*` `(e, f) \<in> R`
      obtain d where "(f,d) \<in> S\<^sup>*" and "(c,d) \<in> R" using  step.IH by auto 
      from  `(b, f) \<in> S\<^sup>*` and `(f,d) \<in> S\<^sup>*`
      have "(b, d) \<in> S\<^sup>*" by simp
      with `(c, d) \<in> R`
      show ?case by auto
    qed
  }
  thus ?thesis unfolding comm_def diag_def by blast
qed

lemma commutes_rtrancl2:
  "diag R S (R\<^sup>*) S \<Longrightarrow> comm (R\<^sup>*) S"
by (rule comm_symm, rule commutes_rtrancl, rule diag_symm)

lemma diag_union:
  "diag A S1 R2 S2 \<Longrightarrow> diag B S1 R2 S2 \<Longrightarrow> diag (A \<union> B) S1 R2 S2"
unfolding diag_def by blast

lemma diag_subseteq_3:
  "diag R1 S1 A S2 \<Longrightarrow> A \<subseteq> B \<Longrightarrow> diag R1 S1 B S2"
unfolding diag_def by blast

lemma diag_Union:
  "(\<And> x. diag (R1 x) S1 R2 S2) \<Longrightarrow> diag (\<Union> x. R1 x) S1 R2 S2"
unfolding diag_def by blast

(*  definition "rel_count2 R S n = R\<^sup>* O (S O R\<^sup>* ) ^^ n  *)


inductive relp_count2 :: "'a rel \<Rightarrow> 'a rel \<Rightarrow> nat \<Rightarrow> 'a \<Rightarrow> 'a \<Rightarrow> bool"
  for R S
  where
    relp_count2_base: "relp_count2 R S 0 x x"
  | relp_count2_left: 
    "(x, y) \<in> R ==> relp_count2 R S k y z \<Longrightarrow> relp_count2 R S k x z"
  | relp_count2_right: 
    "(x, y) \<in> S ==> relp_count2 R S k y z \<Longrightarrow> relp_count2 R S (Suc k) x z"

(* https://stackoverflow.com/a/16604803/946226 *)
definition rel_count2 where "rel_count2 R S k = {(x, y). relp_count2 R S k x y}"

lemma rel_count2_relp_count2_eq[pred_set_conv]:
  "relp_count2 R S n = (\<lambda> x y. (x,y) \<in> rel_count2 R S n)"
 by(simp add: rel_count2_def)

lemmas rel_count2_intros [intro?] = relp_count2.intros[to_set]
lemmas rel_count2_induct [consumes 2, induct set: rel_count2] = relp_count2.induct[to_set]
lemmas rel_count2_cases [consumes 2, cases set: rel_count2] = relp_count2.cases[to_set]
lemmas rel_count2_simps = relp_count2.simps[to_set]

lemma rel_count2_of_left_star:
  "R\<^sup>* \<subseteq> rel_count2 R S 0"
proof-
  {
  fix a b assume "(a,b) \<in> R\<^sup>*"
  then have "(a,b) \<in> rel_count2 R S 0"
  proof(induction rule: converse_rtrancl_induct)
    case base
    then show ?case by rule
  next
    case (step y z)
    from this(1) this(3)
    show ?case by (rule rel_count2_intros(2))      
  qed  
  }
  thus ?thesis by auto
qed

lemma rel_count2_trans:
  "rel_count2 R S k1 O rel_count2 R S k2 \<subseteq> rel_count2 R S (k1 + k2)"
proof-
  {
  fix a b c
  assume "relp_count2 R S k1 a b" and "(b,c) \<in> rel_count2 R S k2"
  then have "(a,c) \<in> rel_count2 R S (k1 + k2)"
  proof(induction rule: relp_count2.induct)
    case (relp_count2_base x)
    then show ?case by auto
  next
    case (relp_count2_left x y k z)
    then show ?case by (simp add: rel_count2_intros(2))
  next
    case (relp_count2_right x y k z)
    then show ?case by (simp add: rel_count2_intros(3))
  qed
  }
  thus ?thesis by (auto simp add: rel_count2_def)
qed

lemma union_star_rel_count2: "(R \<union> S)\<^sup>* \<subseteq> (\<Union> k. rel_count2 R S k)"
proof-
  {
  fix a b
  assume "(a,b) \<in> (R \<union> S)\<^sup>*"
  then have "\<exists> k. (a, b) \<in> rel_count2 R S k"
  proof(induction rule: converse_rtrancl_induct)
    case base
    have "(b, b) \<in> rel_count2 R S 0" by rule
    then show ?case by blast
  next
    case (step y z)
    then obtain k where "(z, b) \<in> rel_count2 R S k" by auto

    from `(y, z) \<in> R \<union> S`
    show ?case
    proof 
      assume "(y, z) \<in> R"
      from this `(z, b) \<in> rel_count2 R S k` 
      have "(y, b) \<in> rel_count2 R S k" by rule
      thus ?case by auto
    next
      assume "(y, z) \<in> S"
      from this `(z, b) \<in> rel_count2 R S k` 
      have "(y, b) \<in> rel_count2 R S (Suc k)" by rule
      thus ?case by auto
    qed
  qed
  }
  thus ?thesis by auto
qed

lemma rel_count2_union_star: "rel_count2 R S n \<subseteq> (R \<union> S)\<^sup>*"
proof-
  {
  fix a b
  assume "relp_count2 R S n a b"
  then have "(a,b) \<in> (R \<union> S)\<^sup>*"
  proof (induction rule: relp_count2.induct)
    case (relp_count2_base x)
    then show ?case by simp
  next
    case (relp_count2_left x y k z)
    then show ?case by (meson UnCI converse_rtrancl_into_rtrancl)
  next
    case (relp_count2_right x y k z)
    then show ?case by (meson UnCI converse_rtrancl_into_rtrancl)   
  qed
  }
  thus ?thesis unfolding rel_count2_def by auto
qed


lemma rel_count2_eq_union_star: "(\<Union> k. rel_count2 R S k) = (R \<union> S)\<^sup>*"
  using rel_count2_union_star union_star_rel_count2 by force

lemma diag_rel_count_union_star_1:
  assumes "\<And> k. diag (rel_count2 A B k) C D E"
  shows "diag ((A \<union> B)\<^sup>*) C D E"
  unfolding rel_count2_eq_union_star[symmetric]
  using assms by (rule diag_Union)


locale U_E_confluence =
  fixes U :: \<open>'a rel\<close> 
  fixes E :: \<open>'a rel\<close>
  fixes equiv :: \<open>nat \<Rightarrow> 'a rel\<close>
  assumes equiv_refl[simp]: "(x,x) \<in> equiv n"
  and equiv_trans: "trans (equiv n)"
  and equiv_symm: "sym (equiv n)"
  and E_equiv: "diag (equiv (Suc n)) E (equiv n) E"
  and U_equiv: "diag (equiv n) U (equiv n) (U\<^sup>=)"
  and U_confl: "diag (U\<^sup>*) (U\<^sup>*) (U\<^sup>*) (U\<^sup>* O equiv n)"
  and E_diamond: "diam E"
  and E_U_commute: "diag E U E (U\<^sup>*)"

context U_E_confluence 
begin

definition "R = U \<union> E"

lemma E_Ustar_commute:"comm E (U\<^sup>*)"
  using E_U_commute by (rule commutes_rtrancl)

lemma E_strong_local_confl:
  "comm (R\<^sup>*) E"
proof-
  from E_U_commute
  have "diag U E (U\<^sup>*) E" by (rule diag_symm)
  hence "diag U E (R\<^sup>*) E"
     by (rule diag_subseteq_3)(simp add: R_def rtrancl_mono)
  also
  from E_diamond
  have "diag E E (R\<^sup>*) E"
    unfolding diam_def comm_def
    by (rule diag_subseteq_3) (auto simp add: R_def)
  ultimately
  show ?thesis
    unfolding R_def
    by -(rule commutes_rtrancl2, rule diag_union)
qed

lemma k_depth:
  "diag (rel_count2 U E k) (equiv (n+k)) (rel_count2 U E k) (equiv n)"
proof-
  {  fix a b c
  assume "relp_count2 U E k a b" and "(a, c) \<in> equiv (n+k)"
  then have "(b, c) \<in> (equiv n) O (rel_count2 U E k)\<inverse>"
  proof(induction arbitrary: c rule: relp_count2.induct)
    case (relp_count2_base x)
    then show ?case by (simp add: rel_count2_intros(1) relcomp.relcompI)
  next
    case (relp_count2_left x y k z)
    from `(x, y) \<in> U` `(x, c) \<in> equiv (n + k)`
    obtain a where "(c,a) \<in> U\<^sup>=" "(y,a) \<in> equiv (n + k)"
      using U_equiv unfolding comm_def diag_def by auto

    from relp_count2_left.IH[OF `(y,a) \<in> equiv (n + k)`]
    obtain b where "(z, b) \<in> equiv n" and "(a,b) \<in> rel_count2 U E k" by auto

    from `(c,a) \<in> U\<^sup>=` `(a,b) \<in> rel_count2 U E k`
    have "(c,b) \<in> rel_count2 U E k" by (auto intro: rel_count2_intros)
    with `(z, b) \<in> equiv n`    
    show ?case by auto
  next
    case (relp_count2_right x y k z)
    from `(x, y) \<in> E` `(x, c) \<in> equiv (n + Suc k)`
    obtain a where "(c,a) \<in> E" "(y,a) \<in> equiv (n + k)"
      using E_equiv unfolding diag_def by auto

    from relp_count2_right.IH[OF `(y,a) \<in> equiv (n + k)`]
    obtain b where "(z, b) \<in> equiv n" and "(a,b) \<in> rel_count2 U E k" by auto

    from `(c,a) \<in> E` `(a,b) \<in> rel_count2 U E k`
    have "(c,b) \<in> rel_count2 U E (Suc k)" by rule
    with `(z, b) \<in> equiv n`    
    show ?case by auto
  qed
} thus ?thesis unfolding rel_count2_def diag_def by blast
qed


lemma U_strong_local_confl_counting:
  "diag (rel_count2 U E k) (U\<^sup>*) (rel_count2 U E k) (U\<^sup>* O equiv n)"
proof-
 {
  fix a b c
  assume "relp_count2 U E k a b" and "(a, c) \<in> U\<^sup>*"
  then have "(b, c) \<in> (U\<^sup>* O equiv n) O (rel_count2 U E k)\<inverse>"
  proof(induction arbitrary: c n rule: relp_count2.induct)
    case (relp_count2_base x)
    moreover have "(c, c) \<in> rel_count2 U E 0" by rule
    moreover have "(c ,c) \<in> equiv n" by simp
    ultimately
    show ?case by blast
  next
    case (relp_count2_left x y k z)
    from `(x, y) \<in> U` have "(x, y) \<in> U\<^sup>*" by simp
    from `(x, y) \<in> U\<^sup>*` and `(x, c) \<in> U\<^sup>*`
    obtain a b where "(y, a) \<in> U\<^sup>*" "(a,b) \<in> equiv (n+k)" "(c,b) \<in> U\<^sup>*" 
      using U_confl unfolding diag_def by blast
  
    from `(y, a) \<in> U\<^sup>*`
    have "(z, a) \<in> (U\<^sup>* O equiv n) O (rel_count2 U E k)\<inverse>" by (rule relp_count2_left.IH)
    then obtain d e where
       "(z,d) \<in> U\<^sup>*" "(d,e) \<in> equiv n" "(a,e) \<in> rel_count2 U E k" by auto

    from `(a,b) \<in> equiv (n+k)` `(a,e) \<in> rel_count2 U E k`
    obtain f where "(b,f) \<in> rel_count2 U E k" "(e,f) \<in> equiv n"
       using k_depth unfolding diag_def by blast

    from `(z,d) \<in> U\<^sup>*` `(d,e) \<in> equiv n` `(e,f) \<in> equiv n`
    have "(z,f) \<in> U\<^sup>* O equiv n" using equiv_trans
      by (meson relcomp.relcompI transE)
    moreover
    from `(c,b) \<in> U\<^sup>*`
    have "(c,b) \<in> rel_count2 U E 0"
      using rel_count2_of_left_star by auto
    with `(b,f) \<in> rel_count2 U E k`
    have "(c,f) \<in> rel_count2 U E k" using rel_count2_trans by fastforce
    ultimately
    show "(z, c) \<in> (U\<^sup>* O equiv n) O (rel_count2 U E k)\<inverse>" by auto
  next
    case (relp_count2_right x y k z)

    from `(x, y) \<in> E` and `(x, c) \<in> U\<^sup>*`
    obtain a where "(y, a) \<in> U\<^sup>*"  "(c,a) \<in> E" 
      using E_Ustar_commute unfolding comm_def diag_def by blast
  
    from `(y, a) \<in> U\<^sup>*`
    have "(z, a) \<in> (U\<^sup>* O equiv n) O (rel_count2 U E k)\<inverse>" by (rule relp_count2_right.IH)
    then obtain e where
       "(z,e) \<in> (U\<^sup>* O equiv n)" "(a,e) \<in> rel_count2 U E k" by auto

    from `(c,a) \<in> E` `(a,e) \<in> rel_count2 U E k`
    have "(c,e) \<in> rel_count2 U E (Suc k)" by rule
    with `(z,e) \<in> (U\<^sup>* O equiv n)`
    show "(z, c) \<in> (U\<^sup>* O equiv n) O (rel_count2 U E (Suc k))\<inverse>" by auto
  qed

 }
 thus ?thesis unfolding rel_count2_def diag_def by blast
qed

lemma U_strong_local_confl:
  "diag (R\<^sup>*) (U\<^sup>*) (R\<^sup>*) (U\<^sup>* O equiv n)"
proof-
  {
  fix k
  have "diag (rel_count2 U E k) (U\<^sup>*) (R\<^sup>*) (U\<^sup>* O equiv n)"
    unfolding R_def
    using U_strong_local_confl_counting
    by (rule diag_subseteq_3)(rule rel_count2_union_star)
  }
  thus ?thesis unfolding R_def by (rule diag_rel_count_union_star_1)
qed

lemma U_final_lemma:
  "diag (rel_count2 U E k) (R\<^sup>*) (rel_count2 U E k O equiv n) (R\<^sup>*)"
proof-
 {
  fix a b c
  assume "relp_count2 U E k a b" and "(a, c) \<in> R\<^sup>*"
  then have "(b, c) \<in> (R\<^sup>*) O (rel_count2 U E k O equiv n)\<inverse>"
  proof(induction arbitrary: c n rule: relp_count2.induct)
    case (relp_count2_base x)
    moreover have "(c, c) \<in> rel_count2 U E 0" by rule
    moreover have "(c ,c) \<in> equiv n" by simp
    ultimately
    show ?case by blast
  next
    case (relp_count2_left x y k z)
    from `(x, y) \<in> U` have "(x, y) \<in> U\<^sup>*" by simp
    from `(x, y) \<in> U\<^sup>*` and `(x, c) \<in> R\<^sup>*`
    obtain a b where "(y, b) \<in> R\<^sup>*" "(c,a) \<in> U\<^sup>*" "(a,b) \<in> equiv (n+k)"  
      using U_strong_local_confl unfolding diag_def by blast
  
    from `(y, b) \<in> R\<^sup>*`
    have "(z, b) \<in> (R\<^sup>*) O (rel_count2 U E k O equiv n)\<inverse>" by (rule relp_count2_left.IH)
    then obtain d e where
       "(z,e) \<in> R\<^sup>*" "(b,d) \<in> rel_count2 U E k" "(d,e) \<in> equiv n"  by auto

    from symE[OF equiv_symm `(a,b) \<in> equiv (n+k)`] `(b,d) \<in> rel_count2 U E k`
    obtain f where "(a,f) \<in> rel_count2 U E k" "(d,f) \<in> equiv n"
      using k_depth unfolding diag_def by blast


    from `(c,a) \<in> U\<^sup>*`
    have "(c,a) \<in> rel_count2 U E 0"
      using rel_count2_of_left_star by auto
    with `(a,f) \<in> rel_count2 U E k`
    have "(c,f) \<in> rel_count2 U E k" using rel_count2_trans by fastforce
    moreover
    from symE[OF equiv_symm `(d,f) \<in> equiv n`]   `(d,e) \<in> equiv n`
    have "(f,e) \<in>  equiv n" using equiv_trans
      by (meson relcomp.relcompI transE)
    ultimately
    show "(z, c) \<in> (R\<^sup>*) O (rel_count2 U E k O equiv n)\<inverse>" using `(z,e) \<in> R\<^sup>*` by blast
  next
    case (relp_count2_right x y k z)

    from `(x, y) \<in> E` and `(x, c) \<in> R\<^sup>*`
    obtain a where "(y, a) \<in> R\<^sup>*" "(c,a) \<in> E"  
      using E_strong_local_confl unfolding comm_def diag_def by blast
  
    from `(y, a) \<in> R\<^sup>*`
    have "(z, a) \<in> (R\<^sup>*) O (rel_count2 U E k O equiv n)\<inverse>" by (rule relp_count2_right.IH)
    then obtain d e where
       "(z,e) \<in> R\<^sup>*" "(a,d) \<in> rel_count2 U E k" "(d,e) \<in> equiv n" by auto


    from `(c,a) \<in> E` `(a,d) \<in> rel_count2 U E k`
    have "(c,d) \<in> rel_count2 U E (Suc k)" by rule
    with `(d,e) \<in> equiv n`  `(z,e) \<in> R\<^sup>*` 
    show "(z, c) \<in> (R\<^sup>*) O (rel_count2 U E (Suc k) O equiv n)\<inverse>" by blast
  qed
 }
 thus ?thesis unfolding rel_count2_def diag_def by blast
qed

theorem confluence_up_to_n:
  "diag (R\<^sup>*) (R\<^sup>*) (R\<^sup>* O equiv n) (R\<^sup>*)"
proof-
  {
    fix k
    have "rel_count2 U E k \<subseteq> R\<^sup>*"  unfolding R_def by (rule rel_count2_union_star)
    hence "rel_count2 U E k O equiv n \<subseteq> R\<^sup>* O equiv n" by auto
    with U_final_lemma
    have "diag (rel_count2 U E k) (R\<^sup>*) (R\<^sup>* O equiv n) (R\<^sup>*)" by (rule diag_subseteq_3)
  }
  thus ?thesis unfolding R_def by (rule diag_rel_count_union_star_1)
qed

end

end
