theory VerseDB
  imports Main
begin

unbundle lattice_syntax

datatype "exp" =
  Val val
| Seq exp exp
| Bar exp exp
| Uni exp exp
| App val val
| Def exp
| One exp
| All exp
and val =
  Var nat
| Const int
| Tup "val list"
| Lam exp


fun liftE :: "nat \<Rightarrow> nat \<Rightarrow> exp \<Rightarrow> exp" ("\<up>\<^sub>e")
and liftV :: "nat \<Rightarrow> nat \<Rightarrow> val \<Rightarrow> val" ("\<up>\<^sub>v")
where
  "\<up>\<^sub>e n k (Val v) = Val (\<up>\<^sub>v n k v)"
| "\<up>\<^sub>e n k (Seq e1 e2) = (Seq (\<up>\<^sub>e n k e1) (\<up>\<^sub>e n k e2))"
| "\<up>\<^sub>e n k (Bar e1 e2) = (Bar (\<up>\<^sub>e n k e1) (\<up>\<^sub>e n k e2))"
| "\<up>\<^sub>e n k (Uni e1 e2) = (Uni (\<up>\<^sub>e n k e1) (\<up>\<^sub>e n k e2))"
| "\<up>\<^sub>e n k (App e1 e2) = (App (\<up>\<^sub>v n k e1) (\<up>\<^sub>v n k e2))"
| "\<up>\<^sub>e n k (Def e) = Def (\<up>\<^sub>e n (k+1) e)"
| "\<up>\<^sub>e n k (One e) = (One e)"
| "\<up>\<^sub>e n k (All e) = (All e)"
| "\<up>\<^sub>v n k (Var i) = Var (if i < k then i else i + n)"
| "\<up>\<^sub>v n k (Const c) = (Const c)"
| "\<up>\<^sub>v n k (Tup vs) = Tup (map (\<up>\<^sub>v n k) vs)"
| "\<up>\<^sub>v n k (Lam e) = Lam (\<up>\<^sub>e n (k+1) e)"

section \<open>Contexts\<close>

text \<open>Context element\<close>

datatype vce =
  CTup "val list" "val list"

type_synonym vc = "vce list"

datatype ece =
  CVal vc (* Implicit CLam at the end*)
| CSeql exp
| CSeqr exp
| CBarl exp
| CBarr exp
| CUnil exp
| CUnir exp
| CAppl vc val
| CAppr val vc
| CDef
| COne
| CAll

type_synonym ec = "ece list"

fun appVCE :: "vce \<Rightarrow> val \<Rightarrow> val" where
  "appVCE (CTup vs1 vs2) v = Tup (vs1 @ [v] @ vs2)"

lemma appVCE_inj[simp]: "appVCE vce v1 = appVCE vce v2 \<longleftrightarrow> v1 = v2"
  by (cases vce) (auto)

definition appVC' ::  "vc \<Rightarrow> val \<Rightarrow> val" where
  "appVC' vc v = foldr appVCE vc v"

definition appVC ::  "vc \<Rightarrow> exp \<Rightarrow> val" where
  "appVC vc e = appVC' vc (Lam e)"

lemma appVC_Cons[simp]: "appVC (vce # vc) e = appVCE vce (appVC vc e)"
  by (simp add: appVC_def appVC'_def)

lemma appVC'_inj[simp]: "appVC' vc v1 = appVC' vc v2 \<longleftrightarrow> v1 = v2"
  by (induction vc) (auto simp add: appVC'_def)

lemma appVC_inj[simp]: "appVC vc v1 = appVC vc v2 \<longleftrightarrow> v1 = v2"
  by (simp add: appVC_def)

fun appECE :: "ece \<Rightarrow> exp \<Rightarrow> exp" where
  "appECE (CVal vc) e = Val (appVC vc e)"
| "appECE (CSeql e2) e1 = Seq e1 e2"
| "appECE (CSeqr e1) e2 = Seq e1 e2"
| "appECE (CBarl e2) e1 = Bar e1 e2"
| "appECE (CBarr e1) e2 = Bar e1 e2"
| "appECE (CUnil e2) e1 = Uni e1 e2"
| "appECE (CUnir e1) e2 = Uni e1 e2"
| "appECE (CAppl vc v) e = App (appVC vc e) v"
| "appECE (CAppr v vc) e = App v (appVC vc e)"
| "appECE CDef e = Def e"
| "appECE COne e = One e"
| "appECE CAll e = All e"

definition appEC ::  "ec \<Rightarrow> exp \<Rightarrow> exp" where
   "appEC ec e = foldr appECE ec e"

lemma appEC_nil[simp]:
  "appEC [] e = e"
  by (simp add: appEC_def)

lemma appEC_append:
  "appEC (ec1 @ ec2) e = appEC ec1 (appEC ec2 e)"
  by (simp add: appEC_def)

lemma appECE_inj[simp]: "appECE ece x = appECE ece y \<longleftrightarrow> x = y"
  by (cases ece) (auto)

lemma appEC_inj[simp]: "appEC ec x = appEC ec y \<longleftrightarrow> x = y"
  by (induction ec) (auto simp add: appEC_def)

type_synonym red = "exp \<Rightarrow> exp \<Rightarrow> bool"

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

section \<open>Parallel context\<close>

definition "parallelEC ec1 ec2 \<longleftrightarrow>
  (\<forall> a b a' b'.
  appEC ec1 a = appEC ec2 b \<longrightarrow>
  (\<exists> ec1' ec2'.
  appEC ec1 a' = appEC ec2' b \<and>
  appEC ec2 b' = appEC ec1' a \<and>
  appEC ec2' b' = appEC ec1' a'))"

definition "parallelECE ece1 ece2 \<longleftrightarrow>
  (\<forall> a b a' b'.
  appECE ece1 a = appECE ece2 b \<longrightarrow>
  (\<exists> ece1' ece2'.
  appECE ece1 a' = appECE ece2' b \<and>
  appECE ece2 b' = appECE ece1' a \<and>
  appECE ece2' b' = appECE ece1' a'))"


definition "parallelVC vc1 vc2 \<longleftrightarrow>
  (\<forall> a b a' b'.
  appVC vc1 a = appVC vc2 b \<longrightarrow>
  (\<exists> vc1' vc2'.
  appVC vc1 a' = appVC vc2' b \<and>
  appVC vc2 b' = appVC vc1' a \<and>
  appVC vc2' b' = appVC vc1' a'))"


lemma Val_eq_appECE_simp[simp]:
  "(Val v = appECE ece a) \<longleftrightarrow>
    (\<exists> vc. ece = CVal vc \<and> v = appVC vc a)"
  by (cases ece) auto

lemma Seq_eq_appECE_simp[simp]:
  "(Seq a b = appECE ece c) \<longleftrightarrow>
    (ece = CSeql b \<and> c = a) \<or> (ece = CSeqr a \<and> c = b)"
  by (cases ece) auto

lemma Bar_eq_appECE_simp[simp]:
  "(Bar a b = appECE ece c) \<longleftrightarrow>
    (ece = CBarl b \<and> c = a) \<or> (ece = CBarr a \<and> c = b)"
  by (cases ece) auto

lemma Uni_eq_appECE_simp[simp]:
  "(Uni a b = appECE ece c) \<longleftrightarrow>
    (ece = CUnil b \<and> c = a) \<or> (ece = CUnir a \<and> c = b)"
  by (cases ece) auto

lemma App_eq_appECE_simp[simp]:
  "(App v1 v2 = appECE ece c) \<longleftrightarrow>
    (\<exists> vc. ece = CAppl vc v2 \<and> v1 = appVC vc c) \<or> 
    (\<exists> vc. ece = CAppr v1 vc \<and> v2 = appVC vc c)"
  by (cases ece) auto


lemma Def_eq_appECE_simp[simp]:
  "(Def e = appECE ece c) \<longleftrightarrow> (ece = CDef \<and> c = e)"
  by (cases ece) auto

lemma One_eq_appECE_simp[simp]:
  "(One e = appECE ece c) \<longleftrightarrow> (ece = COne \<and> c = e)"
  by (cases ece) auto

lemma All_eq_appECE_simp[simp]:
  "(All e = appECE ece c) \<longleftrightarrow> (ece = CAll \<and> c = e)"
  by (cases ece) auto


lemma parallelVC[intro!]:
  assumes neq: "vc1 \<noteq> vc2"
  shows "parallelVC vc1 vc2"
using assms
proof(induct vc1 vc2 rule: list_induct2'[case_names Nil_Nil Cons_Nil Nil_Cons Cons_Cons])
  case Nil_Nil
  then show ?case by auto
next
  case (Cons_Nil vce vc)
  then show ?case by(cases vce)(auto simp add: parallelVC_def appVC_def appVC'_def)
next
  case (Nil_Cons vce vc)
  then show ?case by(cases vce)(auto simp add: parallelVC_def appVC_def appVC'_def)
next
  case (Cons_Cons vce1 vc1 vce2 vc2)
  show ?case
  proof(cases "vce1 = vce2")
    case True with Cons_Cons have "parallelVC vc1 vc2" by auto
    with True show ?thesis
      apply (cases vce1)
      apply (auto simp add: parallelVC_def )
      (* Ugly! *)
      apply (elim allE) apply (erule impE) apply (assumption)
      apply (erule_tac x = a' in allE)
      apply (erule_tac x = b' in allE)
      apply (erule exE)+
      apply (rule_tac x= "CTup x1 x2 # vc1'" in exI)
      apply (rule_tac x= "CTup x1 x2 # vc2'" in exI)
      apply auto
      done
  next
    case False
    then show ?thesis
    proof (cases vce1; cases vce2)
      fix vs1 vs1' vs2 vs2'
      assume "vce1 = CTup vs1 vs1'" "vce2 = CTup vs2 vs2'"
      show ?thesis
      proof(cases "length vs1 + length vs1' = length vs2 + length vs2'")
        case True
        consider "length vs1 < length vs2" | "length vs1 = length vs2" | "length vs1 > length vs2"
          by fastforce     
        thus ?thesis
        proof(cases)
          case 1          
          then show ?thesis
          using `vce1 = _` `vce2 = _` True
          apply (auto simp add: parallelVC_def )
            (* Ugly! *)
          apply (rule_tac x= "CTup vs1 (list_update vs1' (length vs2 - length vs1 - 1) (appVC vc2 b'))
                                   # vc1" in exI)
          apply (rule_tac x= "CTup (list_update vs2 (length vs1) (appVC vc1 a')) vs2' # vc2" in exI)        
          apply (auto simp add: list_update_append1)
            apply (metis list_update_append1 list_update_length)
           apply (metis Suc_diff_Suc less_Suc_eq list_update_append list_update_code(3) list_update_length not_less_eq)          
          apply (smt (verit, ccfv_SIG) Suc_diff_Suc less_Suc_eq list_update_append list_update_code(3) list_update_length not_less_eq) 
          done
        next
          case 2 thus ?thesis
            using  `vce1 \<noteq> vce2`  `vce1 = _` `vce2 = _` by (auto simp add: parallelVC_def)
        next
          case 3
          then show ?thesis
          using `vce1 = _` `vce2 = _` True
          apply (auto simp add: parallelVC_def )
            (* Ugly! *)
          apply (rule_tac x= "CTup (list_update vs1 (length vs2) (appVC vc2 b')) vs1' # vc1" in exI)
          apply (rule_tac x= "CTup vs2 (list_update vs2' (length vs1 - length vs2 - 1) (appVC vc1 a')) # vc2" in exI)        
          apply (auto simp add: list_update_append1)
           apply (metis Suc_diff_Suc less_Suc_eq list_update_append list_update_code(3) list_update_length not_less_eq)          
            apply (metis list_update_append1 list_update_length)
          apply (smt (verit, ccfv_SIG) Suc_diff_Suc less_Suc_eq list_update_append list_update_code(3) list_update_length not_less_eq) 
          done
      qed        
      next
        case False
        hence [simp]: "vs1 @ v1 # vs1' \<noteq> vs2 @ v2 # vs2'" for v1 v2
          by (auto dest!: arg_cong[of _ _ length])
        then show ?thesis
          using `vce1 = _` `vce2 = _`
          by (auto simp add: parallelVC_def appVC_def appVC'_def)
      qed
    qed
  qed  
qed

lemma parallelVC_CVal_ECE[intro!]:
  assumes "parallelVC vc1 vc2"
  shows "parallelECE (CVal vc1) (CVal vc2)"
  using assms
  by (fastforce simp add: parallelVC_def parallelECE_def)

lemma parallelVC_CAppl_ECE[intro!]:
  assumes "parallelVC vc1 vc2 \<or> (v1 \<noteq> v2)"
  shows "parallelECE (CAppl vc1 v1) (CAppl vc2 v2)"
  using assms
  apply (auto simp add: parallelVC_def parallelECE_def)
  by (metis appECE.simps(8))

lemma parallelVC_CAppr_ECE[intro!]:
  assumes "parallelVC vc1 vc2 \<or> (v1 \<noteq> v2)"
  shows "parallelECE (CAppr v1 vc1) (CAppr v2 vc2)"
  using assms
  apply (auto simp add: parallelVC_def parallelECE_def)
  by (metis appECE.simps(9))


lemma parallelECE:
  assumes neq: "ece1 \<noteq> ece2"
  shows "parallelECE ece1 ece2"
using assms
proof(cases ece1)
  case CVal
  thus ?thesis using assms
  proof(cases ece2)
    case (CVal x1)
    then show ?thesis using `ece1 = _` neq by auto
  qed(auto simp add: parallelECE_def)
next case CSeql with neq show ?thesis unfolding parallelECE_def by fastforce
next case CSeqr with neq show ?thesis unfolding parallelECE_def by fastforce
next case CBarl with neq show ?thesis unfolding parallelECE_def by fastforce
next case CBarr with neq show ?thesis unfolding parallelECE_def by fastforce
next case CUnil with neq show ?thesis unfolding parallelECE_def by fastforce
next case CUnir with neq show ?thesis unfolding parallelECE_def by fastforce
next case (CAppl vc1 v1)
  thus ?thesis using assms
  proof(cases ece2)
    case (CAppl vc2 v2)
    then show ?thesis using `ece1 = _` neq by auto
  next
    case (CAppr v2 vc2)
    then show ?thesis using `ece1 = _` neq 
      apply (auto simp add: parallelECE_def)
      apply (rule_tac x= "CAppl vc1 (appVC vc2 b')" in exI)
      apply (rule_tac x= "CAppr (appVC vc1 a') vc2" in exI)
      apply auto
      done
  qed(auto simp add: parallelECE_def)
next
next case (CAppr v1 vc1)
  thus ?thesis using assms
  proof(cases ece2)
    case (CAppr v2 vc2)
    then show ?thesis using `ece1 = _` neq by auto
  next
    case (CAppl vc2 v2)
    then show ?thesis using `ece1 = _` neq 
      apply (auto simp add: parallelECE_def)
      apply (rule_tac x= "CAppr (appVC vc2 b') vc1" in exI)
      apply (rule_tac x= "CAppl vc2 (appVC vc1 a')" in exI)
      apply auto
      done
  qed(auto simp add: parallelECE_def)
next case CDef with neq show ?thesis unfolding parallelECE_def by fastforce
next case COne with neq show ?thesis unfolding parallelECE_def by fastforce
next case CAll with neq show ?thesis unfolding parallelECE_def by fastforce
qed

lemma parallelEC_singleton:
  assumes neq: "parallelECE ece1 ece2"
  shows "parallelEC [ece1] [ece2]"
  using assms
  unfolding parallelECE_def parallelEC_def
  apply (auto simp add: appEC_def)
  apply (elim allE) apply (erule impE) apply (assumption)
  apply (erule_tac x = a' in allE)
  apply (erule_tac x = b' in allE)
  apply (erule exE)+
  apply (rule_tac x= "[ece1']" in exI)
  apply (rule_tac x= "[ece2']" in exI)
  apply auto
  done


lemma parallelEC_append1:
  assumes "parallelEC ec1 ec2"
  shows "parallelEC (ec1 @ ec1') (ec2 @ ec2')"
  unfolding parallelEC_def
proof(intro allI impI; goal_cases)
  case (1 a b a' b')
  hence "appEC ec1 (appEC ec1' a) = appEC ec2 (appEC ec2' b)"
    by (simp add: appEC_append)
  with assms
    obtain ec1'' ec2''
    where "appEC ec1 (appEC ec1' a') = appEC ec2'' (appEC ec2' b)"
      and "appEC ec2 (appEC ec2' b') = appEC ec1'' (appEC ec1' a)"
      and "appEC ec2'' (appEC ec2' b') = appEC ec1'' (appEC ec1' a')"
    unfolding parallelEC_def by blast
  show ?case
  proof(intro exI conjI)
    show "appEC (ec1 @ ec1') a' = appEC (ec2'' @ ec2') b"
      using `appEC ec1 (appEC ec1' a') = _ ` by (simp add: appEC_def)
  next  
    show "appEC (ec2 @ ec2') b' = appEC (ec1'' @ ec1') a"
      using `appEC ec2 (appEC ec2' b') = _ ` by (simp add: appEC_def)
  next
    show "appEC (ec2'' @ ec2') b' =  appEC (ec1'' @ ec1') a'"
      using `appEC ec2'' _ = _` by (simp add: appEC_def)
  qed
qed

lemma parallelCons2:
  assumes "parallelEC ec1 ec2"
  shows "parallelEC (ece # ec1) (ece # ec2)"
  unfolding parallelEC_def
proof(intro allI impI; goal_cases)
  case (1 a b a' b')
  hence "appEC ec1 a = appEC ec2 b" by (simp add: appEC_def)
  with assms
    obtain ec1' ec2'
    where "appEC ec1 a' = appEC ec2' b"
      and "appEC ec2 b' = appEC ec1' a"
      and "appEC ec2' b' = appEC ec1' a'"
    unfolding parallelEC_def by blast
  show ?case
  proof(intro exI conjI)
    show "appEC (ece # ec1) a' = appEC (ece # ec2') b"
      using `appEC ec1 a' = _ ` by (simp add: appEC_def)
  next  
    show "appEC (ece # ec2) b' = appEC (ece # ec1') a"
      using `appEC ec2 b' = _ ` by (simp add: appEC_def)
  next
    show "appEC (ece # ec2') b' = appEC (ece # ec1') a'"
      using `appEC ec2' b' = _` by (simp add: appEC_def)
  qed
qed

section \<open>Local confluence via overlap\<close>

lemma commute_parallel_context:
  assumes "congruent R"
  assumes "parallelEC ec1 ec2"
  assumes "appEC ec1 a = appEC ec2 b"
  assumes "R a a' "
  assumes "R b b'"
  shows "(R OO R\<inverse>\<inverse>) (appEC ec1 a') (appEC ec2 b')" 
  using assms
proof-
  from `parallelEC _ _` `_appEC _ _ = _`
  obtain ec1' ec2'
    where "appEC ec1 a' = appEC ec2' b"
      and "appEC ec2 b' = appEC ec1' a"
      and "appEC ec2' b' = appEC ec1' a'"
    unfolding parallelEC_def by blast
  moreover
  from `R b b'`
  have "R (appEC ec2' b) (appEC ec2' b')" using `congruent R` by auto
  moreover
  from `R a a'`
  have "R (appEC ec1' a) (appEC ec1' a')" using `congruent R` by auto
  ultimately
  show ?thesis by (metis conversepI relcomppI)
qed

lemma cc_local_confluence[case_names red_equiv at_root below_root]:
  assumes "red_equiv (cc R) S"
  assumes at_root: "R\<inverse>\<inverse> OO R \<le> S"
  assumes below_root: "R\<inverse>\<inverse> OO cc' R \<le> S"
  shows "(cc R)\<inverse>\<inverse> OO cc R \<le> S"
proof-
  from `red_equiv _ _`
  have "congruent S"
       "symp S"
       "reflp S"
   and R_left: "cc R OO S \<le> S"
   and R_right: "S OO (cc R)\<inverse>\<inverse> \<le> S"
  unfolding red_equiv_def by auto
{
  fix a1 a2 C1 b C2 c
  assume "appEC C1 a1 = appEC C2 a2"
  assume "R a1 b"
  assume "R a2 c"
  have "S (appEC C1 b) (appEC C2 c)"
    using `appEC _ _ = _`
  proof(induction C1 C2 rule: list_induct2'[case_names Nil_Nil Cons_Nil Nil_Cons Cons_Cons])
    case Nil_Nil
    hence "a1 = a2" by simp
    with `R a1 b` `R a2 c`
    have "(R\<inverse>\<inverse> OO R) b c" by auto
    hence "S b c" using at_root by auto
    then show ?case by simp
  next
    case (Nil_Cons ece ec)
    from `R a2 c` Nil_Cons
    have "cc' R a1 (appEC (ece # ec) c)" by (auto intro: cc'.intros)
    with `R a1 b` Nil Cons
    have "(R\<inverse>\<inverse> OO cc' R) b (appEC (ece # ec) c)" by auto
    hence "S b (appEC (ece # ec) c)" using below_root by auto
    thus ?case using Nil Cons by simp
  next
    case (Cons_Nil ece1 ec1)
    from `R a1 b` Cons_Nil
    have "cc' R a2 (appEC (ece1 # ec1) b)" by (auto intro: cc'.intros)
    with `R a2 c` Nil Cons
    have "(R\<inverse>\<inverse> OO cc' R) c (appEC (ece1 # ec1) b)" by auto
    hence "S c (appEC (ece1 # ec1) b)" using below_root by auto
    hence "S (appEC (ece1 # ec1) b) c" using `symp S` by (auto simp add: symp_def)
    thus ?case using Nil by simp
  next
    case (Cons_Cons ece1 ec1 ece2 ec2)
    then show ?case
    proof (cases "ece1 = ece2")
      case True with Cons_Cons 
      have "appEC ec1 a1 = appEC ec2 a2" by (simp add: appEC_def)
      hence "S (appEC ec1 b) (appEC ec2 c)" using Cons_Cons by auto
      hence "S (appEC [ece1] (appEC ec1 b)) (appEC [ece1] (appEC ec2 c))" using `congruent S` by auto
      then show ?thesis using True by (simp add: appEC_def)
    next
      case False
      hence "parallelECE ece1 ece2" by (rule parallelECE)
      hence "parallelEC [ece1] [ece2]" by (rule parallelEC_singleton)
      from parallelEC_append1[OF this]
      have "parallelEC (ece1 # ec1) (ece2 # ec2)" by simp
      from congruent_cc[of R] this Cons_Cons(2) cc_rootI[of R, OF `R a1 b`] cc_rootI[of R, OF `R a2 c`]
      have "((cc R) OO (cc R)\<inverse>\<inverse> ) (appEC (ece1 # ec1) b) (appEC (ece2 # ec2) c)"
        by (rule commute_parallel_context)
      moreover
      have "(cc R) OO (cc R)\<inverse>\<inverse> \<le> S"
        using R_left R_right `reflp S` by (auto simp add: reflp_def)
      ultimately
      show ?thesis by auto
    qed
  qed

} thus ?thesis using `symp S` by (auto simp add: cc.simps symp_def)
qed

section \<open>The actual rules\<close>

inductive rule_Seq where
  rule_Seq: "rule_Seq (Seq (Val v) e) e"

inductive rule_Unify_Seql where
  rule_Unify_Seql: "rule_Unify_Seql (Uni (Seq e1 e2) e3) (Seq e1 (Uni e2 e3))"


definition "Rs = (rule_Seq \<squnion> rule_Unify_Seql)"
definition "VR = cc Rs"

lemmas Rs_cases = rule_Seq.cases rule_Unify_Seql.cases

lemma transitive_VR[trans]:
  "VR a b \<Longrightarrow> VR b c \<Longrightarrow> VR\<^sup>*\<^sup>* a c"
  by auto

lemma congruent_VR[simp]: "congruent VR"
  unfolding VR_def VR_def by simp


section \<open>Joinability relation\<close>

definition "J = VR\<^sup>*\<^sup>* OO VR\<^sup>*\<^sup>*\<inverse>\<inverse>"

lemma refl_J[simp]: "J x x"
  unfolding J_def by auto

lemma reflp_J[simp]: "reflp J"
  unfolding reflp_def by simp

lemma congruent_J[simp]: "congruent J"
  unfolding J_def VR_def by simp

lemma symp_J[simp]: "symp J"
  unfolding J_def VR_def symp_def by auto

lemma joinI[case_names Peak]:
  assumes "\<And> a b c. R1 a b \<Longrightarrow> R2 a c \<Longrightarrow> S b c"
  shows "R1\<inverse>\<inverse> OO R2 \<le> S"
  using assms by auto

lemma J_VRr[elim]:
  assumes "VR b c"
  assumes "J a c"
  shows "J a b"
  using assms by (auto simp add: J_def VR_def)

lemma J_VRl[elim]:
  assumes "VR c a"
  assumes "J a b"
  shows "J c b"
  using assms by (metis J_VRr sympD symp_J)


lemma J_VRstar[trans]:
  assumes "J a c"
  assumes "VR\<^sup>*\<^sup>* b c"
  shows "J a b"
  using assms by (auto simp add: J_def VR_def)

lemma red_equiv_J: "red_equiv VR J"
  unfolding red_equiv_def
  using converse_relcompp by auto

section \<open>Elementary diagrams at the root\<close>

lemma Seq_Seq: "rule_Seq\<inverse>\<inverse> OO rule_Seq \<le> J"
  by(auto intro!: joinI elim!: rule_Seq.cases)

lemma Unify_Seql_Unify_Seql: "rule_Unify_Seql\<inverse>\<inverse> OO rule_Unify_Seql \<le> J"
  by(auto intro!: joinI elim!: rule_Unify_Seql.cases)

lemma Seq_Unify_Seql: "rule_Seq\<inverse>\<inverse> OO rule_Unify_Seql \<le> J"
  by(auto intro!: joinI elim!: rule_Seq.cases rule_Unify_Seql.cases)

lemma mirror_elementary:
  assumes "R1\<inverse>\<inverse> OO R2 \<le> J"
  shows "R2\<inverse>\<inverse> OO R1 \<le> J"
  by (metis assms converse_relcompp conversep_conversep conversep_mono symp_J symp_conv_conversep_eq)

lemmas root_diagrams
  = Seq_Seq
    Unify_Seql_Unify_Seql
    Seq_Unify_Seql
    Seq_Unify_Seql[THEN mirror_elementary]

section \<open>Elementary diagrams not at the root\<close>

lemma Rs_Val[elim!]:
  assumes "Rs (Val v) c"
  obtains False
  using assms unfolding Rs_def
  by (auto elim: Rs_cases)


lemma cc_Val:
  assumes "cc R (Val v) c"
  obtains
    (atVal) "R (Val v) c"
  | (in_Val) vc a b where "cc R a b" and "v = appVC vc a" and "c = Val (appVC vc b)"
  using assms
  apply (elim cc.cases)
  apply (case_tac C)
   apply simp
  apply (case_tac a)
           apply (auto simp add: appEC_def cc.simps)
  apply blast
  done

lemma cc_Seq:
  assumes "cc R (Seq e1 e2) c"
  obtains 
    (here) "R (Seq e1 e2) c"
  | (left) e1' where "cc R e1 e1'" and "c = Seq e1' e2"
  | (right) e2' where "cc R e2 e2'" and "c = Seq e1 e2'"
  using assms
  apply (elim cc.cases)
  apply (case_tac C)
   apply simp
  apply (case_tac a)
           apply (auto simp add: appEC_def cc.simps)
  apply blast
  done

lemma cc'_Seq:
  assumes "cc' R (Seq e1 e2) c"
  obtains (left) e1' where "cc R e1 e1'" and "c = Seq e1' e2"
  | (right) e2' where "cc R e2 e2'" and "c = Seq e1 e2'"
  using assms
  apply (elim cc'.cases)
  apply (case_tac C)
   apply simp
  apply (case_tac a)
           apply (auto simp add: appEC_def cc.simps)
  apply blast
  done


lemma cc'_Uni:
  assumes "cc' R (Uni e1 e2) c"
  obtains (left) e1' where "cc R e1 e1'" and "c = Uni e1' e2"
  | (right) e2' where "cc R e2 e2'" and "c = Uni e1 e2'"
  using assms
  apply (elim cc'.cases)
  apply (case_tac C)
   apply simp
  apply (case_tac a)
           apply (auto simp add: appEC_def cc.simps)
  apply blast
done


lemma Seq_C: "rule_Seq\<inverse>\<inverse> OO cc' Rs \<le> J"
proof (induction rule: joinI)
  case (Peak a b c)
  then show ?case
  proof(induction)
    case (rule_Seq v e)
    from `cc' Rs (Seq (Val v) e) c`
    show ?case
    proof(induct rule: cc'_Seq)
      case (left v')
      thus ?case
      proof(induct rule: cc_Val)
        case atVal
        thus ?case by auto
      next
        case (in_Val vc a b)
        have "VR (Seq v' e) e" unfolding VR_def Rs_def `v' = _`
          by (intro cc_rootI rule_Seq.intros sup2I1 sup2I2)
        with `c = _`
        have "VR c e" by simp
        thus ?thesis by force
      qed
    next
      case (right e')
      have "VR (Seq (Val v) e') e'" unfolding VR_def Rs_def
        by (intro cc_rootI rule_Seq.intros sup2I1 sup2I2)
      with `c = _`
      have "VR c e'" by simp
      moreover
      from `cc Rs e e'`
      have "VR e e'" unfolding VR_def.
      ultimately
      show ?thesis by force
    qed
  qed
qed

lemma Unify_Seql_C: "rule_Unify_Seql\<inverse>\<inverse>  OO cc' Rs \<le> J"
proof (induction rule: joinI)
  case (Peak a b c)
  then show ?case
  proof(induction)
    case (rule_Unify_Seql e1 e2 e3)
    from `cc' Rs (Uni (Seq e1 e2) e3) c`
    show ?case
    proof(induct rule: cc'_Uni)
      case (left e12')
      from `cc Rs (Seq e1 e2) e12'`
      show ?case
      unfolding `c = _`
      proof(induct rule: cc_Seq)
        case here
        (* Overlap with rule Seq *)
        from `Rs (Seq e1 e2) e12'`
        obtain v where "e1 = Val v" and "e12' = e2"
          unfolding Rs_def by (auto elim: Rs_cases)
        have "VR (Seq (Val v) (Uni e2 e3)) (Uni e2 e3)"
          unfolding VR_def Rs_def by (auto intro: rule_Seq.intros)
        thus ?case unfolding `e12' = _` `e1 = _`
          by fastforce
      next
        case (left e1')
        have "VR (Uni (Seq e1' e2) e3) (Seq e1' (Uni e2 e3))"
          unfolding VR_def Rs_def by (auto intro: rule_Unify_Seql.intros)
        moreover
        from `cc Rs e1 e1'`
        have "VR e1 e1'" unfolding VR_def.
        hence "VR (appEC [CSeql (Uni e2 e3)] e1) (appEC [CSeql(Uni e2 e3)] e1')"
          using  congruent_VR congruentE by blast
        ultimately
        show ?case unfolding `e12' = _` by (fastforce simp add: appEC_def)
      next
        case (right e2')
        have "VR (Uni (Seq e1 e2') e3) (Seq e1 (Uni e2' e3))"
          unfolding VR_def Rs_def by (auto intro: rule_Unify_Seql.intros)
        moreover
        from `cc Rs e2 e2'`
        have "VR e2 e2'" unfolding VR_def.
        hence "VR (appEC [CSeqr e1, CUnil e3] e2) (appEC [CSeqr e1, CUnil e3] e2')"
          using  congruent_VR congruentE by blast
        ultimately
        show ?case unfolding `e12' = _` by (fastforce simp add: appEC_def)
      qed
    next
      case (right e3')
        have "VR (Uni (Seq e1 e2) e3') (Seq e1 (Uni e2 e3'))"
          unfolding VR_def Rs_def by (auto intro: rule_Unify_Seql.intros)
        moreover
        from `cc Rs e3 e3'`
        have "VR e3 e3'" unfolding VR_def.
        hence "VR (appEC [CSeqr e1, CUnir e2] e3) (appEC [CSeqr e1, CUnir e2] e3')"
          using  congruent_VR congruentE by blast
        ultimately
        show ?case unfolding `c = _` by (fastforce simp add: appEC_def)
    qed
  qed
qed

lemmas non_root_diagrams =
  Seq_C
  Unify_Seql_C

theorem local_confluence:
  "VR\<inverse>\<inverse> OO VR \<le> J"
  unfolding VR_def
proof (induct rule: cc_local_confluence)
  case red_equiv show ?case
    using red_equiv_J[unfolded VR_def].
next
  case at_root show ?case
    apply (simp only: Rs_def converse_join relcompp_distrib2 relcompp_distrib)
    apply(intro le_supI root_diagrams)
    done
next
  case below_root show ?case 
  apply (subst Rs_def)
  apply (simp only: converse_join relcompp_distrib2)
  apply (intro le_supI non_root_diagrams)
  done
qed

end