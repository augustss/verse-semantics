theory Subst
  imports Syntax
begin

fun liftE :: "nat \<Rightarrow> nat \<Rightarrow> exp \<Rightarrow> exp" ("\<up>\<^sub>e")
and liftV :: "nat \<Rightarrow> nat \<Rightarrow> val \<Rightarrow> val" ("\<up>\<^sub>v")
where
  "\<up>\<^sub>e n k (Val v) = Val (\<up>\<^sub>v n k v)"
| "\<up>\<^sub>e n k (Seq e1 e2) = (Seq (\<up>\<^sub>e n k e1) (\<up>\<^sub>e n k e2))"
| "\<up>\<^sub>e n k (Bar e1 e2) = (Bar (\<up>\<^sub>e n k e1) (\<up>\<^sub>e n k e2))"
| "\<up>\<^sub>e n k (Uni e1 e2) = (Uni (\<up>\<^sub>e n k e1) (\<up>\<^sub>e n k e2))"
| "\<up>\<^sub>e n k (App e1 e2) = (App (\<up>\<^sub>v n k e1) (\<up>\<^sub>v n k e2))"
| "\<up>\<^sub>e n k (Def e) = Def (\<up>\<^sub>e n (k+1) e)"
| "\<up>\<^sub>e n k (One e) = (One (\<up>\<^sub>e n k e))"
| "\<up>\<^sub>e n k (All e) = (All (\<up>\<^sub>e n k e))"
| "\<up>\<^sub>e n k Fail = Fail"
| "\<up>\<^sub>v n k (Var i) = Var (if i < k then i else i + n)"
| "\<up>\<^sub>v n k (Const c) = (Const c)"
| "\<up>\<^sub>v n k (Tup vs) = Tup (map (\<up>\<^sub>v n k) vs)"
| "\<up>\<^sub>v n k (Lam e) = Lam (\<up>\<^sub>e n (k+1) e)"
| "\<up>\<^sub>v n k (Op op) = Op op"

fun substE :: "nat \<Rightarrow> val \<Rightarrow> exp \<Rightarrow> exp"
and substV :: "nat \<Rightarrow> val \<Rightarrow> val \<Rightarrow> val"
where
  "substE n v (Val v1) = Val (substV n v v1)"
| "substE n v (Seq e1 e2) = Seq (substE n v e1) (substE n v e2)"
| "substE n v (Bar e1 e2) = Bar (substE n v e1) (substE n v e2)"
| "substE n v (Uni e1 e2) = Uni (substE n v e1) (substE n v e2)"
| "substE n v (App v1 v2) = App (substV n v v1) (substV n v v2)"
| "substE n v (Def e) = Def (substE (n+1) (\<up>\<^sub>v 1 0 v) e)"
| "substE n v (One e) = One (substE n v e)"
| "substE n v (All e) = All (substE n v e)"
| "substE n v Fail = Fail"
| "substV n v (Var i) = (if i = n then v else Var i)"
| "substV n v (Const k) = Const k"
| "substV n v (Tup vs) = Tup (map (substV n v) vs)"
| "substV n v (Lam e) = Lam (substE (n+1) (\<up>\<^sub>v 1 0 v) e)"
| "substV n v (Op k) = Op k"


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


lemma occursE_bars[simp]:
  "occursE n (bars es) \<longleftrightarrow> (\<exists> e \<in> set es. occursE n e)"
by (induction es rule: bars.induct) auto

lemma occursE_seqs[simp]:
  "occursE n (seqs es) \<longleftrightarrow> (\<exists> e \<in> set es. occursE n e)"
by (induction es rule: seqs.induct) auto

lemma occursE_liftE: "occursE n (\<up>\<^sub>e k j e) \<longleftrightarrow>
    (if n < j then occursE n e else if n < j + k then False else  occursE (n - k) e)"
  and occursV_liftV: "occursV n (\<up>\<^sub>v k j v) \<longleftrightarrow>
    (if n < j then occursV n v else if n < j + k then False else  occursV (n - k) v)"
  by (induction k j e and k j v arbitrary: n and n rule: liftE_liftV.induct)
     (auto simp add: Suc_diff_le)

lemma occursE_substE[simp]:
  "occursE i (substE n v e) \<longleftrightarrow> 
  (i \<noteq> n \<and> occursE i e) \<or> (occursE n e \<and> occursV i v)"
and occursV_substV[simp]:
  "occursV i (substV n v v') \<longleftrightarrow> 
  (i \<noteq> n \<and> occursV i v') \<or> (occursV n v' \<and> occursV i v)"
  by (induction n v e and n v v' arbitrary: i and i rule: substE_substV.induct)
     (auto simp add: occursV_liftV)
  
end
