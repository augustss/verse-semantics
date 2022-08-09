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
| "\<up>\<^sub>e n k (One e) = (One e)"
| "\<up>\<^sub>e n k (All e) = (All e)"
| "\<up>\<^sub>v n k (Var i) = Var (if i < k then i else i + n)"
| "\<up>\<^sub>v n k (Const c) = (Const c)"
| "\<up>\<^sub>v n k (Tup vs) = Tup (map (\<up>\<^sub>v n k) vs)"
| "\<up>\<^sub>v n k (Lam e) = Lam (\<up>\<^sub>e n (k+1) e)"


end
