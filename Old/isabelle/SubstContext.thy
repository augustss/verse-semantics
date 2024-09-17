theory SubstContext
imports Subst Contexts
begin

class has_depth = has_vars +
  fixes depth :: "'a \<Rightarrow> nat"
  assumes depth_bind[simp]: "depth (bind f x) = depth x"

lemma depth_del[simp]: "depth (renumber f x) = depth x"
  unfolding renumber_def by simp

instantiation list :: (has_depth) has_vars 
begin

fun bind_list where
  "bind_list f [] = []"
| "bind_list f (x # xs) = bind f x # bind_list (lifted (depth x) 0 f) xs"

fun occurs_list where
  "occurs_list n [] = False"
| "occurs_list n (x # xs) \<longleftrightarrow> occurs n x \<or> occurs_list (depth x + n) xs"

instance
proof (rule Subst.has_vars_class.intro, (intro_classes)[1], induction rule: has_varsI)
  case (Bind_bind f g xs)
  show ?case
    by (induction g xs arbitrary: f rule:bind_list.induct) (auto simp add: bind_bind)
next
  case (Occurs_bind i f xs)
  show ?case
    apply (induction f xs arbitrary: i rule:bind_list.induct)
     apply (auto simp add: occurs_bind lifted_def renumber_def bumpAt_def)
    apply (metis add_diff_inverse_nat)
    done
next
  case (Bind_id_iff f xs)
  show ?case
    by (induction f xs rule:bind_list.induct) (auto simp add: bind_id_iff)
qed
end
                                     
instantiation list :: (has_depth) has_depth
begin
definition depth_list :: "'a list \<Rightarrow> nat" where
  "depth_list xs = sum_list (map depth xs)"
instance
proof
  fix f and xs :: "'a list"
  show "depth (bind f xs) = depth xs"
    by (induction xs arbitrary: f) (auto simp add: depth_list_def)
qed
end

lemmas renumber_list_simps[simp] =
  bind_list.simps[where f = "\<lambda> n. Var (f n)" for f, folded renumber_def, unfolded bind_lifted_Var_f]

lemma depth_replicate[simp]: "depth (replicate n xs) = n * depth xs"
  by (auto simp add: depth_list_def sum_list_replicate)


instantiation vce :: has_vars 
begin
fun bind_vce :: "(nat \<Rightarrow> val) \<Rightarrow> vce \<Rightarrow> vce" where
  "bind_vce f (CTup vs1 vs2) = CTup (map (bind f) vs1) (map (bind f) vs2)"

fun occurs_vce :: "nat \<Rightarrow> vce \<Rightarrow> bool" where
  "occurs_vce n (CTup vs1 vs2) \<longleftrightarrow> (\<exists> v \<in> set vs1. occurs n v) \<or> (\<exists> v \<in> set vs2. occurs n v)"

instance
proof (rule Subst.has_vars_class.intro, (intro_classes)[1], induction rule: has_varsI)
  case (Bind_bind f g x)
  show ?case by (cases x)(simp add: bind_bind)
next
  case (Occurs_bind i f x)
  show ?case by (cases x)(auto simp add: occurs_bind)
next
  case (Bind_id_iff f x)
  show ?case by (cases x)(auto 4 4 simp add: bind_id_iff intro!: map_idI)
qed
end

lemmas renumber_vce_simps[simp] = bind_vce.simps[where f = "\<lambda> n. Var (f n)" for f, folded renumber_def]

lemma occurs_appVCE[simp]:
  "occurs n (appVCE vce v) \<longleftrightarrow> occurs n v \<or> occurs n vce"
  by (cases vce) auto

instantiation vce :: has_depth
begin
fun depth_vce :: "vce \<Rightarrow> nat" where "depth_vce vce = 0"
instance by standard simp
end

lemma occurs_appVC[simp]:
  "occurs n (appVC vc e) \<longleftrightarrow> occurs (1 + n) e \<or> occurs n vc"
  by (induction vc) (auto simp add: appVC_def appVC'_def )

instantiation ece :: has_vars
begin
fun bind_ece :: "(nat \<Rightarrow> val) \<Rightarrow> ece \<Rightarrow> ece" where
  "bind_ece f (CVal vc) = CVal (bind f vc)"
| "bind_ece f CDef = CDef"
| "bind_ece f (CAppl vc v2) = CAppl (bind f vc) (bind f v2)"
| "bind_ece f (CAppr v1 vc) = CAppr (bind f v1) (bind f vc)"
| "bind_ece f (CSeql e2) = CSeql (bind f e2)"
| "bind_ece f (CSeqr e1) = CSeqr (bind f e1)"
| "bind_ece f (CBarl e2) = CBarl (bind f e2)"
| "bind_ece f (CBarr e1) = CBarr (bind f e1)"
| "bind_ece f (CUnil e2) = CUnil (bind f e2)"
| "bind_ece f (CUnir e1) = CUnir (bind f e1)"
| "bind_ece f COne = COne"
| "bind_ece f CAll = CAll"

definition "occurs_ece n ece = occurs n (appECE ece Fail)"

instance
proof (rule Subst.has_vars_class.intro, (intro_classes)[1], induction rule: has_varsI)
  case (Bind_bind f g ece)
  show ?case
    by (cases ece) (auto simp add: bind_bind)
next
  case (Occurs_bind i f ece)
  show ?case
    by (cases ece) (auto simp add: occurs_bind occurs_ece_def)
next
  case (Bind_id_iff f ece)
  show ?case by (cases ece) (auto simp add: occurs_ece_def bind_id_iff)
qed
end

instantiation ece :: has_depth
begin
fun (sequential) depth_ece :: "ece \<Rightarrow> nat" where
  "depth_ece (CVal _) = 1"
| "depth_ece CDef = 1"
| "depth_ece (CAppl _ _) = 1"
| "depth_ece (CAppr _ _) = 1"
| "depth_ece _ = 0"
instance
proof
  fix f and ece :: ece
  show "depth (bind f ece) = depth ece"
    by (cases ece) auto
qed
end

lemmas renumber_ece_simps[simp] = bind_ece.simps[where f = "\<lambda> n. Var (f n)" for f, folded renumber_def]

lemma depth_isXE[simp]: "isXE ece \<Longrightarrow> depth ece = 0"
  by (induction ece) (auto elim: isXE.cases)

lemma depth_isX[simp]: "isX ec \<Longrightarrow> depth ec = 0"
  by (induction ec) (auto simp add: isX_def depth_list_def)

lemma occurs_appECE[simp]:
  "occurs n (appECE ece e) \<longleftrightarrow> occurs n ece \<or> occurs (depth ece + n) e"
  by (cases ece) (auto simp add: occurs_ece_def)

lemma occursE_appEC[simp]:
  "occurs n (appEC ec e) \<longleftrightarrow> occurs n ec \<or> occurs (depth ec + n) e"
  apply (induction n ec rule: occurs_list.induct)
   apply (auto simp add: appEC_def depth_list_def)
  apply (metis ab_semigroup_add_class.add_ac(1) group_cancel.add2)
  apply (simp add: ab_semigroup_add_class.add_ac(1) add.commute)
  done

end