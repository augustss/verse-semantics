theory Contexts
imports
  Main
  Syntax
begin

section \<open>Contexts\<close>

text \<open>Context element\<close>

datatype vce =
  CTup "val list" "val list"

type_synonym vc = "vce list"

datatype ece =
  CVal vc (* Implicit CLam at the end*)
| CSeql (* *) exp
| CSeqr exp (* *) 
| CBarl (* *) exp
| CBarr exp (* *) 
| CUnil (* *) exp
| CUnir exp (* *) 
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

inductive isXE :: "ece \<Rightarrow> bool" where
  "isXE (CSeql e2)"
| "isXE (CSeqr e1)"
| "isXE (CUnil e2)"
| "isXE (CUnir e1)"

definition isX :: "ec \<Rightarrow> bool" where
  "isX ec \<longleftrightarrow> (\<forall> ece \<in> set ec. isXE ece)"

end