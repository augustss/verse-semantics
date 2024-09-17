theory Syntax 
imports Main
begin

datatype op = add_op | gt_op

datatype "exp" =
  Val val
| Seq exp exp
| Bar exp exp
| Uni exp exp
| App val val
| Def exp
| One exp
| All exp
| Fail
and val =
  Var nat
| Const int
| Tup "val list"
| Lam exp
| Op op

abbreviation VLet :: "val \<Rightarrow> exp \<Rightarrow> exp" where
  "VLet v e \<equiv> Def (Seq (Uni (Val (Var 0)) (Val v)) e)"

inductive isHNF where
  "isHNF (Const k)"
| "isHNF (Tup vs)"
| "isHNF (Lam e)"
| "isHNF (Op op)"

inductive_cases isHNF_Var[elim]:
  "isHNF (Var v)"

type_synonym red = "exp \<Rightarrow> exp \<Rightarrow> bool"

section \<open>Smart constructors\<close>

fun bars :: "exp list \<Rightarrow> exp" where
  "bars [] = Fail"
| "bars [x] = x"
| "bars (x#xs) = Bar x (bars xs)"

fun seqs :: "exp list \<Rightarrow> exp" where
  "seqs [] = Fail"
| "seqs [x] = x"
| "seqs (x#xs) = Seq x (seqs xs)"

lemma seqs_Cons[simp]: "xs \<noteq> [] \<Longrightarrow> seqs (x#xs) = Seq x (seqs xs)"
  by (induction xs) auto

end