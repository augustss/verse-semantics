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


type_synonym red = "exp \<Rightarrow> exp \<Rightarrow> bool"



end