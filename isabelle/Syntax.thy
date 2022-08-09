theory Syntax 
imports Main
begin

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

type_synonym red = "exp \<Rightarrow> exp \<Rightarrow> bool"



end