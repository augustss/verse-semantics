theory Timbda1Semantics
  imports ZF
begin

text \<open> 
  \section{Syntax of Timbda1}
  Here we define the inductive syntax for the Timbda1 language. 
\<close>

consts expr :: i
datatype expr =
    Var ("i \<in> nat")
  | Con ("n \<in> nat")
  | Nat
  | App ("e\<^sub>1 \<in> expr", "e\<^sub>2 \<in> expr")
  | Img ("e \<in> expr")
  | Fail
  | Choice ("e\<^sub>1 \<in> expr", "e\<^sub>2 \<in> expr")
  | Equal ("e\<^sub>1 \<in> expr", "e\<^sub>2 \<in> expr")
  | Lam ("e\<^sub>1 \<in> expr", "e\<^sub>2 \<in> expr")
  | Let ("e\<^sub>1 \<in> expr", "e\<^sub>2 \<in> expr")
  | Any
  | Pair ("e\<^sub>1 \<in> expr", "e\<^sub>2 \<in> expr")
  | Fst ("e \<in> expr")
  | Snd ("e \<in> expr")

text \<open> 
  The fixed-point combinator \texttt{Fix} does not need to be a primitive 
  syntactic construct; it is derived as an abstract macro via intersection.
\<close>

definition Fix :: "i \<Rightarrow> i" where
  "Fix(e) \<equiv> Equal(Img(e), Img(Any))"

text \<open> 
  \section{Value Space and Large Cardinals}
  We axiomatize two nested Grothendieck Universes ($U_0 \in U_1$). 
  $U_0$ provides the bounded domain for the \texttt{Any} construct, 
  while $U_1$ acts as our total value universe. This is equivalent to 
  working in the Von Neumann universe $V_\kappa$ where $\kappa$ is the 
  first strongly inaccessible cardinal.
\<close>

axiomatization U0 :: i and U1 :: i where
  U0_in_U1: "U0 \<in> U1" and
  U1_trans: "x \<in> U1 \<Longrightarrow> x \<subseteq> U1" and
  U1_nat:   "nat \<in> U1" and
  U1_Pow:   "x \<in> U1 \<Longrightarrow> Pow(x) \<in> U1" and
  U1_Un:    "x \<in> U1 \<Longrightarrow> \<Union>(x) \<in> U1" and
  U1_Rep:   "\<And>I f. \<lbrakk> I \<in> U1; \<And>x. x \<in> I \<Longrightarrow> f(x) \<in> U1 \<rbrakk> \<Longrightarrow> RepFun(I, f) \<in> U1"

definition Val :: i where
  "Val \<equiv> U1"

definition Env :: i where
  "Env \<equiv> nat -> Val"

definition ext_env :: "[i, i] \<Rightarrow> i" where
  "ext_env(\<rho>, v) \<equiv> \<lambda>i \<in> nat. nat_case(v, \<lambda>m. \<rho> ` m, i)"

lemma ext_env_type [TC]:
  "\<lbrakk> \<rho> \<in> Env; v \<in> Val \<rbrakk> \<Longrightarrow> ext_env(\<rho>, v) \<in> Env"
  unfolding Env_def ext_env_def
  by (auto simp add: lam_type nat_case_type)

text \<open> 
  \section{Relational Evaluator Semantics}
  The interpreter takes an expression and returns a function mapping 
  an environment to a set of (input, output) pairs.
\<close>

consts eval :: "i \<Rightarrow> i"
primrec
  "eval(Var(i)) = (\<lambda>\<rho> \<in> Env. {\<langle>\<rho> ` i, \<rho> ` i\<rangle>})"
  
  "eval(Con(n)) = (\<lambda>\<rho> \<in> Env. {\<langle>n, n\<rangle>})"
  
  "eval(Nat) = (\<lambda>\<rho> \<in> Env. {\<langle>{\<langle>a, a\<rangle> . a \<in> nat}, {\<langle>a, a\<rangle> . a \<in> nat}\<rangle>})"
  
  "eval(App(e\<^sub>1, e\<^sub>2)) = (\<lambda>\<rho> \<in> Env. 
     (\<Union>p \<in> (eval(e\<^sub>1) ` \<rho>). \<Union>q \<in> (eval(e\<^sub>2) ` \<rho>). 
        {\<langle>b, b\<rangle> . b \<in> range(snd(p)), \<langle>snd(q), b\<rangle> \<in> snd(p)}))"
        
  "eval(Img(e)) = (\<lambda>\<rho> \<in> Env. (\<Union>p \<in> (eval(e) ` \<rho>). snd(p)))"
  
  "eval(Fail) = (\<lambda>\<rho> \<in> Env. 0)"
  
  "eval(Choice(e\<^sub>1, e\<^sub>2)) = (\<lambda>\<rho> \<in> Env. (eval(e\<^sub>1) ` \<rho>) \<union> (eval(e\<^sub>2) ` \<rho>))"
    
  "eval(Equal(e\<^sub>1, e\<^sub>2)) = (\<lambda>\<rho> \<in> Env. (eval(e\<^sub>1) ` \<rho>) \<inter> (eval(e\<^sub>2) ` \<rho>))"
  
  "eval(Lam(e\<^sub>1, e\<^sub>2)) = (\<lambda>\<rho> \<in> Env. 
     { \<langle> ({ \<langle>snd(fst(x)), fst(snd(x))\<rangle> . x \<in> h }), 
         ({ \<langle>fst(fst(x)), snd(snd(x))\<rangle> . x \<in> h }) \<rangle> . 
       h \<in> Pi(eval(e\<^sub>1) ` \<rho>, \<lambda>p. eval(e\<^sub>2) ` ext_env(\<rho>, snd(p))) })"

  "eval(Let(e\<^sub>1, e\<^sub>2)) = (\<lambda>\<rho> \<in> Env. 
     (\<Union>q \<in> (eval(e\<^sub>1) ` \<rho>). eval(e\<^sub>2) ` ext_env(\<rho>, snd(q))))"

  "eval(Any) = (\<lambda>\<rho> \<in> Env. {\<langle> {\<langle>a, a\<rangle> . a \<in> U0}, {\<langle>a, a\<rangle> . a \<in> U0} \<rangle>})"

  "eval(Pair(e\<^sub>1, e\<^sub>2)) = (\<lambda>\<rho> \<in> Env. 
     (\<Union>p \<in> (eval(e\<^sub>1) ` \<rho>). \<Union>q \<in> (eval(e\<^sub>2) ` \<rho>). 
        {\<langle>\<langle>fst(p), fst(q)\<rangle>, \<langle>snd(p), snd(q)\<rangle>\<rangle>}))"

  "eval(Fst(e)) = (\<lambda>\<rho> \<in> Env.
     (\<Union>x \<in> (eval(e) ` \<rho>). {\<langle>fst(fst(x)), fst(snd(x))\<rangle>}))"

  "eval(Snd(e)) = (\<lambda>\<rho> \<in> Env. 
     (\<Union>x \<in> (eval(e) ` \<rho>). {\<langle>snd(fst(x)), snd(snd(x))\<rangle>}))"
end
