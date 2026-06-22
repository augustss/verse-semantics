(* Relational semantics *)
From Stdlib Require Import ssreflect.

Require Import ZFSet.
Require Import ZFNotation.
Require Import Syntax.

(* 
Functor denotational semantics of a term

    ε⟦t        ⟧ρ : P(U_1×U_1)
    ε⟦x        ⟧ρ = {⟨ρ.x,ρ.x⟩}
    ε⟦x:=at    ⟧ρ = {⟨j,y⟩:ε⟦at⟧ρ | ρ.x=y}
    ε⟦i~>x:at  ⟧ρ = {⟨j,y⟩:ε⟦at⟧ρ | ρ.x=y, ρ.i=j}
    ε⟦any      ⟧ρ = {⟨id_U,id_U⟩}
    ε⟦_        ⟧ρ = id_{U_1}
    ε⟦at,bt    ⟧ρ = ε⟦at⟧ρ × ε⟦bt⟧ρ
    ε⟦atUbt    ⟧ρ = ε⟦at⟧ρ ∪ ε⟦bt⟧ρ
    ε⟦at=bt    ⟧ρ = ε⟦at⟧ρ ∩ ε⟦bt⟧ρ
    ε⟦λ_{at} bt⟧ρ = { (h,f)
                    | f∈(⋃_{aρ∈ρ-at} dom(ε⟦at⟧aρ))→U
                    , h∈(⋃_{aρ∈ρ-at} ran(ε⟦at⟧aρ))→U
                    , ∀(⟨au,av⟩∈ε⟦at⟧aρ | aρ∈ρ-at).
                          ∃(⟨bu,bv⟩∈ε⟦bt⟧bρ | bρ∈aρ-bt). ⟨au,bv⟩∈f ∧ ⟨av,bu⟩∈h }
    ε⟦{at}     ⟧ρ = {⟨s,s⟩ where s={⋃_{aρ∈ρ-at} ε⟦at⟧aρ}
    ε⟦ft(pt)   ⟧ρ = {⟨r,r⟩ | f∈ran(ε⟦ft⟧ρ), p∈ran(ε⟦pt⟧ρ), r∈U_1, (p,r)∈f}
    ε⟦at; bt   ⟧ρ = ∪_{⟨i,x⟩∈ε⟦at⟧ρ} ε⟦bt⟧ρ
    ε⟦∈t       ⟧ρ = ∪ ran(ε⟦t⟧ρ)
    ε⟦at↝bt   ⟧ρ = {⟨x,z⟩ | ∃ x y z. ⟨x,y⟩∈ε⟦at⟧ρ, ⟨y,z⟩∈ε⟦bt⟧ρ}

Syntactic sugar
    ε⟦ :bt     ⟧ρ = ε⟦∈bt⟧ρ
    ε⟦x:bt     ⟧ρ = ε⟦x:=∈bt⟧ρ
    ε⟦at∈bt    ⟧ρ = ε⟦at~>∈bt⟧ρ
    ...def'n desugaring

*)



(*** Evaluator.

   [Etyp] (type projection [:e]) of the original development is [Eimg]
   in [Syntax]; [Ebind] ([e1 :>= e2]) was added to [Syntax] for this
   evaluator.  [Eany] / [Etype] denote the polymorphic identity function
   value ⟨env, anyId, anyId⟩ / ⟨env, is_type, is_type⟩.  The remaining
   [Syntax] constructors that this triple semantics does not interpret
   fall through to [∅]. ***)

Fixpoint eval (e : Expr) (env : Env) {struct e} : ZFSet :=
  match e with
  | Econ k =>
      {| ⟨ env , natZ k , natZ k ⟩ |}
  | Evar x =>
      {| ⟨ env , env_lookup env x , env_lookup env x ⟩ |}
  | Enat =>
      {| ⟨ env , natId , natId ⟩ |}
  | Elam e1 e2 =>
      h ← Π[ t ∈ eval e1 env ] eval e2 (proj1 t) ;;
        let f := '⟨ ab , cd ⟩ ← h ;; {| ⟨ proj3 ab , proj2 cd ⟩ |} in
        let g := '⟨ ab , cd ⟩ ← h ;; {| ⟨ proj2 ab , proj3 cd ⟩ |} in
        {| ⟨ env , f , g ⟩ |}
  | Eapp e1 e2 =>
      '⟨ r , _ , b ⟩  ← eval e1 env ;;
      '⟨ ka , va ⟩    ← b ;;
      '⟨ rs , sa , _ ⟩ ← eval e2 r ;;
        ⦃ _ ∈ {| ⟨ rs , va , va ⟩ |} | ka = sa ⦄
  | Ebind e1 e2 =>
      '⟨ r , _ , b ⟩ ← eval e1 env ;; eval e2 (env_cons b r)
  | Eimg e1 =>
      '⟨ r , _ , b ⟩ ← eval e1 env ;;
      '⟨ ka , va ⟩   ← b ;;
        {| ⟨ r , ka , va ⟩ |}
  | Efail => ∅
  | Echoice e1 e2 =>
      ('⟨ _ , a , b ⟩ ← eval e1 env ;; {| ⟨ env , a , b ⟩ |})
      ∪ ('⟨ _ , a , b ⟩ ← eval e2 env ;; {| ⟨ env , a , b ⟩ |})
  | Eadd e1 e2 =>
      '⟨ _ , _ , b1 ⟩ ← eval e1 env ;;
      '⟨ _ , _ , b2 ⟩ ← eval e2 env ;;
        let s := natZAdd b1 b2 in
        {| ⟨ env , s , s ⟩ |}
  | Eequal e1 e2 =>
      eval e1 env ∩ eval e2 env
  | Eany => {| ⟨ env , anyId , anyId ⟩ |}
  | Etype => {| ⟨ env , is_type , is_type ⟩ |}
  (* [x := e]: each result of [e] re-conses its value [b] onto its own
     environment (binding [x]); the [a]/[b] components are kept. *)
  | Eassign _ e =>
      '⟨ r , a , b ⟩ ← eval e env ;; {| ⟨ env_cons b r , a , b ⟩ |}
  (* [e1 ; e2]: run [e1] for its environments, then [e2] under each. *)
  | Eseq e1 e2 =>
      '⟨ r , _ , _ ⟩ ← eval e1 env ;; eval e2 r
  (* [x := e1 ; e2] is [e1]'s assignment composed with sequencing — see
     [eval_let_seq_assign].  This lets a single [Elam] domain sequence
     several binders, e.g. [t := :type ; x := :t]. *)
  | Elet _ e1 e2 =>
      '⟨ r , _ , b ⟩ ← eval e1 env ;; eval e2 (env_cons b r)
  | _ => ∅
  end.