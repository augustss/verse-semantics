# Making β-reduction sound: three relational denotational models for PCF

## Overview: the versions of PCF

This directory develops Plotkin's PCF (call-by-name, de Bruijn) with a
*relational* set-theoretic semantics in `ZFSet`, across three denotational
models that share one syntax. Each model is a single `.v` file; this note
(`PCF.md`) is the design narrative tying them together. The thread running
through all of them is **when is β-reduction sound?** — and each model answers
it a little better than the last.

| File | What it is | Divergence is… | β-reduction | Determinism |
|---|---|---|---|---|
| [`PCFSyntax.v`](PCFSyntax.v) | shared syntax, typing, CBN small-step reduction (`lift`/`subst`/`step`) — no semantics | — | — | — |
| [`PCFStrict.v`](PCFStrict.v) | the original *strict* relational model: a type denotes its set of values, a term the set it may produce | the empty set `∅` | **unsound** (`S_beta_unsound`) | relational (`fix` is multivalued) |
| [`PCFLifted.v`](PCFLifted.v) | *ground-lifted*: a bottom `⊥` at the ground types; `succ`/`pred`/`iszero` are ⊥-strict; `fix` takes its *unique* fixed point | `⊥` at ground types, still `∅` at others | sound for **single-valued** arguments (`S_beta_sound`) | subsingleton (`evalTm_subsingleton`) — one value or `∅` |
| [`PCFPointed.v`](PCFPointed.v) | *type-indexed bottom*: a `⊥` at **every** type, via a type-directed evaluator; the total/Scott-style model | `⊥` at every type — never `∅` | β's strictness obstruction is gone (subst metatheory pending) | **total** (`eval_total`) — exactly one value |

Each model `Require Import`s `PCFSyntax` and adds its own `evalTy`/`evalTm`.
`PCFStrict.v` also carries the big-step semantics and contextual equivalence;
the small-step `step` it shares with the others is the relation whose β rule
this note tracks.

The rest of this note is the design story: §1 the problem in `PCFStrict.v`,
§2–§4 the `PCFLifted.v` fix and its one caveat, §5 the `PCFPointed.v` total
model.

## 1. The problem in the original model

`PCFStrict.v` gives PCF a *relational* set-theoretic semantics in `ZFSet`:

- a type denotes the **set of its values** (`evalTy`);
- a term denotes the **set of values it may produce**, with the empty set
  `∅` standing for divergence (`evalTm`);
- a well-typed term is a subset of its type (`soundness`).

In that model the small-step rule

```
S_beta :  (λ:T. body) e2  -->  subst 0 e2 body
```

is **unsound**: there exist `e --> e'` with `evalTm e ρ ≠ evalTm e' ρ`
(`S_beta_unsound`). The file is careful to note that *evaluation order is not
the cause* — β is unsound under both call-by-name and call-by-value. There are
two independent obstructions, and both stem from `∅` being overloaded to mean
both "no value" and "divergence":

### Obstruction 1 — application is strict

```coq
evalTm (tapp e1 e2) ρ = f ← evalTm e1 ρ ;; v ← evalTm e2 ρ ;; image f {| v |}
```

The bind `v ← evalTm e2 ρ` ranges over the argument's value-set. If the
argument denotes `∅` (divergence), the *whole* application is `∅` — even when
the function discards its argument. Operationally `(λx:ι. 0) loop` converges to
`0`, but denotationally it is `∅` (`cbn_const_loop_den`).

### Obstruction 2 — arrows are total

```coq
evalTy (TArr T1 T2) = Pi (evalTy T1) (fun _ => evalTy T2)
```

`Pi` is the set of **total** functional graphs. A λ whose body diverges on some
input has *no* total realization, so it denotes `∅` — even though a λ is a
perfectly good value operationally. `λy:ι. loop` is the witness
(`evalTm_div_fun`): an operational value that denotes `∅`. Feeding it to a
constant function reproduces the β counterexample without any divergent
*ground* argument (`tot_app_den`, `S_beta_unsound`).

### Why these break β specifically

`evalTm` is compositional, so soundness of `S_beta` reduces to a **substitution
lemma**

```coq
evalTm (subst 0 s body) ρ  =  a ← evalTm s ρ ;; evalTm body (env_cons a ρ).
```

This lemma is **false** in the original model. Take `body = tzero` (which
ignores its variable) and `s = λy:ι. loop`. Then `subst 0 s tzero = tzero`, so
the left side is `{| 0 |}`; but `evalTm s ρ = ∅`, so the right side is
`a ← ∅ ;; {| 0 |} = ∅`. The strict bind over an empty argument-set destroys a
result that does not even depend on the argument. β fails for exactly this
reason.

## 2. The fix: make divergence a *value*

The root cause is that `∅` carries two meanings and the relational binds
propagate it strictly. We separate the two meanings by adding a distinguished
**bottom element** `⊥` to the *ground* types, representing divergence as a
genuine value. A divergent ground term then denotes `{| ⊥ |}` (not `∅`), and —
crucially — a divergent-bodied λ is realised by the all-`⊥` graph, which is
already a total function into the lifted codomain. So no separate `⊥` is needed
at arrow types: lifting the ground types suffices to dissolve both obstructions.

Concretely (`bot : ZFSet` is any fixed set outside every value set — we take
`bot := Omega`, which by foundation is not a natural, and is not a functional
graph either):

### Change 1 — pointed *ground* types; arrows are plain `Pi`

```coq
Fixpoint evalTy (T : Ty) : ZFSet :=
  match T with
  | TNat       => Omega ∪ {| bot |}
  | TBool      => bools ∪ {| bot |}                          (* bools = {0} ∪ {1} *)
  | TArr T1 T2 => Pi (evalTy T1) (fun _ => evalTy T2)        (* no ∪ {| bot |} *)
  end.
```

An arrow is a **total** function over the (lifted) argument domain into the
(lifted) codomain. `λy:ι. loop` is realised by the graph mapping every input to
`⊥ ∈ evalTy ι` — a genuine element of `Pi`, so it denotes a nonempty set, not
`∅` (Obstruction 2 dissolved). Because arrow types contain only real function
graphs (never the raw `bot`), `tapp` and `tlam` need *no* `⊥` case, and
`soundness` is the original proof plus a few `⊥`-cases for the ground rules.

### Change 2 — application clause is unchanged

```coq
evalTm (tapp e1 e2) ρ = f ← evalTm e1 ρ ;; v ← evalTm e2 ρ ;; image f {| v |}
```

No edit is needed. A function value `f` is total over its (lifted) domain, so it
has a defined value `f(⊥)`; a *non-strict* function (e.g. the constant `λx. 0`)
sends `⊥` to a proper value. Once an argument like `λy:ι. loop` denotes a real
(all-`⊥`) function rather than `∅`, the application bind no longer collapses.
The non-strictness of application is thus a *consequence* of lifting, not a
special case in the clause.

### Change 3 — the ground primitives `succ`/`pred`/`iszero` propagate ⊥

The strictness that was wrongly living in `tapp` belongs to the ground
primitives, which now map `⊥` to `⊥`. Their results are ground (so the `⊥`
lands in a lifted type); writing the decision in the comprehension style
already used for `iszero`:

```coq
| tsucc e => v ← evalTm e ρ ;; strictN (natZSucc v) v
| tpred e => v ← evalTm e ρ ;; strictN (natZPred v) v

Definition strictN k v := ⦃ _ ∈ {| k |} | v ∈ Omega ⦄ ∪ ⦃ _ ∈ {| bot |} | v ∉ Omega ⦄.
```

so `strictN k v = {| k |}` for a proper natural `v` and `{| ⊥ |}` for `v = ⊥`.
`iszero` likewise gains a `⊥`-branch and excludes `⊥` from its existing
branches.

**`tif` is left unchanged.** A conditional is genuinely strict in its guard, and
its two existing comprehensions `⦃_ ∈ ⟦e1⟧ | b = 1⦄ ∪ ⦃_ ∈ ⟦e2⟧ | b = 0⦄`
already yield `∅` when the guard `b = ⊥` (both equalities fail). That `∅` is the
*correct* denotation of a divergent-guard `if`, and it cannot break β (which
never rewrites an `if`); using a raw `⊥`-branch here would instead be unsound at
arrow result types, where `⊥ ∉ Pi`.

`tvar`, `tlam`, `tapp` and the constructors are unchanged.

### Change 4 — `tfix` denotes the *unique* fixed point

`PCFStrict.v` lets `tfix` denote the *set of all* fixed points of the function value.
In the lifted model `tfix` instead keeps a value only when the fixed point is
**unique**:

```coq
| tfix e => f ← evalTm e ρ ;;
    ⦃ a ∈ dom f | ⟨a,a⟩ ∈ f /\ (forall b, ⟨b,b⟩ ∈ f -> b = a) ⦄
```

so it denotes a singleton (the lone fixed point) or `∅` (none, or several). This
keeps every term single-valued — exactly what the substitution lemma needs under
binders (see §3) — and it agrees with operational behaviour: a function with no
unique fixed point loops. `loop = fix (λx:ι. succ x)` still denotes `{| ⊥ |}`
(`eval_loop`): `⊥` is its only fixed point.

## 3. Why β becomes sound — and the one caveat

The strictness obstructions (1 and 2) — the ones the original file attributes
β's failure to — are exactly what lifting removes. Concretely:

**The substitution lemma now holds for a single-valued argument.** When
`evalTm s ρ = {| a |}` (a deterministic value),

```coq
Lemma evalTm_subst_sing e : forall k s ρ a,
  evalTm s ρ = {| a |} ->
  evalTm (subst k s e) ρ = evalTm e (env_ins k a ρ).
```

is provable by induction on `e`, with a companion `lift` lemma
`evalTm (lift d e) (env_ins d b ρ) = evalTm e ρ`. The `k = 0` instance is the β
case: `evalTm (subst 0 e2 body) ρ = evalTm body (env_cons a ρ)`.

In the *original* model this fails on `body = tzero`, `s = λy:ι. loop`, because
`evalTm s ρ = ∅` collapses the right side to `∅`. Under lifting,
`evalTm (λy:ι. loop) ρ` is the singleton `{| g |}` (the all-`⊥` function), so
the substitution goes through and the previously-fatal witness is repaired.

**β-soundness** for a single-valued, in-domain argument (`evalTm e2 ρ = {| a |}`
with `a ∈ evalTy T`):

```
evalTm (tapp (tlam T body) e2) ρ
  = f ← Pi(evalTy T, φ) ;; image f {| a |}     (φ a = ⟦body⟧[a]; bind over {a})
  = φ a                                         (⋃_{f∈Pi} image f {|a|} = φ a, a∈domain)
  = evalTm body (env_cons a ρ)
  = evalTm (subst 0 e2 body) ρ.                 (substitution lemma)
```

This discharges the exact term that refuted β before
(`(λx:ι→ι. 0) (λy:ι. loop)`): it now denotes `{| 0 |}`, equal to its reduct.

### Why the argument must be single-valued — and why that is now free

The substitution lemma is restricted to a **single-valued** `s` for a real
reason, not just convenience. Substitution copies `e2` under the binders of
`body`; when `e2` is *multivalued*, each copy may independently range over
`evalTm e2 ρ`, whereas application picks one value for all copies. Formally, the
`tlam` case of the induction needs

```
Pi(D, fun c => a ← S ;; G c a)  =  a ← S ;; Pi(D, fun c => G c a)     (S = evalTm e2 ρ)
```

and *Pi-of-a-union ≠ union-of-Pis* unless `S` is a singleton. So β would be
unsound for a multivalued argument.

In `PCFStrict.v` arguments *can* be multivalued — but only through `fix`, which there
denotes the *set of all* fixed points (`fix (λx:ι. x)` denotes all of `ω ∪ {⊥}`,
`fixid_multivalued`). That is the same multivaluedness the original file flags as
the "deeper obstruction," now seen to bear on β directly. **Change 4 removes it
at the source**: with `tfix` denoting the *unique* fixed point, every other
construct preserves single-valuedness, so every well-typed term denotes a
singleton or `∅` — formalized as `evalTm_subsingleton` (the determinism lemma).
For a **terminating** argument (a singleton) this discharges the hypotheses of
`S_beta_sound`, so β holds.

It does **not** make β *unconditional*, because Change 4 also turns a `fix` with
no unique fixed point into `∅` — reintroducing divergence-as-`∅`. A divergent
argument then denotes `∅`, and the *strict* application collapses to `∅` even
when the body discards it: `(λy:ι. 0) (fix (λx:ι. x))` reduces to `0` but denotes
`∅` (since `fix (λx:ι. x)` now denotes `∅`, not `ω ∪ {⊥}`). So Change 4 trades
the *multivalued*-`fix` obstruction for an *empty*-`fix` one; closing the latter
needs divergence to denote `⊥` at every type (a type-indexed bottom), which the
ground-only lift does not supply for a non-unique `fix`.

`S_fix` (fix unfolding) is sound under the corresponding condition — the
abstracted body is a single total function — via the `evalTm_fix_unfold`
equation, whose side condition the lifted, inhabited `Pi` now supplies.

## 4. What is preserved, re-proved, and still open

**Preserved.** Type soundness (`evalTm e ρ ⊆ evalTy T`) still holds — arrows are
still total functions, now over lifted domains, and the proof gains only
`⊥`-cases. The value/primitive small-step rules stay sound. The relational style
is untouched, except that `tfix` now selects the unique fixed point rather than
the whole set.

**Re-proved / new (all machine-checked in `PCFLifted.v`).** Type `soundness`
is re-proved for the lifted model. The new results are the `lift` lemma
(`evalTm_lift`), the single-valued substitution lemma (`evalTm_subst_sing`), and
`S_beta_sound` (for single-valued in-domain arguments) — which *replaces*
`PCFStrict.v`'s `S_beta_unsound`. As the concrete payoff, `eval_loop` computes
`⟦loop⟧ = {| ⊥ |}`, `eval_lam_loop` shows `λy:ι. loop` denotes a real function
(not `∅`, contrast `evalTm_div_fun`), and `tot_app_lifted` / `beta_step_sound`
flip `PCFStrict.v`'s witness `tot_app_den` from `∅` to `{| 0 |}` — i.e. that β step
now preserves denotations. (All rest only on the development's standard axioms:
the quotient axioms, prop/functional extensionality, and Hilbert choice.)

**Determinism (`evalTm_subsingleton`).** Every well-typed term denotes a
subsingleton (a singleton or `∅`), proved by induction on typing: the bind/`Pi`
constructors preserve single-valuedness (`iUnion_subsingleton`,
`Pi_subsingleton`), the ⊥-strict primitives have mutually exclusive guards
(`strictN_subsingleton`, and the `iszero`/`if` guard analyses), and the
unique-fixed-point `tfix` is single-valued by construction.

**Still open (for `PCFLifted.v`).** `S_beta_sound` keeps its single-valuedness
hypothesis: by determinism it is dischargeable exactly for *terminating*
arguments (those denoting a singleton). Unconditional β additionally needs the
*empty*-`fix` case above repaired — divergence denoting `⊥` at every type. That
is exactly what `PCFPointed.v` does next.

## 5. The total model: a type-indexed bottom (`PCFPointed.v`)

The residual `∅` in `PCFLifted.v` comes from divergence being `⊥` only at
*ground* types: a `tfix` with no unique fixed point, or a `tif` with a divergent
guard, has nowhere to put a bottom at an arrow type, so it falls back to `∅`.
[`PCFPointed.v`](PCFPointed.v) removes this by giving **every** type a bottom.

- **Type-indexed bottom** `botT : Ty → ZFSet` — `⊥` at a ground type, and the
  *all-`⊥` function* `{⟨a, botT T2⟩ | a ∈ evalTy T1}` at an arrow. `botT T ∈
  evalTy T` for all `T` (`botT_in_evalTy`).
- **Type-directed evaluator** `eval Γ e ρ` — identical to `PCFLifted.evalTm`
  except the two divergent cases emit `botT` of the right type instead of `∅`
  (the type is recovered by a syntax-directed `infer Γ e`, correct on
  well-typed terms: `infer_sound`).

The headline is **`eval_total`**: *every well-typed term denotes exactly one
value* — `has_type Γ e T → models Γ ρ → ∃ a, a ∈ evalTy T ∧ eval Γ e ρ = {| a |}`.
There is no `∅` any more. This single theorem subsumes both type soundness
(`eval_sound`) and determinism (`eval_single_valued`), and upgrades the latter
from `PCFLifted`'s *subsingleton* to a genuine *singleton* (`eval_nonempty`:
every well-typed term converges to a value). For instance `fix (λx:ι. x)` now
denotes `{| ⊥ |}`, so `(λy:ι. 0) (fix (λx:ι. x))` denotes `{| 0 |}` — the
`PCFLifted` β counterexample is gone.

With `eval_total` the strictness obstruction to β is fully removed: a well-typed
argument is always a singleton. What remains for an *unconditional*
`eval Γ (tapp (tlam T1 body) e2) ρ = eval Γ (subst 0 e2 body) ρ` is only the
standard de Bruijn substitution metatheory for the type-directed `eval` (a
`lift` lemma and a context-threading substitution lemma; `infer` commutes with
`lift`/`subst` unconditionally, so no `∅` is involved) — pure syntactic
bookkeeping, noted at the end of `PCFPointed.v`.

(`eval_total` rests on the development's standard axioms plus excluded middle —
used once, classically, to split on whether a `fix` has a unique fixed point —
which is consistent with the existing `epsilon`/`prop_ext`.)
