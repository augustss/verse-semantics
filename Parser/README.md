# README

## Build

```sh
$ stack build
$ cabal v2-build
```

## Run

```sh
$ stack run
$ cabal v2-run
```

## Test

(or just leave out the arguments/options)

```
$ stack test --fast --test-arguments -rewrite
$ cabal v2-test --test-options -rewrite
```

or to use the plain `Makefile`

```sh
$ make test
$ make testr # popl-rewrite-rules
$ make testt # tim-parser
$ make testf # fresh-rewrite-rules
```

## TODO: Fresh Rules

```haskell
TRACE: NFT :

[
  ("ELIM-CST",
  def $y1 in
    {($y1 = 2);
      def $c1 in
        {($c1 = one {def x in {(x = def x in {($y1 = x); (isInt(x); x)}); (\_.add(arr{x, 1}))} | (\_.fail)}); $c1(arr{})
        }
    }),
  ("ELIM-CST",
  def $a2 in
    {($a2 = (\$y1.def $c1 in {($c1 = one {def x in {(x = (\x.isInt(x); x)($y1)); (\_.add(arr{x, 1}))} | (\_.fail)}); $c1(arr{})})); def $y1 in {($y1 = 2); def $c1 in {($c1 = one {def x in {(x = def x in {($y1 = x); (isInt(x); x)}); (\_.add(arr{x, 1}))} | (\_.fail)}); $c1(arr{})}}})]

 [("ELIM-CST",

    def $y1 in
      {($y1 = 2);
      def $c1 in
        {($c1 = one { def x in {(x = def x in {($y1 = x); (isInt(x); x)}); (\_.add(arr{x, 1}))} | (\_.fail)});
          $c1(arr{})}}),

  ("ELIM-CST",def $a2 in {($a2 = (\$y1.def $c1 in {($c1 = one {def x in {(x = (\x.isInt(x); x)($y1)); (\_.add(arr{x, 1}))} | (\_.fail)}); $c1(arr{})})); def $y1 in {($y1 = 2); def $c1 in {($c1 = one {def x in {(x = def x in {($y1 = x); (isInt(x); x)}); (\_.add(arr{x, 1}))} | (\_.fail)}); $c1(arr{})}}})
 ]
```

```haskell
--------------------------------------------------------------------------------
-- from TRS/Main.hs
--------------------------------------------------------------------------------

rules :: Bool -> ERule
rules True  = rulesPOPL
rules False = rulesFRESH

runFresh :: Expr -> [(String, Expr)]
runFresh = normalFormsFuel 99 rulesFRESH

eFail :: Expr
eFail = lam Fail

def :: String -> Expr -> Expr
def = DEF . ident

lam :: Expr -> Expr
lam = LAM (ident "_")

iVAR :: String -> Expr
iVAR = VAR . ident

iVar :: String -> Value
iVar = Var . ident

iDEF :: String -> Expr -> Expr
iDEF = DEF . ident

eStuck :: Expr
eStuck =
  def "$y1"
    ( iVAR "$y1" :=: INT 2 :>:
      iDEF "$c1"
        (
        (iVAR "$c1" :=: One (iDEF "x"
                              (( iVAR "x" :=: (iDEF "x1" ((iVAR "$y1" :=: iVAR "x1") :>: ((IsINT :@: iVar "x1") :>: iVAR "x1"))) )
                                :>: (lam (ADD :@: (VARR [iVar "x", VINT 1]))))
                              :|:
                              eFail
                           )
        )
        :>:
        ( iVar "$c1" :@: VARR [] )
        )
    )

eStuck1 :: Expr
eStuck1 =
  def "$y1"
    ( iVAR "$y1" :=: INT 2 :>:
      iDEF "$c1"
        (
        (iVAR "$c1" :=: One (iDEF "x"
                              (( iVAR "x" :=: (iDEF "x1" ((INT 2 :=: iVAR "x1") :>: ({- (iVar "isInt" :@: iVar "x1") :>: -} iVAR "x1"))) )
                                :>: (lam (iVar "add" :@: (VARR [iVar "x", VINT 1]))))
                              :|:
                              eFail
                           )
        )
        :>:
        ( iVar "$c1" :@: VARR [] )
        )
    )

eStuck2 =
  iDEF "$c1"
        (
        (iVAR "$c1" :=: One (iDEF "x"
                              (( iVAR "x" :=: (iDEF "x1" ((iVAR "$y1" :=: iVAR "x1") :>: ((iVar "isInt" :@: iVar "x1") :>: iVAR "x1"))) )
                                :>: (lam (iVar "add" :@: (VARR [iVar "x", VINT 1]))))
                              :|:
                              eFail
                           )
        )
        :>:
        ( iVar "$c1" :@: VARR [] )
        )

eStuck3 =
  iDEF "$c1"
        (iVAR "$c1" :=: One (iDEF "x"
                              (( iVAR "x" :=: INT 2 )
                                :>: (lam (iVar "add" :@: (VARR [iVar "x", VINT 1]))))
                              :|:
                              eFail
                            )
        )
        :>:
        ( iVar "$c1" :@: VARR [] )


fillContext :: VContext -> Expr
fillContext ctx = ctx (iVar "#")

eOne :: Expr
eOne =
  One (iDEF "x"
          (( iVAR "x" :=: INT 2)
          :>: (lam (ADD :@: (VARR [iVar "x", VINT 1]))))
          :|:
          eFail
          )

dumpCtx c e = mapM_ print [ (ctx (iVar "#") , v) | (ctx, v) <- c e]

eBody1 = iDEF "x1" ((iVAR "$y1" :=: iVAR "x1") :>: ((IsINT :@: iVar "x1") :>: iVAR "x1"))
eBody0 = iDEF "x1" ((iVAR "$y1" :=: iVAR "x1") :>: (IsINT :@: iVar "x1"))

eBody2 = iDEF "x1" eb2'
eb2' = ((iVAR "x1" :=: INT 2) :>: ((IsINT :@: iVar "x1") :>: iVAR "x1"))
```

P-ADD;ELIM-CST;DEREF-S-K;P-MUL;ELIM-CST;APP-BETA;ELIM-CST;DEREF-H;ONE-CHOICE;SEQ;P-IsINT;ELIM-CST;DEREF-S-K;CONJ-SEMI;UNIFY-SEQR;ELIM-CST;APP-BETA;ELIM-CST;DEREF-S-K;APP-BETA;ELIM-CST;DEREF-H;ELIM-CST;APP-BETA;ELIM-CST;DEREF-H;ONE-CHOICE;SEQ;P-IsINT;ELIM-CST;DEREF-S-K;DEREF-S-K;DEREF-S-K;CONJ-SEMI;UNIFY-SEQR;ELIM-CST;DEREF-S-K;DEREF-S-K;APP-BETA;ELIM-CST;DEREF-S-K;APP-BETA;ELIM-CST;DEREF-H;SWAP-C;CONJ-SEMI;UNIFY-SEQR: 6

P-ADD;ELIM-CST;DEREF-S-K;P-MUL;ELIM-CST;APP-BETA;ELIM-CST;DEREF-H;ONE-CHOICE;SEQ;P-IsINT;ELIM-CST;DEREF-S-K;CONJ-SEMI;UNIFY-SEQR;ELIM-CST;DEREF-S-K;APP-BETA;ELIM-CST;DEREF-S-K;APP-BETA;ELIM-CST;DEREF-H;ELIM-CST;APP-BETA;ELIM-CST;DEREF-H;ONE-CHOICE;SEQ;P-IsINT;ELIM-CST;DEREF-S-K;DEREF-S-K;CONJ-SEMI;UNIFY-SEQR;ELIM-CST;DEREF-S-K;DEREF-S-K;APP-BETA;ELIM-CST;DEREF-S-K;APP-BETA;ELIM-CST;DEREF-H;SWAP-C;CONJ-SEMI;UNIFY-SEQR: 8

TRACE: ds :

def x in {def $r1 in {(5 = ((x = ($r1; (\x.isInt(x); x)($r1))); x)); 5}}


def
5 = (x = y);
x
```haskell
def a in {def r1 in {(5 = ((a = (r1; (def x. x = r1; isInt(x); x))); a)); 5}}





  def x in {
    def $r1 in {
      (5 = (x = ($r1;
                  (\x.isInt(x); x)($r1)
                )
           )
      );
      5
    }
  }
```