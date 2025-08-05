{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module TimE where
import Epic.List
import FrontEnd.Expr
import ValueS
import ENVS


dE :: SrcEssential -> Ident -> Ident -> [ENV]
dE (Lit (LInt k))                   i x = [ i .=. x /\ x .= Int k ]
dE (EPrim p)                        i x = [ i .=. x /\ x .= Fun (dP p) ]
dE (Variable v) i x | isSrcUnderscore v = [ i .=. x ]
                    | otherwise         = [ i .=. x /\ x .=. v ]
dE (DefineE y t)                    i x = [ x .=. y ] *** dE t i x
dE (DefineV y)                      i x = [ univ ] -- [ i .=. x /\ x .=. y ]
dE (Unify t0 t1)                    i x = dE t0 i x *** dE t1 i x
dE (Choice t0 t1)                   i x = dB t0 i x ++ dB t1 i x
dE (Seq t0 t1)                      i x = dE t0 j y `remv` [j, y] *** dE t1 i x  where (j, y) = fresh2 t0
dE (Where t0 t1)                    i x = dE t0 i x `remv` [j, y] *** dE t1 j y  where (j, y) = fresh2 t1
-- dE (Array ts) i x = ...  needs tuples, i.e., functions
dE (Block t)                        i x = dB t i x
dE (If3 t0 t1 t2)                   i x = -- squash $
{-
  -- According to Koen
  [ hides vs0 (d0 /\ d1)     | d1 <- dE t1 i x ] ++
  [ compl d0 /\ hides vs0 d2 | d2 <- dE t2 i x ]
  where d0 = firstK vs0 (dE t0 j y `remv` [j, y])
        vs0 = bvs t0
        (j, y) = fresh2 (If3 t0 t1 t2)
-}

  -- According to Simon (Koen)
  (([      d0] *** dB t1 i x) `remv` vs0) ++
   [compl (hides vs0 d0)] *** dB t2 i x
  where d0 = first vs0 (dC t0)
        vs0 = bvs t0

{-
  -- According to Tim
  ((bs  *** dB t1 i x) `remv` bvs t0 `remv` [j, y]) ++
  (([b] *** dB t2 i x) `remv` bvs t0 `remv` [j, y])
  where a0 = dE t0 j y
        (j, y) = fresh2 (Array [t0, t1, t2])
        Snoc bs b = go empty a0
        go s [] = [univ \\\ s]
        go s (a:as) = (a \\\ s) : go (s \/ a) as
-}
-- For2
--dE (Range t)                        i x = undefined
--dE (ApplyD t0 t1)                   i x = undefined

dE e                               _ _ = error $ "dE: unimplemented " ++ show e

dB :: SrcEssential -> Ident -> Ident -> [ENV]
dB e i x = dE e i x `remv` bvs e

dC :: SrcEssential -> [ENV]
dC e = dE e i x `remv` [i,x]  where (i, x) = fresh2 e

dP :: PrimOp -> FUN
dP Neg = [funNegate]
dP p = error $ "dP undefined " ++ show p

firstK :: [Ident] -> [ENV] -> ENV
firstK ys []         = empty
firstK ys (env:envs) = env \/ (compl (hides ys env) /\ firstK ys envs)

first :: [Ident] -> [ENV] -> ENV
first xs [] = empty
first xs (d:ds) = d \/ (first xs ds \\\ hides xs d)

squash :: [ENV] -> [ENV]
squash = filter (/= empty)

infixl 8 ***
(***) :: [ENV] -> [ENV] -> [ENV]
s1 *** s2 = [ d1 /\ d2 | d1 <- s1, d2 <- s2 ]

remv :: [ENV] -> [Ident] -> [ENV]
remv s xs = map (hides xs) s

fresh2 :: SrcEssential -> (Ident, Ident)
fresh2 t = (is!!0, is!!1) where is = freshList (getFree t)

bvs :: SrcEssential -> [Ident]
bvs = getVisibleBinders

-------

den :: SrcEssential -> [ENV]
den t = dE (Block t) i res `remv` [i]  where (i, _x) = fresh2 t

res :: Ident
res = Ident noLoc "res"

xx, yy, zz :: Ident
xx = Ident noLoc "x"
yy = Ident noLoc "y"
zz = Ident noLoc "z"
ii = Ident noLoc "i"
jj = Ident noLoc "j"

ex1 :: SrcEssential
ex1 = Lit (LInt 1)

ex2 :: SrcEssential
ex2 = DefineE xx ex1

ex3 :: SrcEssential
ex3 = ex2 `Seq` Variable xx

vxx :: SrcEssential
vxx = Variable xx
k1 :: SrcEssential
k1 = Lit (LInt 1)
k2 :: SrcEssential
k2 = Lit (LInt 2)
k3 :: SrcEssential
k3 = Lit (LInt 3)
k77 :: SrcEssential
k77 = Lit (LInt 77)
infix 4 ===
(===) :: SrcEssential -> SrcEssential -> SrcEssential
x === y = x `Unify` y
infixr 2 |||
(|||) :: SrcEssential -> SrcEssential -> SrcEssential
x ||| y = x `Choice` y

aa = (DefineV xx `Seq` (vxx === k1 ||| vxx === k2))
bb = (vxx === k1 ||| vxx === k2)
tt = vxx === k2
ee = k77

ifTests :: [(SrcEssential, [ENV])]
ifTests = [
  -- if(ex x. x=1 | x=2) { x=2 } else {77}    []
    (If3 (DefineV xx `Seq` (vxx === k1 ||| vxx === k2)) (vxx === k2) k77
    , []
    )
  -- if(ex x. x=1 | x=2) { x=1 } else {77}    [r=1]
  , (If3 (DefineV xx `Seq` (vxx === k1 ||| vxx === k2)) (vxx === k1) k77
    , [res.=Int 1]
    )
  -- if(x=1 | x=3){ x=2 }else{77}             [{{x/={1,3},r=77}}]
  , (If3 (                 (vxx === k1 ||| vxx === k3)) (vxx === k2) k77
    , [xx./=Int 1 /\ xx./= Int 2 /\ res.=Int 77]
    )
  ]

{-
if(x=1 | x=2){ x=2 }else{77}
[{{r=x=2}}u{{x/={1,2},r=77}}]
?xr. r=if(x=1 | x=2){ x=2 }else{77}; x=2; r
[{{r=2}} ]
?xr. r=if(x=1 | x=2){ x=2 }else{77}; x=1; r
[]
if (?x. x = (1|2)) { x } else {fail}
[{{r=1}}]
if (?x. x=(1|2)) { x | x } else {fail}
[{{r=1}},{{r=1}}]
if (?x y, (x=7 | y=3) { x } else fail
[ {{x=7,r=7}} ]
if (x=7 | y=3) { x } else fail
[ {{x=7,r=7}}u
  {{x/=7,y=3,r=x}} ]
-}
      
