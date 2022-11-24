module Parser.TRSAdapter(rewrite, coreToTrs) where
import Data.Char(toLower)
import Data.Function(on)
import Data.List(nubBy)
import Data.Maybe
import qualified TRS.TRSCore as T
import qualified TRS.Bind as T
import qualified TRS.RulesPOPL as RulesPOPL
import qualified TRS.RulesPLDI as RulesPLDI
import TRS.TRS
import TRS.Traced(toList)
import Parser.Expr(Ident(..), noLoc)
import Parser.Core
import Parser.Error
import Parser.Flags

import Debug.Trace
import Epic.Print

rewrite :: Flags -> Core -> [Core]
rewrite flg = map (trsToCore . sub flg . rtrace) . checkOne . subs flg . nf n (rules flg) . ds flg . coreToTrs
 where
  trsFlags       = T.TRSFlags { T.tfUnderLambda = fUnderLambda flg, T.tfAlias = fAlias flg, T.tfUnifyEq = fUnifyEq flg }
  n              = fRewriteSteps flg
  tr             = fTrace flg
  latex          = fLatex flg
  nf | fDfs flg  = normalFormFuelTrace trsFlags
     | otherwise = \ x y z -> map toList $ normalFormsFuelTrace trsFlags x y z
  checkOne [x]   = [x]
  checkOne nes   = trace (unlines $
                          "Multiple:" :
                          map (\(s,e) -> s ++ ": " ++ prettyShow (trsToCore e) ++ "\n+++++") (map head nes))
                         nes
  rtrace xs | not tr = res
            | latex = trace (latexTrace xs) res
            | otherwise = trace (showReductionTrace (prettyShow . trsToCore) xs) res
    where res = snd (head xs)

  showReductionTrace sh xs = msg
    where
      msg = "***** Reduction trace\n" ++ (unlines $ map pr $ reverse xs) ++ "*****\n"
      pr (s, a) = s ++ ":\n" ++ sh a ++ "\n----------\n"

ds :: Flags -> T.Expr -> T.Expr
ds flg
  | fFresh flg = RulesPLDI.dsFreshFP
  | otherwise  = id

subs :: Flags -> [Trace T.Expr] -> [Trace T.Expr]
subs flg ts
  | fFinalInline flg && fFresh flg =
    let ts' = [(e', t) | t@((_, e):_) <- ts, let e' = RulesPLDI.finalSubst e]
        ts'' = nubBy ((==) `on` fst) ts'
    in  map snd ts''
  | otherwise  = ts

sub :: Flags -> T.Expr -> T.Expr
sub flg | fFinalInline flg && fFresh flg = RulesPLDI.finalSubst
        | otherwise = id

rules :: Flags -> RulesPOPL.ERule
rules flg
  | fFresh flg = RulesPLDI.rulesPLDI -- <> RulesPLDI.rulesStructural
  | otherwise  = RulesPOPL.rulesPOPL

coreToTrs :: Core -> T.Expr
coreToTrs (CValue v) = T.Val (coreToTrsV v)
coreToTrs (CUnify e1 e2) = coreToTrs e1 T.:=: coreToTrs e2
coreToTrs (CSeq []) = undefined
coreToTrs (CSeq [e]) = coreToTrs e
coreToTrs (CSeq (e:es)) = coreToTrs e T.:>: coreToTrs (CSeq es)
coreToTrs (CApplyVV v1 v2) = coreToTrsV v1 T.:@: coreToTrsV v2
coreToTrs (CBar e1 e2) = coreToTrs e1 T.:|: coreToTrs e2
coreToTrs CFail = T.Fail
coreToTrs (COne e) = T.One $ coreToTrs e
coreToTrs (CAll e) = T.All $ coreToTrs e
coreToTrs (CDef [] e) = coreToTrs e
coreToTrs (CDef (i:is) e) = T.Def $ T.Bind (coreToTrsI i) (coreToTrs $ CDef is e)
coreToTrs (CSucceeds e) = coreToTrs e  -- XXX temporarily
coreToTrs CWrong{} = T.Wrong
coreToTrs (CSplit e f g) = T.Split (coreToTrs e) (coreToTrsV f) (coreToTrsV g)
coreToTrs e@CMacro{} = impossible e
coreToTrs e@CLambda{} = impossible e
coreToTrs e@CApply{} = impossible e

coreToTrsV :: Value -> T.Value
coreToTrsV (Var i) = T.Var $ coreToTrsI i
coreToTrsV (HNF h) = T.HNF $ coreToTrsH h

coreToTrsH :: HNF -> T.HNF
coreToTrsH (HInt i) = T.Int i
coreToTrsH HRat{} = undefined
coreToTrsH (HPrim s) = T.Op $ fromMaybe (error $ "unknown op: " ++ s) $ lookup s ops
  where ops = map (\ (x,y) -> (y, x)) allOps
coreToTrsH (HArray vs) = T.Arr $ map coreToTrsV vs
coreToTrsH (HLam x e) = T.Lam $ T.Bind (coreToTrsI x) (coreToTrs e)

coreToTrsI :: Ident -> T.Ident
coreToTrsI (Ident _ s) = T.Name s

trsToCore :: T.Expr -> Core
trsToCore (T.Val v) = CValue $ trsToCoreV v
trsToCore (e1 T.:=: e2) = CUnify (trsToCore e1) (trsToCore e2)
trsToCore ee@(_ T.:>: _) = CSeq $ map trsToCore $ flat ee
  where flat (e1 T.:>: e2) = flat e1 ++ flat e2
        flat e = [e]
trsToCore (e1 T.:|: e2) = CBar (trsToCore e1) (trsToCore e2)
trsToCore (e1 T.:@: e2) = CApplyVV (trsToCoreV e1) (trsToCoreV e2)
trsToCore T.Fail = CFail
trsToCore ee@T.Def{} = flat [] ee
  where flat vs (T.Def (T.Bind x e)) = flat (vs ++ [x]) e
        flat vs e = CDef (map trsToCoreI vs) (trsToCore e)
trsToCore (T.One e) = COne $ trsToCore e
trsToCore (T.All e) = CAll $ trsToCore e
trsToCore T.Wrong = CWrong "unknown"
trsToCore (T.Split e f g) = CSplit (trsToCore e) (trsToCoreV f) (trsToCoreV g)

trsToCoreV :: T.Value -> Value
trsToCoreV (T.Var i) = Var (trsToCoreI i)
trsToCoreV (T.HNF h) = HNF (trsToCoreH h)

trsToCoreH :: T.HNF -> HNF
trsToCoreH (T.Int i) = HInt i
trsToCoreH (T.Op op) = HPrim $ fromMaybe undefined $ lookup op allOps
trsToCoreH (T.Arr vs) = HArray $ map trsToCoreV vs
trsToCoreH (T.Lam (T.Bind x e)) = HLam (trsToCoreI x) (trsToCore e)

trsToCoreI :: T.Ident -> Ident
trsToCoreI (T.Name s) = Ident noLoc s
trsToCoreI (T.Prim i) = Ident noLoc $ "$" ++ show i

allOps :: [(T.Op, String)]
allOps = [
{-
  (T.Gt, "intGT$"),
  (T.Le, "intGE$"),
  (T.Lt, "intLT$"),
  (T.Le, "intLE$"),
  (T.Ne, "intNE$"),
-}
  (T.Gt, "in'>'"),
  (T.Ge, "in'>='"),
  (T.Lt, "in'<'"),
  (T.Le, "in'<='"),
  (T.Ne, "in'<>'"),
  (T.Add, "in'+'"),
  (T.Sub, "in'-'"),
  (T.Mul, "in'*'"),
  (T.Div, "in'/'"),
  (T.Neg, "pre'-'"),
  (T.Plus, "pre'+'"),
  (T.IsInt, "isInt$"),
  (T.MapAp, "mapAp$"),
  (T.Cons, "cons$"),
  (T.NotFcn, "notFcn$")
  ]

----------------------------------------------

latexTrace :: Trace T.Expr -> String
latexTrace = unlines . map one . reverse
  where one (s, e) = "  & \\movestoR{" ++ map toLower s ++ "} & |" ++ showLatex e ++ "| \\\\"

-- Show with the special notation used in the papers.
-- The string excludes the || used to indicate preprocessing.
showLatex :: T.Expr -> String
showLatex ee = expr 0 ee ""
  where
    expr :: Int -> T.Expr -> ShowS
    expr p (T.Val v) = value p v
    expr p (a T.:=: b) = showParen (p > 3) $ expr 4 a . showString " == " . expr 4 b
    expr p (a T.:>: b) = showParen (p > 1) $ expr 2 a . showString " ; "  . expr 1 b
    expr p (a T.:|: b) = showParen (p > 2) $ expr 3 a . showString " `choose` " . expr 2 b
    expr _ (a T.:@: b) = showString "apply1 " . value 4 a . showString "(" . value 0 b . showString ")"
    expr _ T.Fail      = showString "fail"
    expr p e@T.Def{}   = showString "def (" . shxs . showString ") (" . expr p a . showString ")"
      where (xs, a) = getXs e
            getXs (T.DEF y b) = (y:ys, c) where (ys, c) = getXs b
            getXs b = ([], b)
            shxs = foldr1 (\ x s -> x . showString " ^^ " . s) (map ident xs)
    expr _ (T.One a)   = showString "one (" . expr 0 a . showString ")"
    expr _ (T.All a)   = showString "all (" . expr 0 a . showString ")"
    expr _ _ = undefined

    value :: Int -> T.Value -> ShowS
    value _ (T.Var x) = ident x
    value p (T.HNF h) = hnf p h

    ident = showString . show

    hnf :: Int -> T.HNF -> ShowS
    hnf _ (T.Int k) = showString (show k)
    hnf _ (T.Op o)  = showString (show o)
    hnf _ (T.Arr []) = showString "tup ()"
    hnf _ (T.Arr vs) = showString "tup (" . foldr1 (\ x s -> x . showString "," . s) (map (value 0) vs) . showString ")"
    hnf p (T.Lam (T.Bind x e)) = showParen (p>0) $ showString "lam " . ident x . showString " (" . expr 0 e . showString ")"
