module FrontEnd.TRSAdapter(rewrite, coreToTrs) where
import Data.Char(toLower)
import Data.Function(on)
import Data.List(nubBy)
import Data.Maybe
import qualified TRS.Bind as T
import qualified Rules.Core as T
import Rules.Equiv(equiv)
import Rules.Systems(ESystem)
import TRS.TRS
import TRS.System(preProcess, rules, postProcess, ruleEnv)
import TRS.Traced(toList)
import FrontEnd.Expr(Ident(..), noLoc)
import FrontEnd.Core
import FrontEnd.Error
import FrontEnd.Flags

import Debug.Trace
import Epic.Print

-- XXX use graph normal form when needed

rewrite :: Flags -> ESystem -> Core -> [Core]
rewrite flg sys = map (trsToCore . sub flg sys . rtrace) . elimDup sys . subs flg sys . nf n (rules sys) . preProcess sys . coreToTrs
 where
  trsFlags       = (ruleEnv sys){ T.tfUnderLambda = fUnderLambda flg }
  n              = fRewriteSteps flg
  tr             = fTrace flg
  latex          = fLatex flg
  nf | fDfs flg  = normalFormFuelTrace trsFlags
     | otherwise = \ x y z -> map toList $ normalFormsFuelTrace trsFlags x y z
{-
  checkOne [x]   = [x]
  checkOne nes   = trace (unlines $
                          "Multiple:" :
                          map (\(s,e) -> s ++ ": " ++ prettyShow (trsToCore e) ++ "\n+++++") (map head nes))
                         nes
-}
  rtrace xs | not tr = res
            | latex = trace (latexTrace xs) res
            | otherwise = trace (showReductionTrace (prettyShow . trsToCore) xs) res
    where res = snd (head xs)

  showReductionTrace sh xs = msg
    where
      msg = "***** Reduction trace\n" ++ (unlines $ map pr $ reverse xs) ++ "*****\n"
      pr (s, a) = s ++ ":\n" ++ sh a ++ "\n----------\n"

elimDup :: ESystem -> [Trace T.Expr] -> [Trace T.Expr]
elimDup sys = nubBy (equiv sys `on` (snd . head))

subs :: Flags -> ESystem -> [Trace T.Expr] -> [Trace T.Expr]
subs flg sys ts
  | fFinalInline flg =
    let ts' = [(e', t) | t@((_, e):_) <- ts, let e' = postProcess sys e]
        ts'' = nubBy ((==) `on` fst) ts'
    in  map snd ts''
  | otherwise  = ts

sub :: Flags -> ESystem -> T.Expr -> T.Expr
sub flg sys | fFinalInline flg = postProcess sys
            | otherwise = id

coreToTrs :: Core -> T.Expr
coreToTrs (CVar i) = T.Var $ coreToTrsI i
coreToTrs (CInt i) = T.Int i
coreToTrs CRat{} = undefined
coreToTrs (CPrim s) = T.Op $ fromMaybe (error $ "unknown op: " ++ s) $ lookup s ops
  where ops = map (\ (x,y) -> (y, x)) allOps
coreToTrs (CArray vs) = T.Arr $ map coreToTrs vs
coreToTrs (CLam x e) = T.Lam $ T.Bind (coreToTrsI x) (coreToTrs e)
coreToTrs (CUnify e1 e2) = coreToTrs e1 T.:=: coreToTrs e2
coreToTrs (CSeq []) = undefined
coreToTrs (CSeq [e]) = coreToTrs e
coreToTrs (CSeq (e:es)) = coreToTrs e T.:>: coreToTrs (CSeq es)
coreToTrs (CApply v1 v2) = coreToTrs v1 T.:@: coreToTrs v2
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
--coreToTrs e@CApply{} = impossible e

coreToTrsV :: Core -> T.Value
coreToTrsV e = case coreToTrs e of T.Val v -> v; _ -> undefined

coreToTrsI :: Ident -> T.Ident
coreToTrsI (Ident _ s) = T.Name s

trsToCore :: T.Expr -> Core
trsToCore (T.Var i) = CVar (trsToCoreI i)
trsToCore (T.Int i) = CInt i
trsToCore (T.Op op) = CPrim $ fromMaybe undefined $ lookup op allOps
trsToCore (T.Arr vs) = CArray $ map trsToCore vs
trsToCore (T.Lam (T.Bind x e)) = CLam (trsToCoreI x) (trsToCore e)
trsToCore (e1 T.:=: e2) = CUnify (trsToCore e1) (trsToCore e2)
trsToCore ee@(_ T.:>: _) = CSeq $ map trsToCore $ flat ee
  where flat (e1 T.:>: e2) = flat e1 ++ flat e2
        flat e = [e]
trsToCore (e1 T.:|: e2) = CBar (trsToCore e1) (trsToCore e2)
trsToCore (e1 T.:@: e2) = CApply (trsToCore e1) (trsToCore e2)
trsToCore T.Fail = CFail
trsToCore ee@T.Def{} = flat [] ee
  where flat vs (T.Def (T.Bind x e)) = flat (vs ++ [x]) e
        flat vs e = CDef (map trsToCoreI vs) (trsToCore e)
trsToCore (T.One e) = COne $ trsToCore e
trsToCore (T.All e) = CAll $ trsToCore e
trsToCore T.Wrong = CWrong "unknown"
trsToCore (T.Split e f g) = CSplit (trsToCore e) (trsToCore f) (trsToCore g)

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
  (T.Cons, "cons$")
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
    expr _ (T.Var x) = ident x
    expr _ (T.Int k) = showString (show k)
    expr _ (T.Op o)  = showString (show o)
    expr _ (T.Arr []) = showString "tup ()"
    expr _ (T.Arr vs) = showString "tup (" . foldr1 (\ x s -> x . showString "," . s) (map (expr 0) vs) . showString ")"
    expr p (T.Lam (T.Bind x e)) = showParen (p>0) $ showString "lam " . ident x . showString " (" . expr 0 e . showString ")"

    expr p (a T.:=: b) = showParen (p > 3) $ expr 4 a . showString " == " . expr 4 b
    expr p (a T.:>: b) = showParen (p > 1) $ expr 2 a . showString " ; "  . expr 1 b
    expr p (a T.:|: b) = showParen (p > 2) $ expr 3 a . showString " `choose` " . expr 2 b
    expr _ (a T.:@: b) = showString "apply1 " . expr 4 a . showString "(" . expr 0 b . showString ")"
    expr _ T.Fail      = showString "fail"
    expr p e@T.Def{}   = showString "def (" . shxs . showString ") (" . expr p a . showString ")"
      where (xs, a) = getXs e
            getXs (T.DEF y b) = (y:ys, c) where (ys, c) = getXs b
            getXs b = ([], b)
            shxs = foldr1 (\ x s -> x . showString " ^^ " . s) (map ident xs)
    expr _ (T.One a)   = showString "one (" . expr 0 a . showString ")"
    expr _ (T.All a)   = showString "all (" . expr 0 a . showString ")"
    expr _ _ = undefined

    ident = showString . show
