module FrontEnd.TRSAdapter(rewrite, coreToTrs, trsToCore, asTRS) where
import Data.Char(toLower)
import Data.Function(on)
import Data.List(nubBy)
import Data.Maybe
import qualified TRS.Bind as T
import qualified Rules.Core as T
import Rules.Equiv(normalForm)
import Rules.Systems(ESystem)
import TRS.NormalForm(normalFormFuelTrace, normalFormsFuelTrace, NormResult(..))
import TRS.System(preProcess, postProcess, ruleEnv, sname)
import TRS.Traced(Traced, term, toList)
import FrontEnd.Core
import FrontEnd.Error
import FrontEnd.Eval
import FrontEnd.Flags
import GHC.Stack

import Debug.Trace
import Epic.Print

asTRS :: (T.Expr -> T.Expr) -> Core -> Core
asTRS f = trsToCore . f . coreToTrs

-- XXX use graph normal form when needed

evaluate :: T.RuleEnv T.Expr -> Core -> [Core]
evaluate tflg e = [eval flg e]
  where flg = EFlags { underLambda = T.tfUnderLambda tflg, traceEval = T.tfTrace tflg, steps = T.tfRewriteSteps tflg }

rewrite :: Flags -> ESystem -> Core -> [Core]
rewrite flg asys | sname sys == "eval" = evaluate (ruleEnv sys)
                 | otherwise = force . map (trsToCore . sub flg sys . rtrace)
                . map toList
                . nrToList
                . nrTrace sys
                . subsNR flg sys
                . elimDupNR sys
                . nf
                . start
                . preProcess sys (ruleEnv sys)
                . coreToTrs
 where
  sys            = asys{ruleEnv = (ruleEnv asys){ T.tfUnderLambda = fUnderLambda flg
                                                , T.tfTrace = fTrace flg, T.tfRewriteSteps = n } }
  n              = fRewriteSteps flg
  tr             = fTrace flg
  latex          = fLatex flg
  nf | fDfs flg  = normalFormFuelTrace  sys n
     | otherwise = normalFormsFuelTrace sys n
  nrToList NormResult{ nrDone = xs, nrLeft = left } | null left || fNoFuelStop flg = xs ++ left
  nrToList _ = []  -- Just flag timeout as an empty list
  rtrace xs | not tr = res
            | latex = trace (latexTrace xs) res
            | otherwise = trace (showReductionTrace (prettyShow . trsToCore) xs) res
    where res = snd (head xs)

  showReductionTrace sh xs = msg
    where
      msg = "***** Reduction trace\n" ++ (unlines $ map pr $ reverse xs) ++ "*****\n"
      pr (s, a) = s ++ ":\n" ++ sh a ++ "\n----------\n"

  -- Force evaluation to get traces
  force xs = if xs==xs then xs else undefined

sub :: Flags -> ESystem -> T.Expr -> T.Expr
sub flg sys | fFinalInline flg = postProcess sys (ruleEnv sys)
            | otherwise = id

moreTrace :: Bool
moreTrace = False

start :: T.Expr -> T.Expr
start e | moreTrace = trace ("start:\n" ++ show e) e
        | otherwise = e

nrTrace :: ESystem -> NormResult T.Expr -> NormResult T.Expr
nrTrace sys nr | moreTrace = trace (normDump sys nr) nr
               | otherwise = nr

normDump :: ESystem -> NormResult T.Expr -> String
normDump sys nr =
  unlines $
  ("done=" ++ show (length (nrDone nr)) ++ ", left=" ++ show (length (nrLeft nr))) :
  map ((++ "\n=====") . show . term) (nrDone nr) ++
  ["\n*****"] ++
  map (dumpOne sys . term) (nrLeft nr)

dumpOne :: ESystem -> T.Expr -> String
dumpOne sys e = show e ++ "\n   " ++ red ++ "\n====="
  where red =
          case normalFormsFuelTrace sys 20000 e of
            NormResult { nrLeft = [] } -> "reduces"
            NormResult { nrDone = done, nrLeft = left } -> "no " ++ show (length done, length left)

type Trace a = [(String, a)]

-- Eliminate duplicates in the 'done' results by using
-- an (expensive) equivalence check.
elimDupNR :: ESystem -> NormResult T.Expr -> NormResult T.Expr
elimDupNR sys nr = nr{ nrDone = elimDupT sys (nrDone nr) }

elimDupT :: ESystem -> [Traced T.Expr] -> [Traced T.Expr]
elimDupT sys = nubTraced (normalForm sys)

nubTraced :: (T.Expr -> T.Expr) -> [Traced T.Expr] -> [Traced T.Expr]
nubTraced f = map snd . nubBy ((==) `on` fst) . map (\ t -> (f (term t), t))

-- Eliminate duplicates in the 'done' results by possibly using
-- a final normalization step.
subsNR :: Flags -> ESystem -> NormResult T.Expr -> NormResult T.Expr
subsNR flg sys nr | not (fFinalInline flg) = nr
                  | otherwise = nr{ nrDone = subsT sys (nrDone nr) }

subsT :: ESystem -> [Traced T.Expr] -> [Traced T.Expr]
subsT sys = nubTraced (postProcess sys (ruleEnv sys))

coreToTrs :: HasCallStack => Core -> T.Expr
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
coreToTrs (CDef (i:is) e) = T.Exi $ T.Bind (coreToTrsI i) (coreToTrs $ CDef is e)
coreToTrs (CSucceeds e) = coreToTrs e  -- XXX temporarily
coreToTrs CWrong{} = T.Wrong
coreToTrs (CSplit e f g) = T.Split (coreToTrs e) (coreToTrsV f) (coreToTrsV g)
coreToTrs e@CMacro{} = impossible e
coreToTrs e@CLambda{} = impossible e
coreToTrs _ = undefined

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
trsToCore (i1 T.:~: i2) = CApply (CVar (Ident noLoc "~")) (CArray [trsToCore (T.Var i1), trsToCore (T.Var i2)])
trsToCore ee@(_ T.:>: _) = CSeq $ map trsToCore $ flat ee
  where flat (e1 T.:>: e2) = flat e1 ++ flat e2
        flat e = [e]
trsToCore (e1 T.:|: e2) = CBar (trsToCore e1) (trsToCore e2)
trsToCore (e1 T.:@: e2) = CApply (trsToCore e1) (trsToCore e2)
trsToCore T.Fail = CFail
trsToCore ee@T.Exi{} = flat [] ee
  where flat vs (T.Exi (T.Bind x e)) = flat (vs ++ [x]) e
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
    expr p e@T.Exi{}   = showString "def (" . shxs . showString ") (" . expr p a . showString ")"
      where (xs, a) = getXs e
            getXs (T.EXI y b) = (y:ys, c) where (ys, c) = getXs b
            getXs b = ([], b)
            shxs = foldr1 (\ x s -> x . showString " ^^ " . s) (map ident xs)
    expr _ (T.One a)   = showString "one (" . expr 0 a . showString ")"
    expr _ (T.All a)   = showString "all (" . expr 0 a . showString ")"
    expr _ _ = undefined

    ident = showString . show
