module FrontEnd.TRSAdapter(rewrite, coreToTrs, trsToCore, asTRS) where
import Data.Char(toLower)
import Data.Function(on)
import Data.List(nubBy)
import Data.Maybe
import qualified Data.IntMap as IM
import qualified Epic.SIntMap as SIM
import qualified TRS.Bind as T
import qualified Rules.Core as T
import Rules.Equiv(normalForm)
import Rules.Systems(ESystem)
import TRS.NormalForm(normalFormFuelTrace, normalFormsFuelTrace, NormResult(..))
import TRS.System(preProcess, postProcess, ruleEnv, sname)
import TRS.Traced(Traced, term, toList)
import FrontEnd.Expr
import FrontEnd.Error
import FrontEnd.EvalBlock(runBlock)
import FrontEnd.Flags
import GHC.Stack

import Debug.Trace
import Epic.Print

asTRS :: (T.Expr -> T.Expr) -> Core -> Core
asTRS f = trsToCore . f . coreToTrs

-- XXX use graph normal form when needed

{-
evaluate :: T.RuleEnv T.Expr -> Core -> [Core]
evaluate tflg e = [eval flg e]
  where flg = EFlags { underLambda = T.tfUnderLambda tflg, traceEval = T.tfTrace tflg, steps = T.tfRewriteSteps tflg }
-}

rewrite :: Flags -> ESystem -> Core -> [Core]
rewrite flg asys | sname sys == "iblock" = (:[]) . runBlock (ruleEnv sys)
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
  nrToList NormResult{ nrDone = xs, nrLeft = left } | null left || fNoFuelStop flg = xs -- ++ left
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
sub flg sys | fPostProcess flg = postProcess sys (ruleEnv sys)
            | otherwise = id

moreTrace :: Bool
moreTrace = False

start :: T.Expr -> T.Expr
start e | moreTrace = trace ("start:\n" ++ prettyShow e) e
        | otherwise = e

nrTrace :: ESystem -> NormResult T.Expr -> NormResult T.Expr
nrTrace sys nr | moreTrace = trace (normDump sys nr) nr
               | otherwise = nr

normDump :: ESystem -> NormResult T.Expr -> String
normDump sys nr =
  unlines $
  ("done=" ++ show (length (nrDone nr)) ++ ", left=" ++ show (length (nrLeft nr))) :
  map ((++ "\n=====") . prettyShow . term) (nrDone nr) ++
  ["\n*****"] ++
  map (dumpOne sys . term) (nrLeft nr)

dumpOne :: ESystem -> T.Expr -> String
dumpOne sys e = prettyShow e ++ "\n   " ++ red ++ "\n====="
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
subsNR flg sys nr | not (fPostProcess flg) = nr
                  | otherwise = nr{ nrDone = subsT sys (nrDone nr) }

subsT :: ESystem -> [Traced T.Expr] -> [Traced T.Expr]
subsT sys = nubTraced (postProcess sys (ruleEnv sys))

coreToTrs :: HasCallStack => Core -> T.Expr
coreToTrs (Variable i) = T.Var $ coreToTrsI i
coreToTrs (Lit (LitInt i)) = T.Int i
coreToTrs (Lit (LitChar c)) = T.Char c
coreToTrs (Lit (LitPtr p)) = T.Ref (T.Ptr p)
coreToTrs (Lit (LitStr s)) = T.Arr (map T.Char s)  -- assume strings are arrays of characters
coreToTrs (Lit (LitPath (Path p))) = T.Path p
coreToTrs Lit{} = undefined
coreToTrs (EPrim "any$") = T.LAM x (T.Var x)  where x = T.Name "x"
coreToTrs (EPrim s) = T.Op $ fromMaybe (error $ "unknown op: " ++ s) $ lookup s ops
  where ops = map (\ (x,y) -> (y, x)) allOps
coreToTrs (Array vs) = T.Arr $ map coreToTrs vs
coreToTrs (Lam x e) = T.LAM (coreToTrsI x) (coreToTrs e)
coreToTrs (Unify e1 e2) = coreToTrs e1 T.:=: coreToTrs e2
coreToTrs (Seq []) = undefined
coreToTrs (Seq [e]) = coreToTrs e
coreToTrs (Seq (e:es)) = coreToTrs e T.:>: coreToTrs (Seq es)
coreToTrs (ApplyD (Array []) _) = T.Fail
coreToTrs (ApplyD v1 v2) = coreToTrs v1 T.:@: coreToTrs v2
coreToTrs (Choice e1 e2) = coreToTrs e1 T.:|: coreToTrs e2
coreToTrs Fail = T.Fail
coreToTrs (Exists [] e) = coreToTrs e
coreToTrs (Exists (i:is) e) = T.Exi $ T.Bind (coreToTrsI i) (coreToTrs $ Exists is e)
coreToTrs (Forall [] e) = coreToTrs e
coreToTrs (Forall (i:is) e) = T.Uni $ T.Bind (coreToTrsI i) (coreToTrs $ Forall is e)
coreToTrs (Succeeds e) = coreToTrs e  -- XXX temporarily
coreToTrs (Wrong s) = T.Wrong s
coreToTrs (Split e f g) = T.Split (coreToTrs e) (coreToTrsV f) (coreToTrsV g)
coreToTrs (Macro1 (Ident _ "one")    [] e) = T.One    $ coreToTrs e
coreToTrs (Macro1 (Ident _ "all")    [] e) = T.All    $ coreToTrs e
coreToTrs (Macro1 (Ident _ "verify") [] e) = T.Verify $ coreToTrs e
coreToTrs (Macro1 (Ident _ "assert") [] e) = T.Assert $ coreToTrs e
coreToTrs (Macro1 (Ident _ "assume") [] e) = T.Assume $ coreToTrs e
coreToTrs (Macro1 (Ident _ "decide") [] e) = T.Decide $ coreToTrs e
coreToTrs (Macro1 (Ident _ "decides") [] e) = T.Decide $ coreToTrs e
coreToTrs (Macro2 (Ident _ "guard") e1 e2) = coreToTrs e1 T.:>>: coreToTrs e2
coreToTrs e@Macro1{} = impossible e
coreToTrs (If3B xs e1 e2 e3) = foldr T.IFB (T.If (coreToTrs e1) (coreToTrs e2) (coreToTrs e3)) (coreToTrsI <$> xs)
-- coreToTrs (If3 e1 e2 e3) = T.If (coreToTrs e1) (coreToTrs e2) (coreToTrs e3)
coreToTrs (EStore h e) = T.Store (SIM.fromList $ map (\ (p,c) -> (T.Ptr p, coreToTrs c)) $ IM.toList $ refMap h) (coreToTrs e)
coreToTrs DomainFail = T.Wrong "DomainFail"
coreToTrs e = error $ "coreToTrs: " ++ prettyShow e

coreToTrsV :: HasCallStack => Core -> T.Value
coreToTrsV e = case coreToTrs e of T.Val v -> v; _ -> undefined

coreToTrsI :: Ident -> T.Ident
coreToTrsI (Ident _ s) = T.Name s

trsToCore :: T.Expr -> Core
trsToCore (T.Var i) = Variable (trsToCoreI i)
trsToCore (T.Int i) = Lit (LitInt i)
trsToCore (T.Char c) = Lit (LitChar c)
trsToCore (T.Path p) = Lit (LitPath (Path p))
trsToCore (T.Op op) = EPrim $ fromMaybe undefined $ lookup op allOps
trsToCore (T.Arr vs) = Array $ map trsToCore vs
trsToCore (T.Map kvs) = Map $ map f kvs
  where f (k, v) = --Function [(trsToCore k,[])] (trsToCore v)
                   InfixOp (trsToCore k) (Ident noLoc "=>") (trsToCore v)
trsToCore (T.Lam (T.Bind x e)) = Lam (trsToCoreI x) (trsToCore e)
trsToCore (e1 T.:=: e2) = Unify (trsToCore e1) (trsToCore e2)
trsToCore ee@(_ T.:>: _) = Seq $ map trsToCore $ flat ee
  where flat (e1 T.:>: e2) = flat e1 ++ flat e2
        flat e = [e]
trsToCore (e1 T.:|: e2) = Choice (trsToCore e1) (trsToCore e2)
trsToCore (e1 T.:@: e2) = ApplyD (trsToCore e1) (trsToCore e2)
trsToCore T.Fail = Fail
trsToCore ee@T.Exi{} = flat [] ee
  where flat vs (T.Exi (T.Bind x e)) = flat (vs ++ [x]) e
        flat vs e = Exists (map trsToCoreI vs) (trsToCore e)
trsToCore ee@T.Uni{} = flat [] ee
  where flat vs (T.Uni (T.Bind x e)) = flat (vs ++ [x]) e
        flat vs e = Forall (map trsToCoreI vs) (trsToCore e)
trsToCore (T.One e) = Macro1 (Ident noLoc "one") [] $ trsToCore e
trsToCore (T.All e) = Macro1 (Ident noLoc "all") []  $ trsToCore e
trsToCore (T.Wrong s) = Wrong s
trsToCore (T.Split e f g) = Split (trsToCore e) (trsToCore f) (trsToCore g)
--trsToCore (T.BlockC e) = trsToCore e
trsToCore (T.If e1 e2 e3) = If3 (trsToCore e1) (trsToCore e2) (trsToCore e3)
trsToCore (T.Store h e) = EStore s (trsToCore e)
  where s = Store { refMap = IM.fromList $ map (\ (T.Ptr i, c) -> (i, trsToCore c)) $ SIM.toList h, outputs = [] }
trsToCore (T.Ref (T.Ptr i)) = Lit (LitPtr i)
trsToCore e = error $ "trsToCore: unimplemented: " ++ show e

trsToCoreI :: T.Ident -> Ident
trsToCoreI (T.Name s) = Ident noLoc s
trsToCoreI (T.Prim i) = Ident noLoc $ "$" ++ show i

allOps :: [(T.Op, String)]
allOps = [
  (T.Gt,    "intGT$"),
  (T.Ge,    "intGE$"),
  (T.Lt,    "intLT$"),
  (T.Le,    "intLE$"),
  (T.Ne,    "intNE$"),
  (T.Add,   "intAdd$"),
  (T.Sub,   "intSub$"),
  (T.Mul,   "intMul$"),
  (T.Div,   "intDiv$"),
  (T.Neg,   "intNeg$"),
  (T.Plus,  "intPlus$"),
  (T.IsInt, "isInt$"),
  (T.IsChar,"isChr$"),
  (T.IsArr, "isArr$"),
  (T.IsMap, "isMap$"),
  (T.IsPath,"isPath$"),
  (T.MapAp, "mapAp$"),
  (T.Cons,  "cons$"),
  (T.Alloc, "alloc$"),
  (T.Read,  "read$"),
  (T.Write, "write$"),
  (T.AddTo, "in'+='"),
  (T.DotDot,"in'..'"),
  (T.Print, "print$"),
  (T.Append,"append$"),
  (T.Error, "err$"),
  (T.Length,"arrLen$"),
  (T.Concat,"arrConc$"),
  (T.MkMap, "mkMap$")
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
