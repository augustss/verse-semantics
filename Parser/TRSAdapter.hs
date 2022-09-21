module TRSAdapter(rewrite) where
import Data.Maybe
import qualified TRSCore as T(Expr(..), Value(..), HNF(..), Op(..))
import qualified Bind as T(Bind(..), Ident(..))
import RulesPOPL(rules)
import TRS(normalFormsFuel)
import Expr(Ident(..), noLoc)
import Core
import Error

rewrite :: Int -> Core -> [Core]
rewrite n = map trsToCore . normalFormsFuel n rules . coreToTrs

coreToTrs :: Core -> T.Expr
coreToTrs (CValue v) = T.Val (coreToTrsV v)
coreToTrs (CUnify e1 e2) = coreToTrs e1 T.:=: coreToTrs e2
coreToTrs (CSeq []) = undefined
coreToTrs (CSeq [e]) = coreToTrs e
coreToTrs (CSeq (e:es)) = coreToTrs e T.:>: coreToTrs (CSeq es)
coreToTrs (CApply v1 v2) = coreToTrsV v1 T.:@: coreToTrsV v2
coreToTrs (CBar []) = T.Fail
coreToTrs (CBar [e]) = coreToTrs e
coreToTrs (CBar (e:es)) = coreToTrs e T.:|: coreToTrs (CBar es)
coreToTrs (COne e) = T.One $ coreToTrs e
coreToTrs (CAll e) = T.All $ coreToTrs e
coreToTrs (CDef [] e) = coreToTrs e
coreToTrs (CDef (i:is) e) = T.Def $ T.Bind (coreToTrsI i) (coreToTrs $ CDef is e)
coreToTrs (CSucceeds e) = coreToTrs e  -- XXX temporarily
coreToTrs CWrong{} = T.Wrong
coreToTrs e = impossible e

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
trsToCore ee@(_ T.:|: _) = CBar $ map trsToCore $ flat ee
  where flat (e1 T.:|: e2) = flat e1 ++ flat e2
        flat e = [e]
trsToCore (e1 T.:@: e2) = CApply (trsToCoreV e1) (trsToCoreV e2)
trsToCore T.Fail = CBar []
trsToCore ee@T.Def{} = flat [] ee
  where flat vs (T.Def (T.Bind x e)) = flat (vs ++ [x]) e
        flat vs e = CDef (map trsToCoreI vs) (trsToCore e)
trsToCore (T.One e) = COne $ trsToCore e
trsToCore (T.All e) = CAll $ trsToCore e
trsToCore T.Wrong = CWrong "unknown"

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
trsToCoreI _ = undefined

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
  (T.Le, "in'>='"),
  (T.Lt, "in'<'"),
  (T.Le, "in'<='"),
  (T.Ne, "in'<>'"),
  (T.Add, "in'+'"),
  (T.Sub, "in'-'"),
  (T.Mul, "in'*'"),
  (T.Div, "in'/'"),
  (T.IsInt, "isInt$"),
  (T.MapAp, "mapAp$")
  ]
