module Koen(rewrite) where
import Data.Maybe
import qualified TRSCore as T(Expr(..), Value(..), HNF(..), Op(..))
import qualified Bind as T(Bind(..), Ident(..))
import Rules(rules)
import TRS(normalFormsFuel)
import Expr(Ident(..), noLoc)
import Core
import Error

rewrite :: Int -> Core -> [Core]
rewrite n = map koenToCore . normalFormsFuel n rules . coreToKoen

coreToKoen :: Core -> T.Expr
coreToKoen (CValue v) = T.Val (coreToKoenV v)
coreToKoen (CUnify e1 e2) = coreToKoen e1 T.:=: coreToKoen e2
coreToKoen (CSeq []) = undefined
coreToKoen (CSeq [e]) = coreToKoen e
coreToKoen (CSeq (e:es)) = coreToKoen e T.:>: coreToKoen (CSeq es)
coreToKoen (CApply v1 v2) = coreToKoenV v1 T.:@: coreToKoenV v2
coreToKoen (CBar []) = T.Fail
coreToKoen (CBar [e]) = coreToKoen e
coreToKoen (CBar (e:es)) = coreToKoen e T.:|: coreToKoen (CBar es)
coreToKoen (COne e) = T.One $ coreToKoen e
coreToKoen (CAll e) = T.All $ coreToKoen e
coreToKoen (CDef [] e) = coreToKoen e
coreToKoen (CDef (i:is) e) = T.Def $ T.Bind (coreToKoenI i) (coreToKoen $ CDef is e)
coreToKoen (CSucceeds e) = coreToKoen e  -- XXX temporarily
coreToKoen e = impossible e

coreToKoenV :: Value -> T.Value
coreToKoenV (Var i) = T.Var $ coreToKoenI i
coreToKoenV (HNF h) = T.HNF $ coreToKoenH h

coreToKoenH :: HNF -> T.HNF
coreToKoenH (HInt i) = T.Int i
coreToKoenH HRat{} = undefined
coreToKoenH (HPrim s) = T.Op $ fromMaybe (error $ "unknown op: " ++ s) $ lookup s ops
  where ops = map (\ (x,y) -> (y, x)) allOps
coreToKoenH (HArray vs) = T.Arr $ map coreToKoenV vs
coreToKoenH HLam{} = undefined

coreToKoenI :: Ident -> T.Ident
coreToKoenI (Ident _ s) = T.Name s

koenToCore :: T.Expr -> Core
koenToCore (T.Val v) = CValue $ koenToCoreV v
koenToCore (e1 T.:=: e2) = CUnify (koenToCore e1) (koenToCore e2)
koenToCore ee@(_ T.:>: _) = CSeq $ map koenToCore $ flat ee
  where flat (e1 T.:>: e2) = flat e1 ++ flat e2
        flat e = [e]
koenToCore ee@(_ T.:|: _) = CBar $ map koenToCore $ flat ee
  where flat (e1 T.:|: e2) = flat e1 ++ flat e2
        flat e = [e]
koenToCore (e1 T.:@: e2) = CApply (koenToCoreV e1) (koenToCoreV e2)
koenToCore T.Fail = CBar []
koenToCore ee@T.Def{} = flat [] ee
  where flat vs (T.Def (T.Bind x e)) = flat (vs ++ [x]) e
        flat vs e = CDef (map koenToCoreI vs) (koenToCore e)
koenToCore (T.One e) = COne $ koenToCore e
koenToCore (T.All e) = CAll $ koenToCore e

koenToCoreV :: T.Value -> Value
koenToCoreV (T.Var i) = Var (koenToCoreI i)
koenToCoreV (T.HNF h) = HNF (koenToCoreH h)

koenToCoreH :: T.HNF -> HNF
koenToCoreH (T.Int i) = HInt i
koenToCoreH (T.Op op) = HPrim $ fromMaybe undefined $ lookup op allOps
koenToCoreH (T.Arr vs) = HArray $ map koenToCoreV vs

koenToCoreI :: T.Ident -> Ident
koenToCoreI (T.Name s) = Ident noLoc s
koenToCoreI _ = undefined

allOps :: [(T.Op, String)]
allOps = [(T.Gt, "in'>'"), (T.Add, "in'+'"), (T.IsInt, "isInt#")]
