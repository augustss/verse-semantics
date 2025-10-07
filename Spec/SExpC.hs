module SExpC where
import Control.Monad.Identity
import Data.Maybe
--import FrontEnd.Expr(SrcExpr(..), Aperture(..), Lit(..))
import qualified FrontEnd.Expr as E
import Exp

type N a = Identity a

-- Hack some identifiers into primitives.
getPrim :: String -> Maybe (Op, Int)
getPrim s =
  case s of
    "operator'+'"  -> Just (Oadd, 2)
    "operator'>'"  -> Just (Ogt,  2)
    "int"          -> Just (Oint, 1)
    "neg"          -> Just (Oneg, 1)
--    "any"          -> Just (Oany, 1)
    _              -> Nothing

srcExprToExp :: E.SrcExpr -> Exp
srcExprToExp = runIdentity . to where
  to expr =
    case expr of
      -- Hack around things pulled in from the prelude.
      E.DefineE (E.Ident _ s) _
        | isJust (getPrim s) -> pure $ Tup []  -- delete operator'...' :=
      -------------------------------------
      E.Lit (E.LInt k)       -> pure $ Int k
      E.Variable (E.Ident _ s)
        | Just (p, _) <- getPrim s -> pure $ Prim p
        | otherwise          -> pure $ Var s
      E.ApplyD (E.Variable (E.Ident _ "operator'|||'")) (E.Array [e0, e1]) ->
        UChoice <$> to e0 <*> to e1
      E.ApplyD e0 e1         -> App <$> to e0 <*> to e1
      E.Unify e0 e1          -> Equ <$> to e0 <*> to e1
      E.Seq e0 e1            -> Seq <$> to e0 <*> to e1
      E.Choice e0 e1         -> Choice <$> to e0 <*> to e1
      E.Fail                 -> pure Fail
      E.DefineV (E.Ident _ s)  -> pure $ Exi s
      E.DefineE (E.Ident _ s) e -> Def s <$> to e
      E.DefineIE (E.Ident _ s) e -> DefI s <$> to e
      E.Array es             -> Tup <$> mapM to es
      E.All e                -> All <$> to e
      E.If3 e0 e1 e2         -> If <$> to e0 <*> to e1 <*> to e2
      E.OfType e1 _ e2       -> to $ E.ApplyD e2 e1
      E.Range e              -> Colon <$> to e
      E.Blk es               -> Block . seqs <$> mapM to es
      E.Function oc e0 _ e1  -> Fun oc' <$> to e0 <*> to e1
        where oc' = case oc of E.Open -> Open; E.Closed -> Closed
      E.Check _ e            -> to e
      e -> error $ "srcExprToOper: cannot handle " ++ show e

seqs :: [Exp] -> Exp
seqs [] = Tup []
seqs [e] = e
seqs (e:es) = e `Seq` seqs es
