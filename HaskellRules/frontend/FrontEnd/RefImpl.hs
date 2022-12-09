module FrontEnd.RefImpl(evalRI) where
evalRI e = undefined
{-
import Data.Ratio
import Control.Monad ((<=<))
import qualified Data.Text as T

import Data.Fix

import Language.Verse.Desugar
import qualified Language.Verse.Eval as Eval
import Language.Verse.Simplify
import qualified Language.Verse.Val as V

import Language.Verse.Loc as Loc(L(..), minBound)
import Language.Verse.Parse.Exp(Exp(..))
import Language.Verse.Name(Name)
import Control.Monad.ST(runST)
import Control.Monad.Trans.Except ( runExceptT )

import qualified FrontEnd.Core as C

type ExpL = L (Exp L Name)

evalRI :: C.Core -> C.Core
evalRI = valsToCore . evalExp . coreToExp

evalExp :: ExpL -> [Fix V.Val]
evalExp x = either (error . show) id $ runST $ runExceptT $ Eval.eval $ either (error . show) id $ simplify <=< desugar $ x

nl :: a -> L a
nl = L Loc.minBound

identToName :: C.Ident -> Name
identToName (C.Ident _ s) = T.pack s

coreToExp :: C.Core -> ExpL
coreToExp = nl . expl
  where
    seqe :: [Exp L Name] -> Exp L Name
    seqe [] = undefined
    seqe [e] = e
    seqe (e:es) = nl e :*>: nl (seqe es)

    expl (C.CVar n) = Name (identToName n)
    expl (C.CInt i) = Int i
--    expl (C.CRat _) = undefined
--    expl (C.CPrim _) = undefined
    expl (C.CArray es) = Tuple $ map coreToExp es
    expl (C.CLam i e) = Lambda (nl (identToName i)) (coreToExp e)
    expl (C.CUnify e1 e2) = coreToExp e1 :=: coreToExp e2
    expl (C.CSeq es) = seqe (map expl es)
    expl (C.CApply (C.CPrim "in'+'") (C.CArray [e1, e2])) = coreToExp e1 :+: coreToExp e2
    expl (C.CApply (C.CPrim "in'-'") (C.CArray [e1, e2])) = coreToExp e1 :-: coreToExp e2
    expl (C.CApply (C.CPrim "in'*'") (C.CArray [e1, e2])) = coreToExp e1 :*: coreToExp e2
    expl (C.CApply (C.CPrim "in'/'") (C.CArray [e1, e2])) = coreToExp e1 :/: coreToExp e2
    expl (C.CApply (C.CPrim "pre'+'") e) = expl e -- XXX
    expl (C.CApply (C.CPrim "in'<>'") (C.CArray [e1, e2])) = Not (nl (coreToExp e1 :=: coreToExp e2))
    expl (C.CApply e1 e2) = Invoke (coreToExp e1) (coreToExp e2)
    expl (C.CBar e1 e2) = coreToExp e1 :|: coreToExp e2
    expl (C.CFail) = Fail
    expl (C.COne e) = One $ coreToExp e
    expl (C.CAll e) = All $ coreToExp e
    expl (C.CSucceeds e) = expl e  -- XXX temporarily
    expl (C.CMacro i e) = error $ "expl: " ++ show (i, e)
    expl (C.CDef [] e) = expl e
    expl (C.CDef (i:is) e) = Exists (nl $ identToName i) (coreToExp (C.CDef is e))
--    expl (C.CWrong _) = undefined
--    expl (C.CSplit _ _ _) = undefined
--    expl (C.CLambda _ _ _ _ _) = undefined
    expl e = error $ "expl: " ++ show e

valsToCore :: [Fix V.Val] -> C.Core
valsToCore [] = C.CFail
valsToCore avs = foldr1 C.CBar (cores avs)
  where
    cores = map (core . getFix)
    core (V.Int i) = C.CInt i
    core (V.Float _) = undefined
    core (V.Rational r) | denominator r == 1 = C.CInt (numerator r)
                        | otherwise = C.CRat r
    core (V.Truth _) = undefined
    core (V.Lambda _i _env _e) = undefined
    core (V.Tuple vs) = C.CArray $ cores vs
-}
