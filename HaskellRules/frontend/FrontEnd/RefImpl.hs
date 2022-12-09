module FrontEnd.RefImpl(evalRI) where
--import Debug.Trace
--import Epic.Print
import Data.Ratio
import Control.Monad ((<=<))
import Control.Monad.ST(runST)
import Control.Monad.Trans.Except ( runExceptT )
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

import Data.Fix

import Language.Verse.Desugar
import qualified Language.Verse.Eval as Eval
import Language.Verse.Simplify
import qualified Language.Verse.Val as V

import Language.Verse.Loc as Loc(L(..), minBound)
import Language.Verse.Parse.Exp(Exp(..))
import Language.Verse.Name(Name)
import Language.Verse.Ident(Ident, name)
import qualified Language.Verse.Simplify.Exp as S

import Epic.Uniplate(universe)
import qualified FrontEnd.Core as C
import qualified FrontEnd.CoreSimp as C
import qualified FrontEnd.Desugar as C
import qualified FrontEnd.Flags as C
import qualified FrontEnd.Parse as C

type ExpL = L (Exp L Name)
type SExpL = L (S.Exp L (Ident Name))

evalRI :: C.Core -> C.Core
evalRI = valsToCore . evalExp . coreToExp

evalExp :: ExpL -> [Fix V.Val]
evalExp x = either (error . show) id $ runST $ runExceptT $ Eval.eval $ either (error . show) id $ simplify <=< desugar $ x

nl :: a -> L a
nl = L Loc.minBound

identToName :: C.Ident -> Name
identToName (C.Ident _ s) = T.pack s

nameToIdent :: Ident Name -> C.Ident
nameToIdent n = C.Ident C.noLoc (maybe "?" T.unpack (name n))

coreToExp :: C.Core -> ExpL
coreToExp = coreToExp' . addPrelude

coreToExp' :: C.Core -> ExpL
coreToExp' = nl . expl
  where
    seqe :: [Exp L Name] -> Exp L Name
    seqe [] = undefined
    seqe [e] = e
    seqe (e:es) = nl e :*>: nl (seqe es)

    expl (C.CVar n) = Name (identToName n)
    expl (C.CInt i) = Int i
--    expl (C.CRat _) = undefined
--    expl (C.CPrim _) = undefined
    expl (C.CArray es) = Tuple $ map coreToExp' es
    expl (C.CLam i e) = Lambda (nl (identToName i)) (coreToExp' e)
    expl (C.CUnify e1 e2) = coreToExp' e1 :=: coreToExp' e2
    expl (C.CSeq es) = seqe (map expl es)
    expl (C.CApply (C.CPrim "in'+'") (C.CArray [e1, e2])) = coreToExp' e1 :+: coreToExp' e2
    expl (C.CApply (C.CPrim "in'-'") (C.CArray [e1, e2])) = coreToExp' e1 :-: coreToExp' e2
    expl (C.CApply (C.CPrim "in'*'") (C.CArray [e1, e2])) = coreToExp' e1 :*: coreToExp' e2
    expl (C.CApply (C.CPrim "in'/'") (C.CArray [e1, e2])) = coreToExp' e1 :/: coreToExp' e2
    expl (C.CApply (C.CPrim "pre'+'") e) = expl e -- XXX
--    expl (C.CApply (C.CPrim "in'<>'") (C.CArray [e1, e2])) = Not (nl (coreToExp' e1 :=: coreToExp' e2))
    expl (C.CApply (C.CPrim "in'<'") (C.CArray [e1, e2]))  = coreToExp' e1 :<: coreToExp' e2
    expl (C.CApply (C.CPrim "in'<='") (C.CArray [e1, e2])) = coreToExp' e1 :<=: coreToExp' e2
    expl (C.CApply (C.CPrim "in'>'") (C.CArray [e1, e2]))  = coreToExp' e1 :>: coreToExp' e2
    expl (C.CApply (C.CPrim "in'>='") (C.CArray [e1, e2])) = coreToExp' e1 :>=: coreToExp' e2
    expl (C.CApply (C.CPrim "isInt$") e) = IsInt (coreToExp e)
    expl (C.CApply (C.CPrim "mapAp$") e) = expl (C.CApply (C.CVar (C.Ident C.noLoc "mapAp")) e)
    expl (C.CApply e1 e2) = Invoke (coreToExp' e1) (coreToExp' e2)
    expl (C.CBar e1 e2) = coreToExp' e1 :|: coreToExp' e2
    expl (C.CFail) = Fail
    expl (C.COne e) = One $ coreToExp' e
    expl (C.CAll e) = All $ coreToExp' e
    expl (C.CSucceeds e) = expl e  -- XXX temporarily
    expl (C.CMacro i e) = error $ "expl: " ++ show (i, e)
    expl (C.CDef [] e) = expl e
    expl (C.CDef (i:is) e) = Exists (nl $ identToName i) (coreToExp' (C.CDef is e))
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
    core (V.Lambda i env e) | H.null env = C.CLam (nameToIdent i) (expToCore e)
                            | otherwise = undefined
    core (V.Tuple vs) = C.CArray $ cores vs

expToCore :: SExpL -> C.Core
expToCore (L _ ee) = core ee
  where
    core (S.Name i) = C.CVar (nameToIdent i)
    core (S.Int i) = C.CInt i
    core e = error $ "expToCore: " ++ show e

preludeTxt :: String
preludeTxt = "\
\tail  := (xs:any  => all{xs[i>0; i:any]});\n\
\cons  := (xxs:any => ((x:any,xs:any)=xxs; all{x | :xs}));\n\
\mapAp := (fs:any  => if (f:=fs[0]) then cons[f[], mapAp[tail[fs]]] else ())\n\
\"

preludeCore :: C.Core
preludeCore =
  C.simpCore $
  C.exprToCore C.defaultFlags $
  C.desugar $
  C.parseDie C.pFile "<prelude>" preludeTxt

addPrelude :: C.Core -> C.Core
addPrelude c | "mapAp$" `elem` primops = ins preludeCore
             | otherwise = c
  where primops = [ s | C.CPrim s <- universe c]
        ins (C.CDef i e) = C.CDef i (ins e)
        ins e = C.CSeq [e, c]
