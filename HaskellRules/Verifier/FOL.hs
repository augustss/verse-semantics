module Verifier.FOL where

import Data.List( intercalate )
import Data.Char( isDigit, isAlpha, toUpper )
import TRS.Bind
import Z3.Monad
import Control.Monad.IO.Class( liftIO )

--------------------------------------------------------------------------------
-- Ident

showIdent :: Ident -> String
showIdent (Name s) = s
showIdent (Prim p) = "pr_" ++ show p ++ "_"

showVar :: Ident -> String
showVar (Name (c:s))
  | isAlpha c    = toUpper c : s
  | otherwise    = "V" ++ (c:s)
showVar (Prim p) = "PR_" ++ show p ++ "_"

--------------------------------------------------------------------------------
-- Term

data Term
  = Vr Ident
  | Ap Ident [Term]
 deriving ( Eq, Ord )

instance Free Term where
  free (Vr x)    = [x]
  free (Ap _ ts) = free ts

instance Show Term where
  show (Vr v)    = showVar v
  show (Ap c []) = showIdent c
  show (Ap f ts) = showIdent f ++ "(" ++ intercalate "," (map show ts) ++ ")"

--------------------------------------------------------------------------------
-- Form

data Form
  = FALSE
  | TRUE
  | Pred Ident [Term]
  | Not Form
  | Form :&&: Form
  | Form :||: Form
  | Forall (Bind Form)
  | Exists (Bind Form)
 deriving ( Eq, Ord )

instance Free Form where
  free (Pred _ ts) = free ts
  free (Not p)     = free p
  free (p :&&: q)  = free (p,q)
  free (p :||: q)  = free (p,q)
  free (Forall b)  = free b
  free (Exists b)  = free b
  free _           = []

instance Show Form where
  show FALSE       = "$false"
  show TRUE        = "$true"
  show (Pred r []) = showIdent r
  show (Pred r [s,t]) | showIdent r == "=" = show s ++ " = " ++ show t
  show (Pred r ts) = showIdent r ++ "(" ++ intercalate "," (map show ts) ++ ")"
  show (Not p)     = "~" ++ show1 p
  show (p :&&: q)  = showAnd [p,q]
  show (p :||: q)  = showOr [p,q]
  show (Forall b)  = "!" ++ showBind b
  show (Exists b)  = "?" ++ showBind b

showAnd ps = intercalate " & " (map show1 (flat ps))
 where
  flat ((p :&&: q) : ps) = flat (p:q:ps)
  flat (p:ps)            = p : flat ps
  flat []                = []
  
showOr ps = intercalate " | " (map show1 (flat ps))
 where
  flat ((p :||: q) : ps) = flat (p:q:ps)
  flat (p:ps)            = p : flat ps
  flat []                = []
  
showBind (Bind x p) = "[" ++ showVar x ++ "]: " ++ show1 p  

show1 p
  | isAtom p  = show p
  | otherwise = "(" ++ show p ++ ")"
 where
  isAtom FALSE      = True
  isAtom TRUE       = True
  isAtom (Not _)    = True
  isAtom (Pred p _) = not (isOp p)
  isAtom _          = False
  
  isOp v = showIdent v == "="

(.=.) :: Term -> Term -> Form
s .=. t = Pred (ident "=") [s,t]

--------------------------------------------------------------------------------
-- PROVER ----------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Values

{-
-- values are coded in Z3 using the following datatype:
data Value
  = Int Integer
  | Tuple Tuple

data Tuple
  = Nil
  | Cons Value Tuple
-}

data Env z3
  = Env
  { valueSort :: Sort
  , boolSort  :: Sort
  
  -- functions
  , tuple  :: [AST] -> z3 AST
  , isInt  :: AST   -> z3 AST
  , selInt :: AST   -> z3 AST
  , int    :: AST   -> z3 AST
  }
  
mkEnv :: MonadZ3 z3 => z3 (Env z3)
mkEnv =
  do valueSym <- mkStringSymbol "Value"
     tupleSym <- mkStringSymbol "Tuple"
     
     intSym    <- mkStringSymbol "Int"
     isIntSym  <- mkStringSymbol "isInt" 
     selIntSym <- mkStringSymbol "selInt"
     intSort   <- mkIntSort
     intCon    <- mkConstructor intSym isIntSym [(selIntSym, Just intSort, 0)]
     
     isTupleSym  <- mkStringSymbol "isTuple" 
     selTupleSym <- mkStringSymbol "selTuple"
     tupleCon    <- mkConstructor tupleSym isTupleSym [(selTupleSym, Nothing, 1)]

     nilSym   <- mkStringSymbol "Nil"
     isNilSym <- mkStringSymbol "isNil"
     nilCon   <- mkConstructor nilSym isNilSym []

     consSym   <- mkStringSymbol "Cons"
     isConsSym <- mkStringSymbol "isCons" 
     headSym   <- mkStringSymbol "head"
     tailSym   <- mkStringSymbol "tail"
     consCon   <- mkConstructor consSym isConsSym [(headSym, Nothing, 0),(tailSym, Nothing, 1)]

     ~[valueSort0, tupleSort] <- mkDatatypes
       [valueSym, tupleSym]
       [[intCon,tupleCon],[nilCon,consCon]]

     boolSort0 <- mkBoolSort

     ~[funInt,funTuple]             <- getDatatypeSortConstructors valueSort0
     ~[funIsInt,funIsTuple]         <- getDatatypeSortRecognizers valueSort0
     ~[~[funSelInt],~[funSelTuple]] <- getDatatypeSortConstructorAccessors valueSort0

     ~[funNil,funCons]  <- getDatatypeSortConstructors tupleSort

     return $ Env
       { valueSort = valueSort0
       , boolSort  = boolSort0

       , tuple = \ts ->
           let tup []     = do mkApp funNil []
               tup (a:as) = do t <- tup as
                               mkApp funCons [a,t]
            in do t <- tup ts
                  mkApp funTuple [t]

       , isInt = \t ->
           do mkApp funIsInt [t]

       , selInt = \t ->
           do mkApp funSelInt [t]

       , int = \t ->
           do mkApp funInt [t]
       }

--------------------------------------------------------------------------------
-- Conversion from Form and Term into Z3

-- Form --> Z3

z3form :: MonadZ3 z3 => Env z3 -> Form -> z3 AST
z3form env FALSE =
  do mkFalse

z3form env TRUE =
  do mkTrue

z3form env (Pred p [s,t]) | showIdent p == "=" =
  do a <- z3term env s
     b <- z3term env t
     mkEq a b

z3form env (Pred p [t]) | showIdent p == "isInt" =
  do a <- z3term env t
     isInt env a

z3form env (Pred p [s,t]) | showIdent p == "<=" =
  do a <- z3term env s >>= selInt env
     b <- z3term env t >>= selInt env
     mkLe a b

z3form env (Pred p ts) =
  do liftIO (putStrLn ("(predicate '" ++ showIdent p ++ "/" ++ show (length ts)
                                      ++ "' not recognized)"))
     pr  <- mkStringSymbol (showIdent p)
     b   <- mkBoolSort
     pr' <- mkFuncDecl pr [valueSort env|_<-ts] (boolSort env)
     as  <- sequence [ z3term env t | t <- ts ]
     mkApp pr' as

z3form env (Not p) =
  do z3form env p >>= mkNot

z3form env (p :&&: q) =
  do a <- z3form env p
     b <- z3form env q
     mkAnd [a,b]

z3form env (p :||: q) =
  do a <- z3form env p
     b <- z3form env q
     mkOr [a,b]

z3form env (Forall (Bind x p)) = 
  do v <- z3term env (Vr x) >>= toApp
     a <- z3form env p
     mkForallConst [] [v] a

z3form env (Exists (Bind x p)) = 
  do v <- z3term env (Vr x) >>= toApp
     a <- z3form env p
     mkExistsConst [] [v] a

-- Term --> Z3

z3term :: MonadZ3 z3 => Env z3 -> Term -> z3 AST
z3term env (Vr v) =
  do vv <- mkStringSymbol (showIdent v)
     mkConst vv (valueSort env)

z3term env (Ap a []) | all isDigit s && not (null s) =
  do mkInteger (read s) >>= int env
 where
  s = showIdent a

z3term env (Ap f ts) | showIdent f == "tup" =
  do as <- sequence [ z3term env t | t <- ts ]
     tuple env as

z3term env (Ap f [s,t]) | showIdent f == "+" =
  do a <- z3term env s >>= selInt env
     b <- z3term env t >>= selInt env
     mkAdd [a,b] >>= int env

z3term env (Ap f ts) =
  do liftIO (putStrLn ("(function '" ++ showIdent f ++ "/" ++ show (length ts)
                                     ++ "' not recognized)"))
     ff  <- mkStringSymbol (showIdent f)
     ff' <- mkFuncDecl ff [valueSort env|_<-ts] (valueSort env)
     as  <- sequence [ z3term env t | t <- ts ]
     mkApp ff' as

--------------------------------------------------------------------------------
-- Prover driver

prove :: Form -> IO Bool
prove phi =
  do putStrLn "-- Proving..."
     (res,msol) <- evalZ3 (script phi)
     putStrLn ("-- Result: " ++ show res)
     case msol of
       Nothing ->
         do return ()
       
       Just inps ->
         do putStrLn ("-- Model: " ++ intercalate ", " inps)
     
     return (res == Unsat)
 where
  script phi =
    do env <- mkEnv
       not_p <- z3form env not_phi
       --s <- astToString not_p
       --liftIO $ putStrLn s
       assert not_p

       withModel $ \m ->
         do sequence [ do ma <- z3term env (Vr x) >>= eval m
                          s  <- case ma of
                                  Just a  -> astToString a
                                  Nothing -> return "?"
                          return (showIdent x ++ "= " ++ s)
                     | x <- inps
                     ]
   where
    (inps,not_phi) = neg phi
    
    neg (Forall (Bind x p)) = (x:xs,phi') where (xs,phi') = neg p
    neg p                   = ([],Not p)

    

--------------------------------------------------------------------------------

