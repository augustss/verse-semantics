{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans -Wno-dodgy-imports #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module FrontEnd.Desugar(
    desugar,
    simplify,
    D, runD
  ) where

import Prelude hiding (pi)

import FrontEnd.Error
import FrontEnd.Expr
import FrontEnd.Flags

-- Epic libraries
import Epic.List
import Epic.Print

-- General libraries
import Data.Monoid
import Data.Either
import Data.List
import Data.Maybe
import Data.IORef
import qualified Data.Set as S
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Writer hiding (guard)
import Debug.Trace

--import qualified Data.Map as M

-- QUESTIONS:
--  x:int='a'   fail or wrong?, tests L93, L95

-- TODO:
--  Add Length
--  Add Err

-- TODO:
--  x:t=v is syntactic sugar for x:=(:t=v) and
--  :t=v is a special form meaning it's not the same as (:t)=v, which is just unification.
--  desugar function effects

desugar :: Flags -> SrcExpr -> IO SrcCore
desugar flgs = runD flgs .
            (-- Simplification [drop this for now]
--             traceDS "simpler"    <=< simpler   <=<  -- verifier breaks without this
--             traceDS "simplify"   <=< simplify  <=<
--             traceDS "lower"      <=< lower     <=<    -- Lowers all/one/for/if into split or whatever
                                                         -- if --> If3B,  for --> For2B

             traceDS "Main desugaring" <=< dsDx      <=<    -- Heavy lifting: Fig 9

--             traceDS "addDeref"   <=< addDeref  <=<    -- Side effects

--             traceDS "lowerApply" <=< lowerApply<=<    -- Round vs square
             traceDS "Add primops"    <=< primops   <=<    -- Var (Ident "op") --> EPrim op
               -- primops: do this after dsSmall, so that PrefixOp, InfixOp, InfixOP are gone

             traceDS "Desugar to Small Source"  <=< dsSmall   <=<    -- Main desugaring into Small Source

             traceDS "Add prelude" <=< addPrelude <=<   -- Prepends prelude from
                                                       --    verifyprelude.verse, mediumprelude.verse

             traceDS "syntaxFixes" <=< syntaxFixes)
  where
    traceDS :: String -> SrcExpr -> D SrcExpr
    traceDS msg e = do { traceD msg (pPrint e)
                       ; pure e }

addPrelude :: SrcExpr -> D SrcExpr
addPrelude orig_e
  = do { prel  <- getDFlagsX (snd . fPrelude)
       ; prel1 <- syntaxFixes prel
       ; return (addUsed (spl prel1) orig_e) }
  where
    -- Split the prelude into an association list
    spl (Array ds) = map (\ e -> (nameOf e, e)) ds
    spl e = impossible e

    -- Find the name of a definition
    nameOf (InfixOp lhs (Ident _ ":=") _) = lhsName lhs
    nameOf e = impossible e

    lhsName :: SrcExpr -> Ident
    lhsName (EffAttr e _) = lhsName e
    lhsName (ApplyS e _) = lhsName e
    lhsName (Variable i) = i
    lhsName e = impossible e

-- Hackily add prelude identifiers used in e
addUsed :: [(Ident, SrcExpr)] -> SrcExpr -> SrcExpr
addUsed prel = loop []
  where
    loop vs e =
      let is = getAllIdents e
          ps = filter (\ (i, _) -> i `elem` is && i `notElem` vs) prel
      in  case ps of
            [] -> e
            ies -> loop (map fst ies ++ vs) $ seqE (map snd ies ++ [e])


-----------------------------------------------
--      The desugaring monad: D
--
--   It is an IO monad (for tracing), with an env that carries
--       a mutable fresh-variable supply
--       the Flags

-----------------------------------------------

newtype D a = MkD (DEnv -> IO a)

data DEnv = DEnv { nextNo :: !(IORef Int), dflags :: !Flags }

instance Monad D where
  MkD m1 >>= k = MkD (\env -> do { r <- m1 env
                                 ; let MkD m2 = k r
                                 ; m2 env })
instance Applicative D where
  pure x = MkD (\_ -> return x)
  (<*>) = ap

instance Functor D where
  fmap f (MkD m) = MkD (\env -> f <$> m env)

runD :: Flags -> D SrcExpr -> IO SrcExpr
-- Runs the D monad
runD flags (MkD thing_inside)
  = do { nextref <- newIORef 1
       ; let env = DEnv { nextNo = nextref, dflags = flags }
       ; thing_inside env }

traceD :: String -> Doc -> D ()
traceD msg doc
  = do { do_trace <- getDFlagsX fTraceDesugar
       ; if do_trace
         then doIO_D (putStrLn ("\n------- " ++ msg ++ "---------\n" ++ render doc))
         else return () }

doIO_D :: IO a -> D a
doIO_D io = MkD (\env -> io)

getDFlags :: D Flags
getDFlags = MkD (\(DEnv { dflags = flags }) -> return flags)

getDFlagsX :: (Flags -> a) -> D a
getDFlagsX f = f <$> getDFlags

newInt :: D Int
newInt = MkD $ \(DEnv { nextNo = ref }) ->
  do { n <- readIORef ref
     ; writeIORef ref (n+1)
     ; return n }

newIdent :: Loc -> String -> D Ident
newIdent l s = do
  n <- newInt
  pure $ Ident l $ "$" ++ s ++ show n


--------------------------------------------------------
--
--         syntaxFixes
--
--------------------------------------------------------

-- Do various early changes:
--  * (e)       -->  e             parens are there to stop the next from possibly firing
--  * e1:e2=e3  -->  e1:e2 := e3   XXX should we do this?
--  * (e1,...)  -->  array{e1,...} no need to distingush them anymore
--  * x&y:e     -->  array{x&y:e}  if outside an array
--                   x:e; y:e      if inside an array
syntaxFixes :: SrcExpr -> D SrcExpr
syntaxFixes = pure . f
  where f :: SrcExpr -> SrcExpr
        f (Parens e) = f e
        f (InfixOp (InfixOp (Variable i1) o@(Op ":") e2) (Ident l3  "=") e3) =
          f $ InfixOp (InfixOp (Variable i1) o e2) (Ident l3 ":=") e3
        f (Tuple es) = f (Array es)
        f (Array es) = Array $ concatMap g es
        f e@(InfixOp (InfixOp _ (Op "&") _) (Op ":" ) _) = f (Array [e])  -- PAMP1
        f e@(InfixOp (InfixOp _ (Op "&") _) (Op ":=") _) = f (Array [e])  -- PAMP1
        f e = composOp f e

        -- PAMP2
        g :: SrcExpr -> [SrcExpr]
        g (InfixOp (InfixOp e1 (Op "&") e2) o@(Op ":" ) rhs) = g (InfixOp e1 o rhs) ++ g (InfixOp e2 o rhs)
        g (InfixOp (InfixOp e1 (Op "&") e2) o@(Op ":=") rhs) = g (InfixOp e1 o rhs) ++ g (InfixOp e2 o rhs)
        g e = [f e]


--------------------------------------------------------
--
--         Desugar into Small Source Verse
--
--------------------------------------------------------

dsSmall :: SrcExpr -> D SrcExpr
dsSmall = ds
  where
    ds :: SrcExpr -> D SrcExpr

    -- Application and unification
    ds (ApplyS  e1 e2) = join (apply applyS <$> ds e1 <*> ds e2)
      where
        applyS x y = eCheck [effSucceeds] (ApplyD x y)

    -- (e1 = e2)  --->  Unify
    ds (InfixOp e1 (Op "=") e2) = Unify <$> ds e1 <*> ds e2

    ds (ApplyD  e1 e2) = join (apply ApplyD <$> ds e1 <*> ds e2)

    -- Bindings
    ds (InfixOp e1 o@(Op ":")  e2) = ds =<< defn e1 (PrefixOp o e2)  -- PCOLONT
    ds (InfixOp e1   (Op ":=") e2) = ds =<< defn e1 e2

    -- Expand type{t} to Fun(x := t)<closed>{x}
    --    ds (Typedef e) = do { x <- newIdent (getLoc e) "x"
    --                        ; ds $ Function [(eDefine x e, [closedId])] (Variable x) }
    --  But a more direct desugaring, which generates less verification clutter,
    --  is   type{t} --> \x. x=t
    ds (Typedef e) = do { x <- newIdent (getLoc e) "x"
                        ; Lam x <$> (Unify (Variable x) <$> ds e) }

    -- Function notation
    ds (InfixOp e1 (Op "=>") e2)  = ds $ Function [(e1, [closedId])] e2
    ds (Function (a:as@(_:_)) b)  = ds $ Function [a] $ Function as b
    ds (Function [(e1, effs)] e2) = do
           e1' <- ds e1
           e2' <- ds e2
           effs' <- checkEffs effs
           pure $ Function [(e1', effs')] e2'

    -- Conditional and for-loop notation
    ds (If1 e) = ds $ If2E e eFalse
    ds (If2 e1 e2) = ds $ If3 e1 e2 eFalse
    ds (If2E e1 e2) = do x <- newIdent (getLoc e1) "x"; ds $ If3 (eDefine x e1) (Variable x) e2
    ds (For1 e) = do x <- newIdent (getLoc e) "x"; ds $ For2 (eDefine x e) (Variable x)

    -- Array
    ds (Array es) = arraySplice =<< mapM elm es
      where elm (PrefixOp (Ident l "..") e) = PrefixOp (Ident l "..") <$> ds e
            elm e = ds e

    -- Let and where
    --    (let e in b) --> e; b
    --    (e1 where e2)  -->   ( x ::= e1; e2; x)
    ds (Let e b) = do { e' <- ds e; b' <- ds b; pure (Seq [e',b']) }
    ds (InfixOp e1 (Op "where") e2) = do
      x <- newIdent (getLoc e1) "x"
      ds $ seqE [eDefine x e1, e2, Variable x]

    -- Do and case
    ds (Case1 b) = do
      let l = getLoc b
      x <- Variable <$> newIdent l "x"
      ds $ Function [(InfixOp x (Op ":") eAny, [])] $ Case2 x b
    ds (Case2 _ _) = undefined
    ds (Block b)   = ds b                                               -- do e --> e
    ds (Blk es)    = ds $ seqE es

    ds (Seq es) = seqE <$> mapM ds es
    ds (OfType e1 eff e2) = OfType <$> ds e1 <*> pure eff <*> ds e2

    -- Operators
    ds (PrefixOp (Op "not") e) = do e' <- ds e; pure $ If3 e' Fail eFalse
    ds (PrefixOp (Op ":") e) = Range [effSucceeds] <$> ds e
    ds (PrefixOp (Op "?") e) = do
      x <- Variable <$> newIdent (getLoc e) "x"
      let ee = Let (InfixOp x (Op ":") e) (Truth x)
      ds $ Typedef $ InfixOp eFalse (Op "|") ee
    ds (PrefixOp (Ident l op) e) = ds =<< call "pre" l op e
    ds (PostfixOp e (Ident l "?")) = ds $ ApplyD e (Variable (Ident l "_"))
    ds (PostfixOp e (Ident l op)) = ds =<< call "post" l op e
    ds (InfixOp e1 (Op "|") e2) = Choice <$> ds e1 <*> ds e2
    ds (InfixOp e1 (Op "and") e2) = ds $ Seq [e1, e2]                  -- XXX multiplicity?
    --ds (InfixOp e1 (Op "and") e2) = ds $ If3 e1 (If2E e2 Fail) Fail    -- XXX binding
    ds (InfixOp e1 (Op "or") e2) = ds $ If2E e1 $ If2E e2 Fail
    ds (InfixOp e1 (Ident l op) e2) = ds =<< call "in" l op (Array [e1, e2])

    -- Misc
    ds (Variable (Ident l "_")) = DefineV <$> newIdent l "u"
    ds (Option Nothing) = pure eFalse

    -- option{e}  -->  if(x:=e)then truth(e)
    ds (Option (Just e)) = do
      t <- newIdent (getLoc e) "t"
      ds $ If2 (eDefine t e) (Truth (Variable t))
    ds (Truth e) = ds $ Map [InfixOp e (Op "=>") e]

    -- one, all
    -- XXX why do we do this?
    ds (One e) = ds $ If2E e Fail
    ds (All e) = ds $ For1 e

    ds (Macro1 (Ident _ "first") [] e) = ds $ If2E e Fail  -- same as one{}
    ds (Macro2 (Ident _ "first") e1 e2) = ds $ If3 e1 e2 Fail

    ds (Exists xs b) = ds $ foldr (\ v e -> seqE [DefineV v, e]) b xs

    ds (Map es) | Just kvs <- mapM simpleMapEntry es =
      ds $ ApplyD (eMkMap noLoc) $ Array [ Array [k, v] | (k, v) <- kvs ]

    ds emap@(Map es) = do
      let loc = getLoc emap
      f <- Variable <$> newIdent loc "f"
      i <- Variable <$> newIdent loc "i"
      a <- Variable <$> newIdent loc "a"
      ds $ ApplyD (eMkMap loc) $
                  For2 (Seq [InfixOp f (Ident loc ":") (Array es),
                             InfixOp (InfixOp i (Ident loc "->") a) (Ident loc ":") f])
                       (Array [i, a])

    ds x = compos ds x

checkEffs :: [Eff] -> D [Eff]
checkEffs = mapM checkEff
  where
    checkEff (Ident _ "invariant") = pure closedId
    checkEff e | e `elem` knownEffects = pure e
               | otherwise = errorMessage $ "unknown effect: " ++ show e

simpleMapEntry :: SrcExpr -> Maybe (SrcExpr, SrcExpr)
simpleMapEntry (InfixOp k (Op "=>") v) = Just (k, v)
simpleMapEntry _ = Nothing

apply :: (SrcValue -> SrcValue -> SrcExpr) -> SrcExpr -> SrcExpr -> D SrcExpr
-- val1[e2]  -->
apply con e1 e2 | isValue e1 = apply1 con e1 e2   -- Easy special case.  Not really needed
-- e1[e2]  -->  f:=e1; f[e2]
apply con e1 e2 = do
  f <- newIdent (getLoc e1) "f"
  r <- apply1 con (Variable f) e2
  pure $ seqE [eDefine f e1, r]

apply1 :: (SrcValue -> SrcValue -> SrcExpr) -> SrcValue -> SrcExpr -> D SrcExpr
-- val1[val2]
apply1 con x1 e2 | isValue e2 = apply2 con x1 e2   -- Easy special case.  Not really needed
-- val1[e2]  -->  a:=e2; val1[a]
apply1 con x1 e2 = do
  a <- newIdent (getLoc e2) "a"
  r <- apply2 con x1 (Variable a)
  pure $ seqE [eDefine a e2, r]

-- val1[val2]  -->
apply2 :: (SrcValue -> SrcValue -> SrcExpr) -> SrcValue -> SrcValue -> D SrcExpr
apply2 con x1 x2 = pure $ con x1 x2

addSucceeds :: [Eff] -> [Eff]
addSucceeds [] = [effSucceeds]   -- Default is <succeeds>
addSucceeds fx = fx

defn :: SrcExpr -> SrcExpr -> D SrcExpr
-- Desugars (p := e) into an expression; see Fig 3, top group

-- Rule: (i := e) -->  (i := e)
defn (Variable (Ident _ "_")) e = do
  x <- newIdent (getLoc e) "u"
  pure $ eDefine x e

defn (Variable i) e = pure $ eDefine i e

-- Rule:   p(a)<fxs> := rhs   -->  p := fun(a){check<fxs>{rhs}}
--         Adding <succeeds> if not present
defn p e
  | (p1, fxs) <- getEffs p
  , Just (f, a, rs) <- getFun p1
  = defn f (Function [(a, rs)] (eCheck (addSucceeds fxs) e))

-- Rule: (e1<fx>:e2 := e)    -->  (e1 :=        e |>{fx} e2)
-- Rule: (f(a)<fx>:e2 := e)  -->  (e1 := fun(a){e |>{fx} e2})
-- but adding <succeeds> if omittec leaving behind <open/closed>
defn (InfixOp e1 (Op ":") e2) e
   = case getFun e1' of
       Just (f, a, rs) -> defn f (Function [(a,rs)] (OfType e fxs' e2))
       Nothing         -> defn e1'                  (OfType e fxs' e2)
  where
    (e1', fxs) = getEffs e1
    fxs' = addSucceeds fxs


-- Rule: (:e2) := e  -->  (x:e2) := e, x fresh
defn (PrefixOp op@(Op ":") e2) e
  = do { u <- newIdent (getLoc e2) "u"
       ; defn (InfixOp (Variable u) op e2) e }

-- Rule: (f(a) := e)  -->  (f := function(a){e})
-- Rule: (p<a> := e)  -->  ...

-- Rule: (p<fx> := e) -->  (p := check<fx>{e})
defn p@(EffAttr {}) e
  | not (null fxs) = defn p' (eCheck fxs e)
  where
    (p', fxs) = getEffs p

--defn (EffAttr e1 r) e v = defn e1 (applyEff [r] e) v
-- Rule: (p?) := e  -->  p := option{e}
--defn (PostfixOp p (Ident _ "?")) e = defn p (Option $ Just e)
-- Rule: (p1,...) := e  -->  (x1:any,...) = e; p1 := x1; ...

defn (Array ps) e = defnArray ps e

-- Rule (p1 -> p2) := e  -->  p1 := x1; p2 := x2; (x1 -> x2) := e
defn (InfixOp (Variable x1) (Op "->") (Variable x2)) e = pure $ DefineIE x1 x2 e
defn (InfixOp x1@Variable{} op@(Op "->") p2) e = do
  x2 <- Variable <$> newIdent (getLoc p2) "x"
  r2 <- defn p2 x2
  r  <- defn (InfixOp x1 op x2) e
  pure $ seqE [r2, r]
defn (InfixOp p1 op@(Op "->") p2) e = do
  x1 <- Variable <$> newIdent (getLoc p2) "x"
  r1 <- defn p1 x1
  r  <- defn (InfixOp x1 op p2) e
  pure $ seqE [r1, r]

defn p _ = errorMessage $ "Bad LHS to := " ++ prettyShow p
--defn p _ = impossible p



defnArray :: [SrcExpr] -> SrcExpr -> D SrcExpr
defnArray ps e = do
  let var p = do
        let (wrap, ip) =
              case p of
                PrefixOp (Ident l "..") p' -> (PrefixOp (Ident l ".."), p')
                _ -> (id, p)
        case ip of
          Variable v ->
            pure (Nothing, wrap (DefineV v))
          _ -> do
            x <- newIdent (getLoc p) "x"
            pure (Just (Variable x, ip), wrap (DefineV x))
  (xps, es) <- unzip <$> mapM var ps
  arr <- arraySplice es
  let (xs, ps') = unzip $ catMaybes xps
  bs <- zipWithM defn ps' xs
--  traceM ("*** " ++ show bs)
  pure $ seqE $ bs ++ [InfixOp arr (Op "=") e]

arraySplice :: [SrcExpr] -> D SrcExpr
arraySplice as =
--  trace ("--- " ++ show (as, arrayElems as)) $
  case arrayElems as of
    []          -> pure $ Array []
    e:es        -> app (arr e) $ map arr es
  where arr (EElems es) = Array es
        arr (ESplice e) = e
        app r [] = pure r
        app r (e : es) = do
          t <- newIdent noLoc "t"
          rest <- app (Variable t) es
          pure $ seqE [eAppend r e t, rest]

eAppend :: SrcExpr -> SrcExpr -> Ident -> SrcExpr
eAppend (Array xs) (Array ys) z = eDefine z (Array (xs ++ ys))
eAppend x y z = Seq [DefineV z, ApplyD (Variable (Ident noLoc "append$")) (Array [x, y, Variable z])]

data ArrayElem = EElems [SrcExpr] | ESplice SrcExpr
  deriving (Show)

-- Handle an array element, it can be ..e or e
arrayElems :: [SrcExpr] -> [ArrayElem]
arrayElems = grp . map cls
  where cls (PrefixOp (Ident _ "..") e) = Left e
        cls e = Right e
        grp [] = []
        grp (Left e : as) = ESplice e : grp as
        grp as =
          let (rs, bs) = span isRight as
          in  EElems [ e | Right e <- rs ] : grp bs

---------------------------------------------------------------------------------

dsDx :: SrcExpr -> D SrcExpr
dsDx e = do
  how <- getDFlagsX fDesugar
  case how of
    DS12 -> dsD_12 e


-- Pick the appropriate form of apply for operators
call :: String         -- "pre", "post", or "in" depending on prefix, postfix or infix
     -> Loc -> String  -- Function
     -> SrcExpr        -- Argumemt
     -> D SrcExpr
call p l s e = do
  ver <- getDFlagsX (not . fAssumeVerified)
  let
    -- For verification, use ApplyS.  At runtime, skip the test.
    con | ver && s' `elem` [
                     "pre'+'","pre'-'",
                     "in'+'","in'-'","in'*'"] = ApplyS
        | s' `elem` ["in'/'","pre'!'",
                     "pre'^'", "pre'[]'", "post'^'",  -- no need for succeeds
                     "pre'+'","pre'-'",  -- XXX not really right
                     "in'+'","in'-'","in'*'",  -- XXX not really right
                     "in'+='", "in'-='", "in'*='", "in'/='", "in'.='",
                     "in'='","in'<>'","in'<'","in'>'","in'<='","in'>='",
                     "length","in'..'"] = ApplyD
        | otherwise = ApplyS
    s' = p ++ "'" ++ s ++ "'"
  pure $ con (Variable (Ident l s')) e

----------------------------------------------

knownEffects :: [Ident]
knownEffects = map (Ident noLoc) [
  "succeeds", "decides", "iterates", "allocates", "reads", "writes", "interacts", "transacts", "open"
  ] ++ [closedId]

_isLambdaEffect :: Ident -> Bool
_isLambdaEffect i = elem i [
  closedId
  ]

closedId :: Ident
closedId = Ident noLoc "closed"

errUndefined :: [Ident] -> D ()
errUndefined is = do
  no_warn <- getDFlagsX fNoWarn
  if no_warn then
    case is of
      [] -> pure ()
      i@(Ident l _) : _ -> errorMessage $ "undefined: " ++ prettyShow (l, i)
   else
    mapM_ (\ i@(Ident l _) -> traceM $ "scopeCheck: warning undefined " ++ prettyShow (l, i)) is

errShadow :: [(Ident, Ident)] -> D ()
errShadow is = do
  no_warn <- getDFlagsX fNoWarn
  if no_warn then
    case is of
      [] -> pure ()
      (i@(Ident li _), (Ident lj _)) : _ -> errorMessage $ "shadowing: " ++ prettyShow (li, i, lj)
   else
    mapM_ (\ (i@(Ident li _), (Ident lj _)) -> traceM $ "warning shadowing " ++ prettyShow (li, i, lj)) is

errMultiple :: [[Ident]] -> D ()
errMultiple =
  mapM_ (\ is -> errorMessage $ "multiply defined: " ++ prettyShow (head is) ++
                         prettyShow [ l | Ident l _ <- is ])

--------------------------------------------------------
--
--         Simplification
--
--------------------------------------------------------

simplify :: SrcExpr -> D SrcExpr
simplify expr = do
  simpl <- getDFlagsX fSimplify
  if not simpl then
    pure expr
   else do
    let loop :: Int -> SrcExpr -> D SrcExpr
        loop 0 e = trace "simplifier timed out" $ pure e
        loop n e = do
          e' <- oneSimplifyPass e
          if e == e' then
            pure e
           else
            loop (n-1) e'
    loop 25 expr

oneSimplifyPass :: SrcExpr -> D SrcExpr
oneSimplifyPass expr = do
  tr <- getDFlagsX fTraceDesugar
  let traceDS :: String -> SrcExpr -> D SrcExpr
      traceDS msg e | tr = trace ("---- " ++ msg ++ "\n" ++ prettyShow e) $
                           pure e
                    | otherwise = pure e
  (traceDS "elimExist"  <=< elimExist <=<
   traceDS "alias"      <=< simpAlias <=<
   traceDS "simpler"    <=< simpler   <=<
   traceDS "inlineVal"  <=< inlineVal <=<
   pure ) expr

inlineVal :: SrcExpr -> D SrcExpr
inlineVal expr = do
  simpl <- getDFlagsX fSimplify
  if simpl then
    pure $ inlineVal' expr
   else
    pure expr

inlineVal' :: SrcExpr -> SrcExpr
inlineVal' expr = evalState (inl expr) []
  where
    inl :: SrcExpr -> State [(Ident, SrcExpr)] SrcExpr
    inl (Seq (e@(Unify (Variable x) v) : es)) | shouldInline x v = do
--      traceM $ "extend " ++ prettyShow (x, v)
      v' <- inl v
      es' <- withVal (x, v') (inl (Seq es))
      pure $ seqE [e, es']
    inl e@(Variable x) = do
      m <- get
      case lookup x m of
--        Just v | trace ("found " ++ prettyShow (x, v)) False -> undefined
        Just v | not (isLam v) -> pure v
        _                      -> pure e
    inl (ApplyD f@(Variable x) a) = do
      m <- get
      case lookup x m of
        Just elam@(Lam i e) | closed elam && i /= x ->
          inl $ Exists [i] $ Seq [ Unify (Variable i) a, e]
        _ -> ApplyD <$> inl f <*> inl a
    inl (Exists is e) = Exists is <$> withNotIn is  (inl e)
    inl (Verify is e) = Verify is <$> withNotIn is  (inl e)
    inl (Lam i e)     = Lam    i  <$> withNotIn [i] (inl e)
    inl e = compos inl e

    isLam Lam{} = True
    isLam _ = False
    shouldInline x (Lam _ _) = unIdent x `elem` ["int", "any"]
    shouldInline _ e = isValue e

    withVal :: (Ident, SrcExpr) -> State [(Ident, SrcExpr)] a -> State [(Ident, SrcExpr)] a
    withVal b ma = do
      m <- get
      pure $ evalState ma (b:m)
    withNotIn :: [Ident] -> State [(Ident, SrcExpr)] a -> State [(Ident, SrcExpr)] a
    withNotIn is ma = do
      m <- get
      let m' = filter ((`notElem` is) . fst) m
--      when (m /= m') $ traceM $ "dropped " ++ prettyShow (m, m')
      pure $ evalState ma m'

-- Eliminate existentials of the form
--  Exists x . ... x=e ...
-- where the x=e is the only occurrence of x.
elimExist :: SrcExpr -> D SrcExpr
elimExist expr = do
  simpl <- getDFlagsX fSimplify
  if simpl then
    pure $ elimE expr
   else
    pure expr
  where
    elimE (Exists [] e) = elimE e
    elimE (Exists (x:xs) e) =
      let e' = elimE (Exists xs e)
      in  case elimX x e' of
            Nothing  -> lExists [x] e'
            Just e'' -> e''
    elimE (If3B [] e1 e2 e3) = If3B [] (elimE e1) (elimE e2) (elimE e3)
    elimE (If3B (x:xs) e1 e2 e3) =
      let If3B xs' e1' e2' e3' = elimE (If3B xs e1 e2 e3)
      in  case elimX x (Choice e1' e2') of   -- just a random binary constructor
            Just (Choice e1'' e2'') -> If3B xs' e1'' e2'' e3'
            _ -> If3B (x:xs') e1' e2' e3'
    elimE For2B{} = error "unimplemented"
    elimE e = composOp elimE e

    --elimX x _ | trace ("----------------" ++ prettyShow x) False = undefined
    elimX x ex =
      let -- elm e | unIdent x == "$x18", trace ("e=" ++ prettyShow e) False = undefined
          elm (Unify (Variable y) e) | x == y = do tell (Sum (1::Int)); elm e
          elm e@(Variable y) | x == y = do tell (Sum 2); pure e
          elm e@(If3B _ e1 _ _) | occurs e1 = do tell (Sum 2); pure e
          elm e@(For2B _ e1 _) | occurs e1 = do tell (Sum 2); pure e
          elm e@(Split e1 _ _) | occurs e1 = do tell (Sum 2); pure e
          elm e = compos elm e
          occurs e = execWriter (elm e) /= 0  -- does x occur in e
      in  case runWriter (elm ex) of
--            xxx | unIdent x == "$x18", trace ("runWriter " ++ prettyShow (x, xxx)) False -> undefined
            (e', Sum n) | n <= 1 -> Just e'
            _                    -> Nothing

instance Pretty a => Pretty (Sum a) where
  pPrint (Sum a) = text "Sum" <+> pPrint a

simpler :: SrcExpr -> D SrcExpr
simpler expr = do
  -- Always remove silly uses of any$
  expr' <- simpValue <=< simpAny $ expr
  simpl <- getDFlagsX fSimplify
  if simpl then
    simpUnify expr'
   else
    pure expr'

-- Simplify  v; e  -->  e
simpValue :: SrcExpr -> D SrcExpr
simpValue = pure . simpValue'

simpValue' :: SrcExpr -> SrcExpr
simpValue' = f
  where f (Seq (Snoc es e)) = seqE $ map f (Snoc (filter (not . isValue') es) e)
        f e = composOp f e
        isValue' Lam{} = True   -- Also remove useless lambdas
        isValue' e = isValue e

{- Cannot do this everywhere, e.g., If3 relies on existentials
-- Simplify  exists . e  -->  e
simpExists :: SrcExpr -> D SrcExpr
simpExists = pure . f
  where f (Exists [] e) = f e
        f e = composOp f e
-}

-- Simplify any[e]  -->  e
simpAny :: SrcExpr -> D SrcExpr
simpAny = pure . f
  where f (ApplyD (Variable (Ident _ "any")) e) = f e  -- This should go away
        f (ApplyD (EPrim "any$") e) = f e
        f (EPrim "fail$") = Fail
        f e = composOp f e

-- Simplify x = (e1; ...; en)  -->  e1; ...; x = en
--          x = (y = e)  -->  x = y; y = e
simpUnify :: SrcExpr -> D SrcExpr
simpUnify = pure . f
  where f (Unify v (Seq (Snoc xs x))) | isValue v = f $ Seq $ xs ++ [Unify v x]
        f (Unify e1 (Unify v e2)) | isValue v = f $ Seq [Unify e1 v, Unify v e2]
        f (Seq es) = seqE $ map f es
        f e = composOp f e

-- If we have a unification x=y, and x&y are bound in the same existential
-- then we can get rid of one of the variables.
simpAlias :: SrcExpr -> D SrcExpr
simpAlias expr = do
  simpl <- getDFlagsX fSimplify
  if simpl then
    pure (f expr)
   else
    pure expr
  where f (Exists is ee) =
          -- The xys list are the identifiers where the x is among the existential
          -- bindings and x is bound locally to y.
          -- We will replace x by y, remove x from the bound variables, and remove the binding.
          let e = f ee
              xys = uniq [] $ Epic.List.nub [ xy | (x, y) <- localUnify e, xy <- pickBetter x y ]
              -- pickBetter x y | trace ("pickBetter " ++ show (x, y)) False = undefined
              pickBetter x y | x == y = []
              pickBetter x y | not xlocal && not ylocal = []
                             |     xlocal && not ylocal = [(x, y)]
                             | not xlocal &&     ylocal = [(y, x)]
                             where xlocal = x `elem` is; ylocal = y `elem` is
              -- Both are local, try to pick the one with the better name to remain.
              pickBetter x y | isTempIdent x || not (isTempIdent y) = [(x, y)]
                             | isTempIdent y || not (isTempIdent x) = [(y, x)]
                             | x < y = [(x, y)]
                             | otherwise = [(y, x)]
              is' = filter (`notElem` map fst xys) is
              e' = substMany [(x, Variable y) | (x, y) <- xys] $ dropUnify xys e
          in  --trace (show (is, xys)) $
              lExists is' e'
        -- Special hack to keep existentials in If3
        f (If3 (Exists is e1) e2 e3) = If3 (Exists is (f e1)) (f e2) (f e3)
        f (If3 e1 e2 e3) = If3 (f e1) (f e2) (f e3)
        f e = composOp f e

uniq :: [Ident] -> [(Ident, Ident)] -> [(Ident, Ident)]
uniq _ [] = []
uniq u (x@(a,b):xs) | a `elem` u || b `elem` u = uniq u xs
                    | otherwise = x : uniq (a:b:u) xs

dropUnify :: [(Ident, Ident)] -> SrcExpr -> SrcExpr
dropUnify xys (Seq es) = seqE (seqDrop (map (dropUnify xys) $ concatMap flat es))
  where flat (Seq xs) = concatMap flat xs
        flat x = [x]
dropUnify xys (Unify (Variable x) (Variable y)) | (x, y) `elem` xys = Variable y
                                                | (y, x) `elem` xys = Variable x
dropUnify _ e = e

seqDrop :: [SrcExpr] -> [SrcExpr]
seqDrop [] = []
seqDrop [e] = [e]
seqDrop (v:es@(_:_)) | isValue v = seqDrop es
seqDrop (e:es) = e : seqDrop es

localUnify :: SrcExpr -> [(Ident, Ident)]
localUnify (Seq es) = concatMap localUnify es
localUnify (Unify (Variable x) (Variable y)) = [(x, y)]
localUnify _ = []

isTempIdent :: Ident -> Bool
isTempIdent (Ident _ ('$':_)) = True
isTempIdent _ = False


--------------------------------------------------------
--
--         Adding dereferencing
--
--------------------------------------------------------

addDeref :: SrcExpr -> D SrcExpr
addDeref = pure . exprD S.empty
  where
    expr _ e@Lit{} = e
    expr s e@(Variable i) | i `S.member` s = applyPrimD "read$" e
                          | otherwise = e
    expr s (Array es) = Array $ map (expr s) es
    expr s (Seq es) = Seq $ map (expr s) es
    expr s (ApplyS e1 e2) = ApplyS (expr s e1) (expr s e2)
    expr s (ApplyD e1 e2) = ApplyD (expr s e1) (expr s e2)
    expr s (If3 e1 e2 e3) = If3 (expr s' e1) (expr s' e2) (exprD s e3)
      where s' = defs s e1
    expr s (For2 e1 e2) = For2 (expr s' e1) (exprD s' e2)
      where s' = defs s e1
    expr s (Let e1 e2) = Let (expr s' e1) (exprD s' e2)
      where s' = defs s e1
    expr s (Block e) = Block (exprD s e)
    expr s (Function [(a,rs)] e2) = Function [(a, rs)] (exprD s' e2)
      where s' = defs s a
    expr s (Unify e1 e2) = Unify (expr s e1) (expr s e2)
    expr _ (DefineV i) = DefineV i
    expr s (DefineE i e) = DefineE i (expr s e)
    expr s (DefineIE i j e) = DefineIE i j (expr s e)
    expr s (Choice e1 e2) = Choice (exprD s e1) (exprD s e2)
    expr s (Set e1 (Ident l sop) e2) = set s e1 op (expr s e2)
      where op = Ident l ("in'" ++ sop ++ "'")
    expr s (MVar i (Just t) (Just e)) = DefineE i $ ApplyD (applyPrimD "new" (expr s t)) (expr s e)
    expr s (Range fx e1) = Range fx (expr s e1)
--    expr s (Typedef e1) = Typedef (exprD s e1)
    expr s (Macro1 m rs e1) = Macro1 m rs (exprD s e1)
    expr s (Exists is e) = Exists is (expr s e)
    expr s (OfType e fx t) = OfType (expr s e) fx (expr s t)
    expr _ Fail = Fail
    expr s (Lam i e) = Lam i (expr s e)
    expr _ e@EPrim{} = e
--    expr s (Map es) = Map $ map (expr s) es
    expr _ e = impossible e

    exprD s e = expr (defs s e) e

    set s e1 (Ident l "in'='") e2 = set s e1 (Ident l "write$") e2
    set s e1@(Variable i) op@(Ident l _) e2
      | i `S.member` s = ApplyD (Variable op) $ Array [e1, e2]
      | otherwise = syntaxError l $ "set variable must be declared with var: " ++ prettyShow i
    set s (ApplyD e1@(Variable i) ei) (Ident l sop) e2
      | i `S.member` s = ApplyD (Variable (Ident l (sop++"[]"))) $ Array [e1, ei, e2]
      | otherwise = syntaxError l $ "set variable must be declared with var: " ++ prettyShow i
    set _ e1 _ _ = syntaxError (getLoc e1) $ "set LHS not valid: " ++ prettyShow e1

    defs :: S.Set Ident -> SrcExpr -> S.Set Ident
    defs s e = S.union s (S.fromList (getVar e))

    applyPrimD s e = ApplyD (Variable (Ident noLoc s)) e



--------------------------------------------------------
--
--             Adding scopes
--
--    Replace  x:=t by   exits x. ...(x=t)...
--
--------------------------------------------------------

addScope :: SrcExpr -> D SrcExpr
addScope e = scope (S.fromList primOps) (Block e)

scope :: S.Set Ident -> SrcExpr -> D SrcExpr
-- The input expression is in BigCore, after desugaring,
-- but still with x := e stuff
-- In  (scope sc expr), `sc` is a set of identifiers already in scope
--     to allow us to complain about shadowing
scope sc = expr
  where
    -- x := e  -->   x = e
    expr (DefineV i)   = pure $ Variable i
    expr (DefineE i e) = Unify (Variable i) <$> expr e

    expr e@Lit{}   = pure e
    expr e@EPrim{} = pure e
    expr e@(Variable i) | i `S.member` sc = pure e
                        | otherwise = do errUndefined [i]; pure e
    expr (Array es) = Array <$> mapM expr es
    expr (Seq es) = seqE <$> mapM expr es
    expr (ApplyD e1 e2) = ApplyD <$> expr e1 <*> expr e2

    expr (If3 e1 e2 e3) = do
      (is, e1', sc') <- defs' sc e1
      If3B is e1' <$> scopeD sc' e2 <*> exprD e3
    expr (For2 e1 e2) = do
      (is, e1', sc') <- defs' sc e1
      For2B is e1' <$> scopeD sc' e2

    expr (Block e) = exprD e
    expr (Let e1 e2) = do { (is, e1'', sc') <- defs' sc e1
                          ; e2' <- scope sc' e2
                          ; pure $ eExists is $ seqE [e1'', e2'] }

    expr (Unify e1 e2) = Unify <$> expr e1 <*> expr e2

    expr (Choice e1 e2) = Choice <$> exprD e1 <*> exprD e2
    expr Fail           = pure Fail

    expr (Check fx e) = Check fx <$> exprD e
    expr (Some e)     = Some <$> expr e
    expr (Guard v e)  = Guard <$> expr v <*> expr e

    expr (OfType e1 eff e2) = OfType <$> exprD e1 <*> pure eff <*> exprD e2

    expr (Exists _ e)  = expr e
    expr (Lam i e)     = Lam i <$> scopeD (S.insert i sc) e
    expr (Verify is e) = Verify is <$> scopeD sc' e
      where sc' = foldr S.insert sc is
    expr e = impossible e

    exprD e = fst <$> defs sc e
    scopeD s e = fst <$> defs s e

    defs :: S.Set Ident -> SrcExpr -> D (SrcExpr, S.Set Ident)
    -- `e` starts a new scoping context.  Wrap it in an `Exists`
    defs as e = do
      (is, e', s) <- defs' as e
      pure (eExists is e', s)

    defs' :: S.Set Ident -> SrcExpr -> D ([Ident], SrcExpr, S.Set Ident)
    -- Find identifers bound in `e`, and return them
    -- along with extended scope-set and transformed `e`.
    defs' as e = do
      let -- Get all binders from e
          is = getVisibleBinders e
          -- errM: find ones that are defined more than once
          errM = filter ((> 1) . length) $ group $ sort is

          -- errS: find ones that are already in scope
          errS = [ (i, j) | i <- is, i `S.member` sc, j <- filter (== i) (S.toList sc) ]
          s' :: S.Set Ident = foldr S.insert as is
      e' <- scope s' e
      errMultiple errM
      errShadow errS
      pure (is, e', s')


--------------------------------------------------------
--
--         Lowering (whatever that is)
--
--------------------------------------------------------

------------------------------------------------------------------------------------

{-
-- Applications have to be lowered before scope insertion
-- so existential get inserted in the right place.
lowerApply :: SrcExpr -> D SrcExpr
lowerApply = f
  where
    f (ApplyS e1 e2) = Succeeds <$> (ApplyD <$> f e1 <*> f e2)
    f (OfType e fx t) = do
      verif <- gets (fVerify . dflags)
      if verif then
        OfType <$> f e <*> pure fx <*> f t
       else
        Succeeds <$> (ApplyD <$> f t <*> f e)
    f e = compos f e

-- Convert Big Core to Core
lower :: SrcExpr -> D SrcExpr
lower e@Lit{} = pure e
lower e@Variable{} = pure e
lower (Array es) = Array <$> mapM lower es
lower e@Wrong{} = pure e
lower (Seq es) = seqE <$> mapM lower es
lower (ApplyD e1 e2) = ApplyD <$> lower e1 <*> lower e2
lower (Unify e1 e2) = Unify <$> lower e1 <*> lower e2
lower (Choice e1 e2) = Choice <$> lower e1 <*> lower e2
lower (For2B is e1 e2) = join $ lowerFor is <$> lower e1 <*> lower e2
lower (If3B is e1 e2 e3) = join $ lowerIf is <$> lower e1 <*> lower e2 <*> lower e3
lower (Macro1 (Ident _ "all") [] e)    = lowerAll =<< lower e
lower (Macro1 (Ident _ "one") [] e)    = lowerOne =<< lower e
lower (Macro1 (Ident _ "some") [] e)   = lowerSome   =<< lower e
lower (Macro2 (Ident _ "guard") e1 e2) = eGuard <$> lower e1 <*> lower e2
lower (Exists is e) = lExists is <$> lower e
lower (Lam i e) = Lam i <$> lower e
lower Fail = pure Fail
lower (Verify is e) = Verify is <$> lower e
lower e = impossible e

-- Lower a for loop
lowerFor :: [Ident] -> SrcExpr -> SrcExpr -> D SrcExpr
lowerFor is e1 e2 = do
  useSplit <- gets (fSplit . dflags)
  if useSplit then
    lowerForSplit is e1 e2
   else
    lowerForAll is e1 e2

-- Lower for loop using split
-- TODO: special case 'for{e}'
lowerForSplit :: [Ident] -> SrcExpr -> SrcExpr -> D SrcExpr
lowerForSplit vs e1 e2 = do
  let l = getLoc e1
  x <- newIdent l "x"   -- array of free variables
  y <- newIdent l "y"   -- thunked result
  h <- newIdent l "h"   -- h = ge, but passed to split to avoid recursion
  let fvs = vs `intersect` getFree e2
      evs = fvArray (map Variable fvs)
      e1' = lExists vs $ Seq [e1, evs]  -- domain + array of free variables
      be  = Split (eForce (Variable y)) fe (Variable h)
      fe = eThunk $ Array []
      ge = Lam x $ Lam y $ Lam h $ lExists fvs $ Seq
             [ Unify (Variable x) evs
             , ApplyD (EPrim "cons$") (Array [e2, be])
             ]
  pure $ Split e1' fe ge

-- Lower for loop using all
lowerForAll :: [Ident] -> SrcExpr -> SrcExpr -> D SrcExpr
lowerForAll (i:is) (Unify (Variable i') e) (Variable i'') | i == i' && i == i'' =
  -- Simple special case: for{e} = all{e}
  pure $ eAll (lExists is e)
lowerForAll is e1 e2 = do
  vv <- newIdent (getLoc e1) "v"
  let ev = Variable vv
      ea = eAll $ lExists is $ Seq [e1, eThunk e2]
  pure $ Exists [vv] $ Seq [Unify ev ea, ApplyD (EPrim "mapAp$") ev]

lowerIf :: [Ident] -> SrcExpr -> SrcExpr -> SrcExpr -> D SrcExpr
lowerIf is e1 e2 e3 = do
  noLambdaIf <- gets (fNoLambdaIf . dflags)
  useSplit <- gets (fSplit . dflags)
  verif <- gets (fVerify . dflags)
  keepIf <- gets (fKeepIf . dflags)
  if verif || keepIf then
    lowerIfVerify is e1 e2 e3
   else if noLambdaIf then
    lowerIfNoLambda is e1 e2 e3
   else if useSplit then
    lowerIfSplit is e1 e2 e3
   else
    lowerIfOne is e1 e2 e3

lowerIfVerify :: [Ident] -> SrcExpr -> SrcExpr -> SrcExpr -> D SrcExpr
lowerIfVerify is e1 e2 e3 = pure $ If3B is e1 e2 e3

lowerIfNoLambda :: [Ident] -> SrcExpr -> SrcExpr -> SrcExpr -> D SrcExpr
lowerIfNoLambda vs e1 e2 e3 = do
  y <- newIdent (getLoc e1) "y"
  let vy = Variable y
      fvs = vs `intersect` getFree e2
      evs = fvArray (map Variable fvs)
  pure $ Exists [y] $ Seq
           [ Unify vy (eOne $ lExists vs (Seq [e1, evs])
                              `Choice`
                              Lit (LitInt 0))
           , lExists fvs (Seq [Unify vy evs, e2])
             `Choice`
             Seq [Unify vy (Lit (LitInt 0)), e3]
           ]

-- TODO: special case 'if{}'
lowerIfSplit :: [Ident] -> SrcExpr -> SrcExpr -> SrcExpr -> D SrcExpr
lowerIfSplit vs e1 e2 e3 = do
  x <- newIdent (getLoc e1) "x"
  let fvs = vs `intersect` getFree e2
      evs = fvArray (map Variable fvs)
      e1' = lExists vs $ Seq [e1, evs]  -- domain + array of free variables
      fe = eThunk e3
      ge = Lam x $ Lam underscore $ Lam underscore $ lExists fvs $ Seq
             [ Unify (Variable x) evs
             , e2
             ]
  pure $ Split e1' fe ge

lowerIfOne :: [Ident] -> SrcExpr -> SrcExpr -> SrcExpr -> D SrcExpr
lowerIfOne is e1 e2 e3 = do
  let e1e2 = lExists is $ Seq [e1, eThunk e2]
  pure $ eForce $ eOne $ Choice e1e2 (eThunk e3)


lowerAll :: SrcExpr -> D SrcExpr
lowerAll e = do
  useSplit <- gets (fSplit . dflags)
  if useSplit then
    lowerAllSplit e
   else
    pure $ eAll e

lowerAllSplit :: SrcExpr -> D SrcExpr
lowerAllSplit e = do
  let l = getLoc e
  x <- newIdent l "x"   -- array of free variables
  y <- newIdent l "y"   -- thunked result
  h <- newIdent l "h"   -- h = ge, but passed to split to avoid recursion
  let xs = Split (eForce $ Variable y) fe (Variable h)
      fe = eThunk $ Array []
      ge = Lam x $ Lam y $ Lam h $
             ApplyD (EPrim "cons$") (Array [Variable x, xs])
  pure $ Split e fe ge

lowerOne :: SrcExpr -> D SrcExpr
lowerOne e = do
  useSplit <- gets (fSplit . dflags)
  if useSplit then
    lowerOneSplit e
   else
    pure $ eOne e

lowerOneSplit :: SrcExpr -> D SrcExpr
lowerOneSplit e = do
  v <- newIdent (getLoc e) "v"
  pure $ Split e (eThunk Fail) (Lam v $ Lam underscore $ Lam underscore $ Variable v)

{-
lowerSucceeds :: SrcExpr -> D SrcExpr
lowerSucceeds e = do
  useSplit <- gets (fSplit . dflags)
  verif <- gets (fVerify . dflags)
  asmVerif <- gets (fAssumeVerified . dflags)
  if verif then
      pure $ eAssert e
   else if asmVerif then
    pure $ e
   else if useSplit then
    lowerSucceedsSplit e
   else
    pure $ Succeeds e

lowerSucceedsSplit :: SrcExpr -> D SrcExpr
lowerSucceedsSplit e = do
  let l = getLoc e
  x <- newIdent l "x"   -- array of free variables
  y <- newIdent l "y"   -- thunked result
  pure $ Split e
               (eThunk $ Wrong "succeeds-fail") $
               Lam x $ Lam y $ Lam underscore $
                   Split (eForce (Variable y))
                         (eThunk (Variable x))
                         (Lam underscore $ Lam underscore $ Lam underscore $ Wrong "succeed-many")

lowerDecides :: SrcExpr -> D SrcExpr
lowerDecides e = do
  useSplit <- gets (fSplit . dflags)
  verif <- gets (fVerify . dflags)
  -- if verif then
  --   unimplemented "verify-decides"
  --  else
  if useSplit && not verif then
    lowerDecidesSplit e
  else
    pure $ eDecide  e

lowerDecidesSplit :: SrcExpr -> D SrcExpr
lowerDecidesSplit e = do
  let l = getLoc e
  x <- newIdent l "x"   -- array of free variables
  y <- newIdent l "y"   -- thunked result
  pure $ Split e
               (eThunk Fail) $
               Lam x $ Lam y $ Lam underscore $
                   Split (eForce (Variable y))
                         (eThunk (Variable x))
                         (Lam underscore $ Lam underscore $ Lam underscore $ Wrong "decides-many")

lowerAssume :: SrcExpr -> D SrcExpr
lowerAssume e = pure $ eAssume e
lowerAssert :: SrcExpr -> D SrcExpr
lowerAssert e = pure $ eAssert e

-}

lowerSome :: SrcExpr -> D SrcExpr
lowerSome e = pure $ eSome e

-}


--------------------------------------------------------
--
--         Primops
--
--------------------------------------------------------

primops :: SrcExpr -> D SrcExpr
-- Replace (Variable (Ident "op")) with (EPrim "op")
primops = f
  where
    f (Variable (Ident _ s)) | Ident noLoc s `S.member` prim = pure $ EPrim s
    f e = compos f e
    prim = S.fromList primOps

-- Primitives
primOps :: [Ident]
primOps = map (Ident noLoc)
  [ "isInt$", "isRat$", "isChr$", "isF32$", "isF64$", "isStr$"
  , "isPtr$", "isArr$", "isFcn$", "isPath$", "isMap$"

  , "intAdd$", "intSub$", "intMul$", "intDiv$", "intNeg$", "intPlus$"
  , "intLT$", "intLE$", "intGT$", "intGE$", "intNE$"

  , "ratAdd$", "ratSub$", "ratMul$", "ratDiv$", "ratNeg$", "ratPlus$"
  , "ratLT$", "ratLE$", "ratGT$", "ratGE$", "ratNE$"

  , "f32Add$", "f32Sub$", "f32Mul$", "f32Div$", "f32Neg$", "f32Plus$"
  , "f32LT$", "f32LE$", "f32GT$", "f32GE$", "f32NE$"

  , "f64Add$", "f64Sub$", "f64Mul$", "f64Div$", "f64Neg$", "f64Plus$"
  , "f64LT$", "f64LE$", "f64GT$", "f64GE$", "f64NE$"


  , "mkMap$"
  , "concat$", "cons$"
  , "length$"
  , "alloc$", "read$", "write$"
  , "in'..'"
  , "in'+'",  "in'-'",  "in'*'",  "in'/'"
  , "in'+='", "in'-='", "in'*='", "in'/='"
  , "print$"
  , "append$"
  , "known$"  -- This is a horrible hack
  , "any$"
  , "fail$"
  , "err$"
  , "arrLen$"
  , "arrConc$"
  ]


{-
  where
    f (ApplyD g@(Variable (Ident _ s)) v) = do
      mfunc <- lowerPrimOp s
      v' <- f v
      case mfunc of
        Nothing -> pure $ ApplyD g v'
        Just func -> pure $ app func v'
    f e@(Variable (Ident _ s)) = do
      mfunc <- lowerPrimOp s
      pure $ maybe e toLam mfunc
    f e = compos f e

    app ([a], es) v | isValue v = substMany [(a, v)] $ seqE es
    app ([a1,a2], es) (Array [v1,v2]) | isValue v1 && isValue v2 = substMany [(a1, v1), (a2, v2)] $ seqE es
    app func v = ApplyD (toLam func) v

    arg = Ident noLoc "arg"
    toLam :: Func -> SrcCore
    toLam (   [],    es) = seqE es
    toLam (   [a],   es) = Lam a   $ seqE es
    toLam (as@[_,_], es) = Lam arg $ lExists as $ seqE $ Unify (Array $ map Variable as) (Variable arg) : es
    toLam _ = undefined  -- shouldn't happen

-- Some "primops" will be expanded into code.
-- This should really be part of the prelude.
-- For verification, we need a different expansion.
lowerPrimOp :: String -> D (Maybe Func)
lowerPrimOp s = do
  if Ident noLoc s `elem` primOps then
    pure $ Just ([], [EPrim s])
  else
    pure $ Nothing

type Func = ([Ident], [SrcCore])

lowerPrimOpVerif :: String -> D (Maybe Func)
lowerPrimOpVerif s = do
  me <- lowerPrimOpRun s
  case me of
    Just ([], [EPrim p]) | Just func <- lookup p verifyPrelude -> pure $ Just func
    Nothing              | Just func <- lookup s verifyPrelude -> pure $ Just func
    r -> pure r

lowerPrimOpRun :: String -> D (Maybe Func)
lowerPrimOpRun s =
  case lookup s preludeFuncs of
    r@Just{} -> pure r
    Nothing  ->
      if Ident noLoc s `elem` primOps then
        pure $ Just ([], [EPrim s])
      else
        pure $ Nothing

-- Definitions that should go in a Prelude
preludeIds :: [Ident]
preludeIds = map (Ident noLoc . fst) preludeFuncs

preludeFuncs :: [(String, Func)]
preludeFuncs =
  [("any", typ [])                                             -- x => x
  ,("nat", typ [app "isInt$" vx, app2 "in'>='" vx (Lit (LitInt 0))]) -- x => int#[x]; x>=0; x
  ,("int", typ [app "isInt$" vx])                              -- x => int#[x]; x
  ,("rat", typ [app "isRat$" vx])                              -- x => rat#[x]; x
  ,("char", typ [app "isChr$" vx])                             -- x => char#[x]; x
  ,("string", typ [app "isStr$" vx])                           -- x => string#[x]; x
  ,("in'->'", arrowV)
  ,("false", bare $ Array [])                                      -- ()
  ,("new", newV)
  ,("post'^'", bare $ EPrim "read$")
  ,("in'.='", bare $ EPrim "write$")
  ,("mapAp", bare $ EPrim "mapAp$")
  ]
  where bare e = ([], [e])
        typ es = ([x], es ++ [Variable x])
        vx = Variable x
        app f v = ApplyD (EPrim f) v
        app2 f v1 v2 = ApplyD (EPrim f) (Array [v1, v2])

        arrowV = ([s, t],
              [ Lam g $ Lam y $
                Exists [sy, gsy] $
                Seq [
                  app "isFcn$" (Variable g),
                  Unify (Variable  sy) (ApplyD (Variable s) (Variable y)),
                  Unify (Variable gsy) (ApplyD (Variable g) (Variable sy)),
                  ApplyD (Variable t) (Variable gsy)
                  ]
              ])
        [s, t, g, y, sy, gsy, x, _xy] =
           map (Ident noLoc . ("$$" ++)) ["s","t","g","y","sy","gsy","x", "xy"]

        newV = ([t],
                [ Lam x $
                  Exists [y] $
                  Seq [
                    Unify (Variable y) (ApplyD (Variable t) (Variable x)),
                    app "alloc$" (Variable y)
                    ]
                ])

verifyPrelude :: [(String, Func)]
verifyPrelude =
  [ arithBinOpInt  "in'+'" "intAdd$"
  , arithBinOpInt  "in'-'" "intSub$"
  , arithBinOpInt  "in'*'" "intMul$"
  , arithBinOpIntC "in'/'" "intDiv$" yNe0
  , arithUnOpInt   "pre'-'" "intNeg$"
  , arithUnOpInt   "pre'+'" "intPlus$"
  , cmpBinOpInt    "in'<'"  "intLT$"
  , cmpBinOpInt    "in'<='" "intLE$"
  , cmpBinOpInt    "in'>'"  "intGT$"
  , cmpBinOpInt    "in'>='" "intGE$"
  , cmpBinOpInt    "in'<>'" "intNE$"
  ]
  where
    arithBinOpInt  p s = (p, arithBinOpInt' [] s)
    arithBinOpIntC p s c = (p, arithBinOpInt' [c] s)
    arithBinOpInt' c p = ([x, y],
      [ cInt vx, cInt vy] ++ c ++
      [ eAssume $ Exists [z] $ Seq [Unify vz (ApplyD (EPrim p) (Array [vx, vy])), cInt vz, vz] ])

    cmpBinOpInt  p s = (p, cmpBinOpInt' s)
    cmpBinOpInt' p = ([x, y],
      [ cInt vx, cInt vy, ApplyD (EPrim p) (Array [vx, vy]), eAssume (Seq [cInt vx, vx]) ])

    yNe0 = ApplyD (EPrim "intNE$") (Array [vy, Lit (LitInt 0)])

    arithUnOpInt p s =
      (p, ([x], [ cInt vx, eAssume (Exists [z] $ Seq [Unify vz (ApplyD (EPrim s) vx), cInt vz, vz]) ]))

    cInt e = ApplyD (EPrim "isInt$") e
    x = Ident noLoc "$$x"
    y = Ident noLoc "$$y"
    z = Ident noLoc "$$z"
    vx = Variable x
    vy = Variable y
    vz = Variable z
-}

-----------------------------------------------


--------------------------------------------------------
--         Identifiers
--------------------------------------------------------

underscore :: Ident
underscore = Ident noLoc "_"

idX :: Ident
idX = Ident noLoc "x"

--------------------------------------------------------
--         Functions to take apart SrcExpr
--------------------------------------------------------

-- Return function, argument, and attributes
getFun :: SrcExpr -> Maybe (SrcExpr, SrcExpr, [Ident])
getFun = gf []
  where
    gf rs (EffAttr e r) = gf (r:rs) e
    gf rs (ApplyS f a) = Just (f, a, reverse rs)
    gf _ _ = Nothing

getEffs :: SrcExpr -> (SrcExpr, [Eff])  -- e<fx1><fx2> --> (e, [fx1,fx2])
getEffs orig_e = go orig_e []
  where
    go (EffAttr e fx) fxs
     | isOpenClosed fx    = wrap fx (go e fxs)
     | otherwise          = go e (fx:fxs)
    go e              fxs = (e, fxs)

    wrap fx (e, fxs) = (EffAttr e fx, fxs)

--------------------------------------------------------
--         Functions to construct SrcExpr
--------------------------------------------------------

eFalse :: SrcExpr
eFalse = Array []

eAny :: SrcExpr
eAny = Variable (Ident noLoc "any$")

eMkMap :: Loc -> SrcExpr
eMkMap l = Variable (Ident l "mkMap$")


eHavoc :: [Eff] -> SrcExpr
eHavoc fx = seqE (map havoc1 fx)
  where
    havoc1 x | x == effSucceeds = seqE []
             | x == effFails    = Fail
             | x == effDecides  = Unify (eSome (Lam idX (Variable idX))) (Array [])
             | otherwise        = errorMessage $ "eHavoc: " ++ show fx

eThunk :: SrcExpr -> SrcExpr
eThunk = Lam (Ident noLoc "_")

eForce :: SrcExpr -> SrcExpr
eForce e = ApplyD e (Array [])

eAll :: SrcExpr -> SrcExpr
eAll = All

eOne :: SrcExpr -> SrcExpr
eOne = One

eVerify :: [Ident] -> SrcExpr -> SrcExpr
eVerify = Verify

eSome :: SrcExpr -> SrcExpr
eSome = Some

eGuard :: [Ident] -> SrcExpr -> SrcExpr
-- Smart constructor, drops empty guard
eGuard xs e
  | null xs   = e
  | otherwise = Guard (fvArray xs) e

eCheck :: [Eff] -> SrcExpr -> SrcExpr
eCheck fxs1 e
  | Check fxs2 e' <- e  = Check (fxs1 ++ fxs2) e'
  | otherwise           = Check fxs1 e

eExists :: [Ident] -> SrcExpr -> SrcExpr
-- Smart constructor, drops empty list of binders
eExists [] e = e
eExists is e = Exists is e

eDefine :: Ident -> SrcExpr -> SrcExpr
-- Smart contructor, floats out nested defines
eDefine x (Seq ts) = seqE (floats ++ [eDefine x rhs])
                   where
                     (floats, rhs) = unSeq ts
eDefine x rhs = DefineE x rhs

eApplyD :: SrcExpr -> SrcExpr -> SrcExpr
-- (eApply f x)  returns  f[x]
-- But has a short-cut for any$[x]
eApplyD (EPrim "any$") x = x
eApplyD f              x = ApplyD f x

-- Used to create the array of free variables passed from the domain to the range
-- of for/if.  If it's just a single variable, don't use an array.
fvArray :: [Ident] -> SrcExpr
fvArray [x] = Variable x
fvArray xs = Array (map Variable xs)

-- After lowering there are no funny scopes, so empty existential
-- are no longer necessary.
-- ToDo: why are empty existentials ever necessary?
lExists :: [Ident] -> SrcExpr -> SrcExpr
lExists [] e = e
lExists is (Exists is' e) = lExists (is ++ is') e
lExists is e = Exists is e

--------------------------------------------------------
--
--         Desugaring Small Source -> Big Core
--
--------------------------------------------------------

data Pi
  = P SrcExpr -- ^ P(x)    The SrcExpr is always small and freely duplicable
              --           Typically just (Variable x)
  | E         -- ^ E
  deriving (Eq, Ord, Show)

data DsMode12
  = MX -- ^ x "execution"
  | MV -- ^ + "verification"
  | MI -- ^ - "checking" ("implementation")
  deriving (Eq, Ord, Show)

dsD_12 :: SrcExpr -> D SrcExpr
dsD_12 = dsDD_12 MV

dsDD_12 :: DsMode12 -> SrcExpr -> D SrcExpr
dsDD_12 s t = dsM_12 s t E


dsB_12 :: DsMode12 -> SrcExpr -> Pi -> SrcExpr -> D SrcExpr
dsB_12 s t E     _
  = dsM_12 s t E
dsB_12 s t (P f) j
  = do z <- newIdent (getLoc t) "z";
       seqDE [ pure $ eDefine z (ApplyD f j)
             , dsM_12 s t (P (Variable z))]

seqDE :: [D SrcExpr] -> D SrcExpr
seqDE ds = seqE <$> sequence ds

defineDE :: String -> D SrcExpr
         -> D (SrcExpr,   -- The defn
               SrcExpr)   -- The use
-- define "x" de   returns   x := e, along with x itself
-- But (just to save clutter) if the rhs turns out to be tiny,
--   just return it alone
defineDE nm ds_rhs
  = do { rhs' <- ds_rhs
       ; if isAtomic rhs'
         then pure (seqE [], rhs')
         else do { x <- newIdent (getLoc rhs') nm
                 ; pure (eDefine x rhs', Variable x) } }

defineDE2 :: String
          -> D SrcExpr               -- The RHS
          -> (SrcExpr -> D SrcExpr)  -- The body
          -> D SrcExpr               -- z := rhs; body
defineDE2 nm ds_rhs ds_body
  = do { rhs' <- ds_rhs
       ; if isAtomic rhs'
         then ds_body rhs'
         else do { x <- newIdent (getLoc rhs') nm
                 ; body' <- ds_body (Variable x)
                 ; if x `elem` getAllIdents body'  -- See Note [Avoiding clutter]
                   then pure (seqE [eDefine x rhs', body'])
                   else pure (seqE [rhs',           body']) } }

{- Note [Avoiding clutter]
~~~~~~~~~~~~~~~~~~~~~~~~~~
In defineDE2 we are building a term looking like:

   x := rhs
   ...body...

where `x` is complely fresh.   We can abbreviate this if

* `rhs` is atomic; then just use `rhs` rather than `x`

* `x` is not used in rhs.  Since `rhs` is a SrcExpr, its binding structure
  is not obvious, and it's awkward to get its true free variables. But since
  `x` is fresh anyway it suffices to look for /any/ occurrence of `x`. At worst
  we'll create a binding we don't really need.

  Hence the user of `getAllIdents`.
-}

dsM_12 :: DsMode12 -> SrcExpr -> Pi -> D SrcExpr

-------------------- Functions -----------------------
dsM_12 MV t@(Function [(t1, _fx)] t2) pi        -- MCFUN+
  = do r <- newIdent (getLoc t) "r"
       body <- defineDE2 "j" (dsM_12 MI t1 (P (Variable r)))
                             (dsB_12 MV t2 pi)
       seqDE [ pure (eVerify [r] body)
             , dsM_12 MI t pi ]

dsM_12 MI (Function [(t1, _fx)] t2) pi        -- MCFUN-
  = do i   <- newIdent (getLoc t1) "i"
       body <- defineDE2 "j" (dsM_12 MV t1 (P (Variable i)))
                             (dsB_12 MI t2 pi)
       pure $ {- TODO: ISFUN -} Lam i body

dsM_12 MX (Function [(t1, _fx)] t2) pi        -- MCFUNX
  = do i   <- newIdent (getLoc t1) "i"
       body <- defineDE2 "j" (dsM_12 MX t1 (P (Variable i)))
                             (dsB_12 MX t2 pi)
       pure $ Lam i body

-------------------- e |>{fx} t -----------------------
dsM_12 MV t@(OfType t1 fx t2) pi      -- MOFTYPE+
    = seqDE [ eCheck fx <$> verify_body
            , dsM_12 MI t pi ]
-- ToDo: check this; not what is in the doc
--  = seqDE [ eVerify [] <$> (eCheck fx <$> verify_body)
--          , dsM_12 MI t pi ]
  where
    verify_body = do { (e1, x) <- defineDE "x" (dsM_12 MV t1 pi)
                     ; (e2, z) <- defineDE "z" (dsDD_12 MV t2)
                     ; pure (seqE [e1, e2, eApplyD z x]) }

dsM_12 MI (OfType t1 fx t2) pi      -- MOFTYPE-
    -- ToDo: fx and pi are unused, which seems suspicious
  = do { (e2, z) <- defineDE "z" (dsDD_12 MI t2)
       ; pure (seqE [ e2
                    , eHavoc fx
                    , eGuard (getFree t1) (eSome z)]) }

dsM_12 s (OfType t1 fx t2) pi      -- MOFTYPE2
  = do { (e1, x) <- defineDE "x" (dsM_12 s t1 pi)
       ; e2 <- dsM_12 s (Range fx t2) (P x)
       ; pure (seqE [e1,e2]) }

-------------------- :{fx} t -----------------
dsM_12 MI (Range fx t) (P i)                 -- MTYPE1
  = do { (e, z) <- defineDE "z" (dsDD_12 MI t)
--       ; pure (seqE [e, eHavoc fx, eGuard i (eSome z) ]) }
-- ToDo: check this... it's not what is in the doc yet
       ; pure (seqE [e, eHavoc fx, eApplyD z i ]) }

dsM_12 s (Range fx t) (P i)                  -- MTYPE2  s = {+,x}
  = do { (e, z) <- defineDE "z" (dsDD_12 s t)
-- ToDo: the eCheck is new
       ; pure (eCheck fx $ seqE [e, eApplyD z i]) }   -- z[x]

dsM_12 s (Range fx t) E                      -- MTYPE3
    -- ToDo: what about <fx>?
  = do { x <- newIdent (getLoc t) "x"
       ; (e, z) <- defineDE "z" (dsDD_12 s t)
       ; let z_app = eApplyD z (Variable x)   -- z[x]
       ; pure (Exists [x] (seqE [e, z_app])) }

-------------------- check<fx>{t} -----------------
dsM_12 MI (Check _fx t) pi                  -- MCHECK-
  = dsM_12 MI t pi

dsM_12 s (Check fx t) pi                   -- MCHECK+X
  = Check fx <$> dsM_12 s t pi

-------------------- (x~>y) := t -----------------
dsM_12 s (DefineIE x y t) E                -- MSQUIGE
  = do i <- newIdent (getLoc t) "i"
       eExists [i] <$> dsM_12 s (DefineIE x y t) (P (Variable i))

dsM_12 s (DefineIE x y t) (P i)             -- MSQUIGP
  = seqDE [ pure $ eDefine x i
          ,        eDefine y <$> dsM_12 s t (P i)
          ]

-------------------- array{t1,...tn} -----------------
dsM_12 s (Array ts) E                       -- MARRAYE
   = Array <$> mapM (\t -> dsM_12 s t E) ts

dsM_12 s (Array ts) (P i)                   -- MARRAYP
   | not (null ts)  -- Shortcut for empty ts, via MEQ
                    -- M_s[ <> ]P(i) --> i = <>
   = do { js <- mapM (\t -> newIdent (getLoc t) "j") ts
        ; es <- zipWithM do_one ts js
        ; pure (eExists js (seqE [ Unify i (Array (Variable <$> js))
                                 , Array es ])) }
   where
     do_one :: SrcExpr -> Ident -> D SrcExpr
     do_one t j = dsM_12 s t (P (Variable j))

-------------------- x := t -----------------
dsM_12 s (DefineE x t) pi                   -- MBIND
  = eDefine x <$> dsM_12 s t pi

-------------------- t1 = t2 -----------------
dsM_12 s (Unify t1 t2) pi                   -- MEQ
  = Unify <$> dsM_12 s t1 pi <*> dsM_12 s t2 pi

-------------------- t1 ; t2 -----------------
dsM_12 MX (Seq ts) pi                      -- MSEMIX
  = do let (ts', t) = unSeq ts
       es' <- mapM (dsDD_12 MX) ts'
       e'  <- dsM_12 MX t pi
       pure $ seqE (es' ++ [e'])

dsM_12 s  (Seq ts) pi                      -- MSEMI
  = do let (ts', t) = unSeq ts
       es' <- mapM (dsDD_12 MV) ts'
       e'  <- dsM_12 s t pi
       pure $ seqE (es' ++ [e'])

-------------------- (t1 | t2) and fail -----------------
dsM_12 s (Choice t1 t2) pi                 -- MCHOICE
  = Choice <$> dsM_12 s t1 pi <*> dsM_12 s t2 pi

dsM_12 _ Fail _
   = pure Fail

-------------------- k, op, x -----------------
dsM_12 _ t@(Lit{}) E                       -- MCONST
   = pure t

dsM_12 _ t@(EPrim{}) E                     -- MPrim
   = pure t

dsM_12 _ t@(Variable {}) E                 -- MVAR
   = pure t

-------------------- t1[t2] -----------------
dsM_12 s (ApplyD t1 t2) E                  -- MVAR
   = eApplyD <$> dsDD_12 s t1 <*> dsDD_12 s t2

dsM_12 s (If3 t1 t2 t3) pi                 -- MIF
  = If3 <$> dsDD_12 s t1 <*> dsM_12 s t2 pi <*> dsM_12 s t3 pi

dsM_12 s (Macro1 m rs t) pi
   = Macro1 m rs <$> dsM_12 s t pi

dsM_12 s (Lam x t) _pi
   = Lam x <$> dsM_12 s t E

dsM_12 _ e@(DefineV _) _
   = pure e

---------- Other terms with P(i) ---------------
dsM_12 s t (P i)                           -- MEQ
   = Unify i <$> dsM_12 s t E


dsM_12 s t pi
   = error $ "TODO: dsM_12 " ++ show (s, t, pi)


