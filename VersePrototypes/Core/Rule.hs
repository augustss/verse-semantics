{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Core.Rule
  ( Rule
  , run
  , lhs
  , label
  , interest
  , skolems
  , assumps
  -- , fresh  -- fresh commented out for now because mixing identNotIn and fresh
              -- is dangerous, and starting to use "fresh" needs to be accompanied
              -- by some kind of invariant that all idents introduced by fresh
              -- will not clash with other ways of creating idents
  , empty
  , only
  , permute
  , choices
  , normalize
  , NormResult(..)
  , showNormResult
  , lotsOfSteps
  , everywhere
  )
 where

import Control.Applicative
import Data.List( (\\) )
import Data.Set( Set )
import qualified Data.Set as S

import Core.Expr
import Core.Bind
import Core.Traced

-----------------------------------------------------------------------------
-- Rule type

newtype Rule a
  = Rule ( Expr                   -- Reader
        -> [SkolIdent]            -- Reader
        -> [Assump]               -- Reader
        -> Set Ident              -- State
        -> [( a
            , String              -- Writer (with (++))
            , Int                 -- Writer (with min)
            , Set Ident           -- State
            )]
         )

run :: Rule a -> Set Ident -> [SkolIdent] -> [Assump] -> Expr -> [(a,String,Int)]
run (Rule m) idfs sks ass e =
  [ (a, s, v)
  | (a, s, v, _) <- m e sks ass idfs
  ]

-----------------------------------------------------------------------------
-- Rule is a Functor, Applicative, Alternative, Monad

instance Functor Rule where
  fmap f (Rule m) =
    Rule (\e sks ass idfs ->
      [ (f a, s, v, idfs')
      | (a, s, v, idfs') <- m e sks ass idfs
      ]
    )

instance Applicative Rule where
  pure x =
    Rule (\_ _ _ idfs -> [(x,"",1,idfs)])

  Rule mf <*> Rule ma =
    Rule (\e sks ass idfs ->
      [ (f a, s1 ++ s2, v1 `min` v2, idfs2)
      | (f,s1,v1,idfs1) <- mf e sks ass idfs
      , (a,s2,v2,idfs2) <- ma e sks ass idfs1
      ]
    )

instance Alternative Rule where
  empty =
    Rule (\_ _ _ _ -> [])

  Rule m1 <|> Rule m2 =
    Rule (\e sks ass idfs ->
      m1 e sks ass idfs ++ m2 e sks ass idfs
    )

instance Monad Rule where
  Rule m1 >>= k =
    Rule (\e sks ass idfs ->
      [ (b, s1 ++ s2, v1 `min` v2, idfs2)
      | (a,s1,v1,idfs1) <- m1 e sks ass idfs
      , let Rule m2 = k a
      , (b,s2,v2,idfs2) <- m2 e sks ass idfs1
      ]
    )

instance MonadFail Rule where
  fail _ = empty

-----------------------------------------------------------------------------

lhs :: Rule Expr
lhs = Rule (\e _ _ idfs -> [(e, "", 1, idfs)])

withLhs :: Expr -> Rule a -> Rule a
withLhs e (Rule m) = Rule (\_ sks ass idfs -> m e sks ass idfs)

label :: String -> Rule ()
label s = Rule (\_ _ _ idfs -> [((), s, 1, idfs)])

interest :: Int -> Rule ()
interest v = Rule (\_ _ _ idfs -> [((), "", v, idfs)])

skolems :: Rule [SkolIdent]
skolems = Rule (\_ sks _ idfs -> [(sks, "", 1, idfs)])

withSkolems :: [SkolIdent] -> Rule a -> Rule a
withSkolems sks (Rule m) = Rule (\e _ ass idfs -> m e sks ass idfs)

assumps :: Rule [Assump]
assumps = Rule (\_ _ ass idfs -> [(ass, "", 1, idfs)])

withAssumps :: [Assump] -> Rule a -> Rule a
withAssumps ass (Rule m) = Rule (\e sks _ idfs -> m e sks ass idfs)

_fresh :: String -> Rule Ident
_fresh pref = Rule (\_ _ _ idfs ->
  let x:_ = identsNotInPrefix pref (S.toList idfs) in
    [(x, "", 1, S.insert x idfs)]
  )

only :: (String->Bool) -> Rule a -> Rule a
only p (Rule m) =
  Rule (\e sks ass idfs ->
    [ (a, s, v, idfs')
    | (a, s, v, idfs') <- m e sks ass idfs
    , p s
    ]
  )

permute :: (Expr -> [(a,String,Int,Set Ident)] -> [(a,String,Int,Set Ident)])
        -> Rule a -> Rule a
permute f (Rule m) = Rule (\e sks ass idfs -> f e (m e sks ass idfs))

-----------------------------------------------------------------------------
-- auxiliary functions

choices :: [Rule a] -> Rule a
choices = foldr (<|>) empty

-----------------------------------------------------------------------------
-- rewriting

-- apply the given rule at every immediate recursive occurrence of LHS
dive :: Rule Expr -> Rule Expr
dive r =
  do e <- lhs
     recurse e
 where
  -- apply the rule r with e' as the LHS
  f e' = withLhs e' r

  -- basic recursive cases
  recurse (e1 :=: e2)   = fmap (:=: e2)  (f e1) <|> fmap (e1 :=:)  (f e2)
  recurse (e1 :>: e2)   = fmap (:>: e2)  (f e1) <|> fmap (e1 :>:)  (f e2)
  recurse (e1 :|: e2)   = fmap (:|: e2)  (f e1) <|> fmap (e1 :|:)  (f e2)
  recurse (e1 :@: e2)   = fmap (:@: e2)  (f e1) <|> fmap (e1 :@:)  (f e2)
  recurse (e1 :>>: e2)  = fmap (:>>: e2) (f e1) <|> fmap (e1 :>>:) (f e2)
  recurse (Some e)      = fmap Some (f e)
  recurse (Tru e)       = fmap Tru (f e)
  recurse (Iter i e e0) = fmap (\e' -> Iter i e' e0) (f e) <|> fmap (Iter i e) (f e0)

  -- recurse on Tup: try every element and rebuild the tuple
  recurse (Tup es) =
    choices [ fmap (\e' -> Tup (take i es ++ [e'] ++ drop (i+1) es)) (f e)
            | (i,e) <- [0..] `zip` es
            ]

  -- recurse on Exi: deal with quantifier by removing x from skolems
  recurse (Exi bnd) = 
    do sks <- skolems
       let (x,e) = unsafeUnbind bnd
       withSkolems (sks \\ [x]) $
         fmap (Exi . bind x) (f e)

  -- recurse on Verify: deal with skolems and assumps
  recurse (Verify bnds) =
    do sks  <- skolems
       asms <- assumps
       let (rs,(as,e)) = alphaRenameVerify sks bnds
       withSkolems (sks ++ rs) $
         withAssumps (asms ++ as) $
           fmap (\e' -> Verify (bindList rs (as,e'))) (f e)

  -- do nothing
  recurse (Lam _) = empty -- do not rewrite under lambdas
  recurse _       = empty

-- apply a rule everywhere (recursively) in the LHS expression
everywhere :: Rule Expr -> Rule Expr
everywhere r = r <|> dive (everywhere r)

-----------------------------------------------------------------------------
-- normalizing

type Fuel = Int

lotsOfSteps :: Fuel
lotsOfSteps = 1000

data NormResult
  = NormOK        -- No rewrites apply
  | NormExpired   -- We ran out of fuel
  | NormInvalid   -- A rewrite produced an invalid output
                  -- according to the `valid` predicate
  deriving( Eq )

instance Show NormResult where
   show = showNormResult

showNormResult :: NormResult -> String
showNormResult NormOK      = "reached a normal form"
showNormResult NormExpired = "ran out of fuel (Unexpected)"
showNormResult NormInvalid = "reached an invalid expression -- yikes!"

normalize :: Fuel    -- Maximum number of steps
          -> Rule Expr -> Expr
          -> (NormResult, Traced Expr)
-- Repeatedly apply the first in the
-- list of possiblities returned by the rule
normalize fuel rule orig_e = go fuel [] orig_e
 where
  go fuel_left tr e =
    case run rule (S.fromList (occurs e)) [] [] e of
      []                        -> (NormOK,      e  :<-- tr)
      (e',lab,v) : _   -- Pick the first offered rewrite
        | fuel_left==0   -> (NormExpired, e  :<-- tr)
        | not (valid e') -> (NormInvalid, e' :<-- tr')
        | otherwise      -> -- ppTrace "norm" (int fuel <+> text s <> braces (pPrintBrief e')) $
                            go (fuel_left-1) tr' e'
        where
          tr' = TS { ts_str = lab, ts_verb = v, ts_payload = e' }
                `setTsPayload` e : tr

-----------------------------------------------------------------------------
