module Core.Rule
  ( Rule
  , run
  , lhs
  , label
  , interest
  , assumps
  , fresh
  , only
  )
 where

import Control.Applicative
import Core.Expr hiding ( Rule, RuleEnv(..) )
import Core.Bind( identsNotInPrefix )
import Data.Set( Set )
import qualified Data.Set as S

-----------------------------------------------------------------------------
-- Rule type

newtype Rule a
  = Rule ( Expr               -- Reader
        -> ([Ident],[Assump]) -- Reader
        -> Set Ident          -- State
        -> [( a
            , String          -- Writer (with (++))
            , Int             -- Writer (with max)
            , Set Ident       -- State
            )]
         )

run :: Rule a -> Set Ident -> ([Ident],[Assump]) -> Expr -> [(a,String,Int)]
run (Rule m) idfs env e =
  [ (a, s, v)
  | (a, s, v, _) <- m e env idfs
  ]

-----------------------------------------------------------------------------
-- Rule is a Functor, Applicative, Alternative, Monad

instance Functor Rule where
  fmap f (Rule m) =
    Rule (\e env idfs ->
      [ (f a, s, v, idfs')
      | (a, s, v, idfs') <- m e env idfs
      ]
    )

instance Applicative Rule where
  pure x =
    Rule (\_ _ idfs -> [(x,"",1,idfs)])

  Rule mf <*> Rule ma =
    Rule (\e env idfs ->
      [ (f a, s1 ++ s2, v1 `min` v2, idfs2)
      | (f,s1,v1,idfs1) <- mf e env idfs
      , (a,s2,v2,idfs2) <- ma e env idfs1
      ]
    )

instance Alternative Rule where
  empty =
    Rule (\_ _ _ -> [])

  Rule m1 <|> Rule m2 =
    Rule (\e env idfs ->
      m1 e env idfs ++ m2 e env idfs
    )

instance Monad Rule where
  Rule m1 >>= k =
    Rule (\e env idfs ->
      [ (b, s1 ++ s2, v1 `min` v2, idfs2)
      | (a,s1,v1,idfs1) <- m1 e env idfs
      , let Rule m2 = k a
      , (b,s2,v2,idfs2) <- m2 e env idfs1
      ]
    )

instance MonadFail Rule where
  fail _ = empty

-----------------------------------------------------------------------------

lhs :: Rule Expr
lhs = Rule (\e _ idfs -> [(e, "", 1, idfs)])

label :: String -> Rule ()
label s = Rule (\_ _ idfs -> [((), s, 1, idfs)])

interest :: Int -> Rule ()
interest v = Rule (\_ _ idfs -> [((), "", v, idfs)])

assumps :: Rule ([Ident],[Assump])
assumps = Rule (\_ env idfs -> [(env, "", 1, idfs)])

fresh :: String -> Rule Ident
fresh pref = Rule (\_ _ idfs ->
  let x:_ = identsNotInPrefix pref (S.toList idfs) in
    [(x, "", 1, S.insert x idfs)]
  )

only :: (String->Bool) -> Rule a -> Rule a
only p (Rule m) =
  Rule (\e env idfs ->
    [ (a, s, v, idfs')
    | (a, s, v, idfs') <- m e env idfs
    , p s
    ]
  )

-----------------------------------------------------------------------------
