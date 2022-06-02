module Bind where

import Data.Char( isDigit )
import Data.List( union, (\\) )

--------------------------------------------------------------------------------

type Name = String

--------------------------------------------------------------------------------

class Free a where
  free :: a -> [Name]

instance Free () where
  free _ = []

instance (Free a, Free b) => Free (a,b) where
  free (a,b) = free a `union` free b

instance Free a => Free [a] where
  free = foldr union [] . map free

--------------------------------------------------------------------------------

data Bind t = Bind Name t
 deriving ( Eq, Ord, Show )

instance Free t => Free (Bind t) where
  free (Bind x t) = free t \\ [x]

substBind :: Free t => (Name->t) -> ([(Name,t)] -> t -> t) -> ([(Name,t)] -> Bind t -> Bind t)
substBind var subst sub a@(Bind x t)
  | null sub'   = a
  | x `elem` vs = Bind x' (subst ((x,var x'):sub') t)
  | otherwise   = Bind x (subst sub' t) 
 where
  sub' = [ (y,t) | (y,t) <- sub, y /= x ]
  vs   = free (map snd sub') 
  zs   = map fst sub' ++ vs ++ free t
  x'   = varNotIn zs

varNotIn :: [Name] -> Name
varNotIn zs = "$" ++ show (maximum (0 : [ read n | '$':n@(_:_) <- zs, all isDigit n ]) + 1)

--------------------------------------------------------------------------------

class Binding t where
  binders :: t -> [Bind t]

bind :: Binding t => (Name -> t) -> Bind t
bind = bindWith []

bindWith :: Binding t => [Name] -> (Name -> t) -> Bind t
bindWith vs f = Bind x t
 where
  t  = f x
  ys = vs ++ [ y | Bind y _ <- binders t ]
  x  = varNotIn ys

--------------------------------------------------------------------------------

