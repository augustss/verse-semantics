{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DeriveDataTypeable #-}
module TRS.Bind where
import Data.Data(Data)
import Data.List( union, (\\) )
import Data.Char( isDigit )

--------------------------------------------------------------------------------

data Ident
  = Name String
  | Prim Int
 deriving ( Eq, Ord, Data )

instance Show Ident where
  show (Name x) = x
  show (Prim n) = "$" ++ show n

ident :: String -> Ident
ident = Name

prim :: Int -> Ident
prim = Prim

identsNotIn :: [Ident] -> [Ident]
identsNotIn zs = [ Name ("v" ++ show (m+i)) | i <- [1..] ]
 where m = maximum (0 : [ read s :: Integer | Name ('v':s) <- zs, not (null s), all isDigit s ])
{-
identsNotIn zs = [ Prim (m+i) | i <- [1..] ]
 where m = maximum (0 : [ n | Prim n <- zs ])
-}

identNotIn :: [Ident] -> Ident
identNotIn = head . identsNotIn

--------------------------------------------------------------------------------

class Free a where
  free :: a -> [Ident]

instance Free () where
  free _ = []

instance (Free a, Free b) => Free (a,b) where
  free (a,b) = free a `union` free b

instance (Free a, Free b, Free c) => Free (a,b,c) where
  free (a,b,c) = free a `union` free b `union` free c

instance Free a => Free [a] where
  free = foldr union [] . map free

-- This simplifies some code
instance Free Ident where
  free i = [i]

--------------------------------------------------------------------------------

data Bind t = Bind Ident t
 deriving ( Eq, Ord, Show, Data )

instance Free t => Free (Bind t) where
  free (Bind x t) = free t \\ [x]

--------------------------------------------------------------------------------

type Subst a = [(Ident,a)]

substBind :: (Free s, Free t)
          => (Ident->s) -> (Subst s -> t -> t) -> (Subst s -> Bind t -> Bind t)
substBind var subst sub a@(Bind x t)
  | null sub'   = a
  | x `elem` vs = Bind x' (subst ((x,var x'):sub') t)
  | otherwise   = Bind x (subst sub' t)
 where
  sub' = [ (y,t) | (y,t) <- sub, y /= x ]
  vs   = free (map snd sub')
  zs   = map fst sub' ++ vs ++ free t
  x'   = identNotIn zs

--------------------------------------------------------------------------------

class Binding t where
  binders :: t -> [Bind t]

bind :: Binding t => (Ident -> t) -> Bind t
bind = bindWith []

bindWith :: Binding t => [Ident] -> (Ident -> t) -> Bind t
bindWith vs f = Bind x t
 where
  t  = f x
  ys = vs ++ [ y | Bind y _ <- binders t ]
  x  = identNotIn ys

--------------------------------------------------------------------------------
