{-# LANGUAGE OverloadedStrings #-}
module Bind where
import Data.List
import Data.String

data Ident
  = Name String
 deriving ( Show, Eq, Ord )

ident :: String -> Ident
ident = Name

identNotIn :: [Ident] -> Ident
identNotIn is = head $ ids \\ is
  where ids = [ Name $ "v" ++ show i | i <- [1::Integer ..] ]

notIn :: (Free a) => a -> Ident
notIn e = identNotIn (allIds e)

instance IsString Ident where
  fromString = Name

--------------------------------------------------------------------------------

class Free a where
  free   :: a -> [Ident]
  allIds :: a -> [Ident]

instance Free () where
  free   _ = []
  allIds _ = []

instance (Free a, Free b) => Free (a,b) where
  free   (a,b) = free a `union` free b
  allIds (a,b) = allIds a ++      allIds b

instance (Free a, Free b, Free c) => Free (a,b,c) where
  free   (a,b,c) = free a `union` free b `union` free c
  allIds (a,b,c) = allIds a ++    allIds b ++    allIds c

instance (Free a, Free b, Free c, Free d) => Free (a,b,c,d) where
  free   (a,b,c,d) = free a `union` free b `union` free c `union` free d
  allIds (a,b,c,d) = allIds a ++    allIds b ++    allIds c ++    allIds d

instance Free a => Free [a] where
  free   = foldr union [] . map free
  allIds = foldr (++)  [] . map allIds

--------------------------------------------------------------------------------

data Bind t = Bind Ident t
 deriving ( Eq, Ord, Show )

instance Free t => Free (Bind t) where
  free (Bind x t) = free t \\ [x]
  allIds (Bind x t) = x : allIds t

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

