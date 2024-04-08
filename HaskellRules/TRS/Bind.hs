{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DeriveDataTypeable #-}
module TRS.Bind where
import Data.Data(Data)
import Data.List( union, (\\), isPrefixOf )
import Data.Char( isDigit )
import Data.Maybe (maybeToList)
import Epic.Print

--------------------------------------------------------------------------------

data Ident
  = Name String
  | Prim Int
 deriving ( Show, Eq, Ord, Data )

instance Pretty Ident where
  pPrintPrec _ _ (Name x) = text x
  pPrintPrec _ _ (Prim n) = text ("$" ++ show n)

ident :: String -> Ident
ident = Name

prim :: Int -> Ident
prim = Prim

identsNotInPrefix :: String -> [Ident] -> [Ident]
identsNotInPrefix prefix zs = [ Name (prefix ++ show (m+i)) | i <- [1..] ]
  where m = maximum (0 : [ read s :: Integer
                         | Name str <- zs
                         , s <- maybeToList (removePrefix prefix str)
                         , not (null s)
                         , all isDigit s
                         ])

removePrefix :: String -> String -> Maybe String
removePrefix p x = if p `isPrefixOf` x then Just (drop (length p) x) else Nothing

identsNotIn :: [Ident] -> [Ident]
identsNotIn zs = filter (`notElem` zs) [ Name x | x <- xs ] ++ identsNotInPrefix "v" zs
 where
  xs = ["x","y","z","u","v","w"]

identNotIn :: [Ident] -> Ident
identNotIn = head . identsNotIn

uvIdentNotIn :: [Ident] -> Ident
uvIdentNotIn = head . uvIdentsNotIn

uvIdentsNotIn :: [Ident] -> [Ident]
uvIdentsNotIn = identsNotInPrefix uvPrefix


uvPrefix :: String
uvPrefix = "uni$"

isUV :: Ident -> Bool
isUV (Name x) = uvPrefix `isPrefixOf` x
isUV _ = False
--------------------------------------------------------------------------------

class Free a where
  free :: a -> [Ident]

instance Free () where
  free _ = []

instance (Free a, Free b) => Free (a,b) where
  free (a,b) = free a `union` free b

instance (Free a, Free b, Free c) => Free (a,b,c) where
  free (a,b,c) = free a `union` free b `union` free c

instance (Free a, Free b, Free c, Free d) => Free (a,b,c,d) where
  free (a,b,c,d) = free a `union` free b `union` free c `union` free d

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
          => (Ident -> s) -> (Subst s -> t -> t) -> (Subst s -> Bind t -> Bind t)
substBind var subst sub a@(Bind x t)
  | null sub'   = a
  | x `elem` vs = Bind x' (subst ((x,var x'):sub') t)
  | otherwise   = Bind x  (subst sub' t)
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
