module TRS.Bind
  ( Ident(..)
  , ident
  , identsNotInPrefix
  , identsNotIn
  , identNotIn
  , Variables(..)
  , free
  , occurs
  , Bind -- abstract! let's see if we can do this
  , bind
  , unsafeUnbind
  , alphaRenameWith
  , Binds(..)
  , Subst
  , substBind
  , substBinds
  )
 where

import Data.List( union, (\\), isPrefixOf )
import Data.Char( isDigit )
import Data.Maybe (maybeToList)
import Epic.Print

--------------------------------------------------------------------------------

newtype Ident = Name String
 deriving ( Show, Eq, Ord )

instance Pretty Ident where
  pPrintPrec _ _ (Name x) = text x

ident :: String -> Ident
ident = Name

identsNotInPrefix :: String -> [Ident] -> [Ident]
identsNotInPrefix prefix zs = [ Name (prefix ++ show (m+i)) | i <- [1..] ]
  where m = maximum (0 : [ read s :: Integer
                         | Name str <- zs
                         , prefix `isPrefixOf` str
                         , let s = drop (length prefix) str
                         , not (null s)
                         , all isDigit s
                         ])

identsNotIn :: [Ident] -> [Ident]
identsNotIn zs = filter (`notElem` zs) [ Name x | x <- xs ] ++ identsNotInPrefix "x" zs
 where
  xs = ["x","y","z","u","v","w"]

identNotIn :: [Ident] -> Ident
identNotIn = head . identsNotIn

--------------------------------------------------------------------------------

class Variables a where
  variables :: (Ident -> [Ident] -> [Ident]) -> a -> [Ident]

free, occurs :: Variables a => a -> [Ident]
free   = variables (filter . (/=))  -- finds all free variables
occurs = variables (union . (:[]))  -- finds all variable occurrences

instance Variables () where
  variables _ _ = []

instance (Variables a, Variables b) => Variables (a,b) where
  variables f (a,b) = variables f a `union` variables f b

instance (Variables a, Variables b, Variables c) => Variables (a,b,c) where
  variables f (a,b,c) = variables f (a,(b,c))

instance (Variables a, Variables b, Variables c, Variables d) => Variables (a,b,c,d) where
  variables f (a,b,c,d) = variables f (a,(b,c,d))

instance Variables a => Variables [a] where
  variables f = foldr union [] . map (variables f)

-- This simplifies some code
instance Variables Ident where
  variables _ x = [x]

--------------------------------------------------------------------------------

data Bind t = Bind Ident t
 deriving ( Eq, Ord, Show )

bind :: Ident -> t -> Bind t
bind x t = Bind x t

unsafeUnbind :: Bind t -> (Ident, t)
unsafeUnbind (Bind x t) = (x,t)

instance Variables t => Variables (Bind t) where
  variables f (Bind x t) = f x (variables f t)

alphaRenameWith :: Variables t => (Ident -> Ident -> t -> t) -> [Ident] -> Bind t -> (Ident, t)
alphaRenameWith ren forb (Bind x t)
  | x `notElem` forb = (x, t)
  | otherwise        = (x', ren x x' t)
 where
  zs = forb ++ free t
  x' = identNotIn zs

-- a list of binders

data Binds t = Body t | Binder (Bind (Binds t))
 deriving ( Eq, Ord, Show )

instance Variables t => Variables (Binds t) where
  variables f (Body t)     = variables f t
  variables f (Binder bnd) = variables f bnd

--------------------------------------------------------------------------------

type Subst a = [(Ident,a)]

substBind :: (Variables s, Variables t)
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

substBinds :: (Variables s, Variables t)
           => (Ident -> s) -> (Subst s -> t -> t) -> (Subst s -> Binds t -> Binds t)
substBinds var subst sub (Body t)     = Body (subst sub t)
substBinds var subst sub (Binder bnd) = Binder (substBind var (substBinds var subst) sub bnd)

--------------------------------------------------------------------------------
