module TRS.Bind
  ( Ident(..), SkolIdent
  , ident, underscore, isUnderscore
  , identsNotInPrefix, identsNotIn, identNotIn, skolNotIn, skolsNotIn

  , Variables(..)
  , free, occurs, intersects

  , Bind -- abstract! let's see if we can do this
  , bind, unsafeUnbind, alphaRenameBindWith
  , BindList, bindList, unsafeUnbindList, alphaRenameBindListWith

  , Subst, substBind, substBinds
  )
 where

import Data.List( union, isPrefixOf )
import Data.Char( isDigit )
import Epic.Print

--------------------------------------------------------------------------------

newtype Ident = Name String
 deriving ( Eq, Ord )

type SkolIdent = Ident   -- Skolem variables, R, in verify(R,A){e}

instance Show Ident where
  show (Name x) = x

instance Pretty Ident where
  pPrintPrec _ _ (Name x) = text x

ident :: String -> Ident
ident = Name

underscore :: Ident
-- `underscore` does not count as free or bound
-- We use it only on the LHS of (_ = e1; e2)
underscore = Name "_"

isUnderscore :: Ident -> Bool
isUnderscore x = x == underscore

identsNotInPrefix :: String -> [Ident] -> [Ident]
identsNotInPrefix prefix forb = [ Name (prefix ++ show (m+i)) | i <- [1..] ]
  where
    m :: Integer  -- m is the max k, such that prefix_k is in forb
    m = maximum (0 : [ read s :: Integer
                     | Name str <- forb
                     , prefix `isPrefixOf` str
                     , let s = drop (length prefix) str
                     , not (null s)
                     , all isDigit s ])

identsNotIn :: [Ident] -> [Ident]
-- Return an infinite list of identifiers not in `forb`
identsNotIn forb = filter (`notElem` forb) [ Name x | x <- xs ]
                   ++ identsNotInPrefix "x" forb
 where
  xs = ["x","y","z","u","v","w"]

skolNotIn :: [SkolIdent] -> SkolIdent
skolNotIn forb = head (skolsNotIn forb)

skolsNotIn :: [SkolIdent] -> [SkolIdent]
skolsNotIn forb = identsNotInPrefix "$r" forb

identNotIn :: [Ident] -> Ident
identNotIn = head . identsNotIn

--------------------------------------------------------------------------------

{- Note [Binders, uses, and occurrences]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In (\x. x), the first x is a /binder/
            the second x is a /use/
Both are /occurrences/
-}

-- A type t is an instances of (Variables t) if
-- t has occurrences of Ident, and/or bindings of Ident
class Variables a where
  variables :: (Ident -> [Ident] -> [Ident])  -- What to do at a binder
            -> a -> [Ident]

free, occurs :: Variables a => a -> [Ident]
-- See Note [Binders, uses, and occurrences
free   = variables (filter . (/=))   -- Finds all free variables
occurs = variables (union . (: []))  -- Finds all variables,
                                     -- both binders and uses


intersects :: [Ident] -> [Ident] -> Bool
-- True if the two lists have one or more common members
intersects xs ys = any (`elem` xs) ys

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

instance Variables Ident where
  variables _ x | isUnderscore x = []   -- Underscore is not a real variable
                | otherwise      = [x]

--------------------------------------------------------------------------------

-- Bind is abstract: data contructor not visible outside this module
data Bind t = Bind Ident t
 deriving ( Eq, Ord, Show )

bind :: Ident -> t -> Bind t
bind x t = Bind x t

instance Variables t => Variables (Bind t) where
  variables f (Bind x t) = f x (variables f t)

alphaRenameBindWith :: Variables t
                    => (Ident -> t -> (Ident, t))  -- Freshen
                    -> Bind t -> (Ident, t)
-- Recommended way to walk inside a Bind
--    (ren x y t) should replace all uses of `x` by `y` in `t`
alphaRenameBindWith freshen (Bind x t)
  = freshen x t

alphaRenameBindListWith :: Variables t
                        => ([Ident] -> t -> ([Ident], t))  -- Freshen
                        -> BindList t -> ([Ident], t)
-- Recommended way to walk inside a Bind
--    (ren x y t) should replace all uses of `x` by `y` in `t`
alphaRenameBindListWith freshen bl
  = freshen rs body
  where
    (rs, body) = unsafeUnbindList bl

unsafeUnbind :: Bind t -> (Ident, t)
-- Non-recommended way to walk inside a Bind
unsafeUnbind (Bind x t) = (x,t)

-- a list of binders

data BindList t = Body t | Binder (Bind (BindList t))
 deriving ( Eq, Ord, Show )

instance Variables t => Variables (BindList t) where
  variables f (Body t)     = variables f t
  variables f (Binder bnd) = variables f bnd

bindList :: [Ident] -> t -> BindList t
bindList xs t = foldr do_one (Body t) xs
  where
    do_one x bl = Binder (bind x bl)

unsafeUnbindList :: BindList t -> ([Ident], t)
unsafeUnbindList (Body t)     = ([], t)
unsafeUnbindList (Binder bnd) = let (x,bl)    = unsafeUnbind bnd
                                    (xs,body) = unsafeUnbindList bl
                                in (x:xs, body)

--------------------------------------------------------------------------------

type Subst a = [(Ident,a)]

substBind :: (Variables s, Variables t)
          => (Ident -> s) -> (Subst s -> t -> t)
          -> Subst s -> Bind t -> Bind t
substBind var subst sub a@(Bind x t)
  | null sub'   = a
  | x `elem` vs = Bind x' (subst ((x,var x'):sub') t)
  | otherwise   = Bind x  (subst sub' t)
 where
  sub' = [ (y,ty) | (y,ty) <- sub, y /= x ]
  vs   = free (map snd sub')
  zs   = map fst sub' ++ vs ++ free t
  x'   = identNotIn zs

substBinds :: (Variables s, Variables t)
           => (Ident -> s) -> (Subst s -> t -> t) -> (Subst s -> BindList t -> BindList t)
substBinds _var subst sub (Body t)     = Body (subst sub t)
substBinds var  subst sub (Binder bnd) = Binder (substBind var (substBinds var subst) sub bnd)

--------------------------------------------------------------------------------
