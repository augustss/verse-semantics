module EQD where

import qualified Data.Set as S
import Data.List( intercalate, nub )
import Test.QuickCheck

-----------------------------------------------------------------------

data Atom x a
  = Val a
  | Var x
 deriving ( Eq, Ord ) -- Val should be smaller than Var

instance (Show x, Show a) => Show (Atom x a) where
  show (Val a) = show a
  show (Var x) = show x

-- only for local use
data Var = Idf String deriving ( Eq, Ord )
instance Show Var where show (Idf x) = x
data Val = Int Int deriving ( Eq, Ord )
instance Show Val where show (Int n) = show n
instance Num Val where fromInteger n = Int (fromInteger n); (+)=undefined; (*)=undefined; abs=undefined; signum=undefined; negate=undefined
x,y,z :: Atom Var a
x = Var (Idf "x"); y = Var (Idf "y"); z = Var (Idf "z")

-----------------------------------------------------------------------

data EQD x a
  = TRUE
  | FALSE
  | IF x (Atom x a) (EQD x a) (EQD x a)
 deriving ( Eq, Ord, Show )

invariant :: (Ord x, Ord a) => EQD x a -> Bool
invariant t = check S.empty t
 where
  check _    TRUE            = True
  check _    FALSE           = True  
  check forb (IF x a yes no) =
       not (x `S.member` forb)
    && Var x > a
    && (x,a) <. yes
    && (x,a) <. no
    && check (S.insert x forb) yes
    && check forb              no

  xa <. IF y b _ _ = xa < (y,b)
  _  <. _          = True

{-
instance (Ord x, Show x, Ord a, Show a) => Show (EQD x a) where
  show t = case cubes t of
             [] -> "fail"
             cs -> intercalate "U" [ showCube c | c <- cs ]
   where
    cubes TRUE            = [[]]
    cubes FALSE           = []
    cubes (IF x a yes no) = [ (x,a,True):c  | c <- cubes yes ]
                         ++ [ (x,a,False):c | c <- cubes no ]

    showCube c = "{{" ++ intercalate "," (pos c ++ neg c) ++ "}}"
     where
      pos c = [ intercalate "=" ( [ show l | (l,r',True) <- c, r'==r ]
                               ++ [show r]
                                )
              | r <- nub [ r | (_,r,True) <- c ]
              ]
      neg c = [ show l ++ "≠" ++ show r
              | (l,r,False) <- c
              ]
-}

-----------------------------------------------------------------------
-- raw operations, do not expose to the user
-- is supposed to maintain the invariant

-- assumes Var x > a
mkIF :: (Ord x, Ord a) => x -> Atom x a -> EQD x a -> EQD x a -> EQD x a
mkIF x a yes no = mkIFxa (simp x a yes) no
 where
   mkIFxa yes@(~(IF y b yes1 no1)) no@(~(IF z c yes2 no2))
     | yes==no      = yes
     | (x,a) =? no  = mkIFxa yes no2
     -- TODO optimization: choose the smallest one of the following two cases
     | (x,a) >? yes = mkIF y b (mkIFxa yes1 no) (mkIFxa no1 no)
     | (x,a) >? no  = mkIF z c (mkIFxa yes yes2) (mkIFxa yes no2)
     | otherwise    = IF x a yes no

   xa >? IF y b _ _ = xa > (y,b)
   _  >? _          = False

   xa =? IF y b _ _ = xa == (y,b)
   _  =? _          = False

   simp x a TRUE  = TRUE
   simp x a FALSE = FALSE
   simp x a (IF y b yes no)
     | y==x       = iff a b yes' no'
     | b==Var x   = iff (Var y) a yes' no'
     | otherwise  = mkIF y b yes' no'
    where
     yes' = simp x a yes
     no'  = simp x a no

-----------------------------------------------------------------------
-- recursively derived logical operators

(/\) :: (Ord x, Ord a) => EQD x a -> EQD x a -> EQD x a
FALSE         /\ _     = FALSE
_             /\ FALSE = FALSE
TRUE          /\ t     = t
t             /\ TRUE  = t
IF x a yes no /\ t     = mkIF x a (yes /\ t) (no /\ t)
  -- this last case looks really inefficient!
  -- especially if you think about regular BDDs
  -- but: it's simple and therefore correct
  -- also: mkIF has to go up and down the tree several times anyway
  -- let's not worry about it until it becomes an issue

qall :: (Ord x, Ord a) => x -> EQD x a -> EQD x a
qall x TRUE            = TRUE
qall x FALSE           = FALSE
qall x (IF y a yes no)
  | y==x || a==Var x   = yes' /\ no'
  | otherwise          = mkIF y a yes' no'
 where
  yes' = qall x yes
  no'  = qall x no

nt :: EQD x a -> EQD x a
nt FALSE           = TRUE
nt TRUE            = FALSE
nt (IF x a yes no) = IF x a (nt yes) (nt no)

-----------------------------------------------------------------------
-- simply derived logical operators

iff :: (Ord x, Ord a) => Atom x a -> Atom x a -> EQD x a -> EQD x a -> EQD x a
iff a b       yes no | a==b      = yes
iff a (Var x) yes no | Var x > a = mkIF x a yes no
iff (Var x) a yes no             = mkIF x a yes no
iff _ _       yes no             = no -- only Val a/=Val b left

(.=.) :: (Ord x, Ord a) => Atom x a -> Atom x a -> EQD x a
a .=. b = iff a b TRUE FALSE

(\/), (==>) :: (Ord x, Ord a) => EQD x a -> EQD x a -> EQD x a
s \/  t = nt (nt s /\ nt t)
s ==> t = nt (s /\ nt t)

qexi :: (Ord x, Ord a) => x -> EQD x a -> EQD x a
qexi x t = nt (qall x (nt t))

  -- an alternative design choice would have been:
  -- 1. a general operator "apply" that can implement /\, \/, =>, etc.
  -- 2. a general operator "quant" that can implement qall, qexi, etc.
  -- but this is simpler and therefore correct

-----------------------------------------------------------------------
