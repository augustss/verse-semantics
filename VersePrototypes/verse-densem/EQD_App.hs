module Main where

import Prelude hiding ( pi )
import EQD
import qualified Data.Set as S
import Data.List( intercalate, (\\) )

data Var = Idf String deriving ( Eq, Ord )
instance Show Var where show (Idf x) = x

a,b,c,x,y,z,u,v,w :: Var
a = Idf "a"
b = Idf "b"
c = Idf "c"
x = Idf "x"
y = Idf "y"
z = Idf "z"
u = Idf "u"
v = Idf "v"
w = Idf "w"
f = Idf "f"

type ENV = EQD Var Val

-- FUN

type REL  = ENV
type FUN  = ENV
type FUN0 = FUN -- only has 𝓍
type FUN1 = FUN -- only has 𝓍,𝓎
type FUN2 = FUN -- only has 𝓍,𝓎,𝓎1
type FUN3 = FUN -- only has 𝓍,𝓎,𝓎1,𝓎2

allFUN1s :: [FUN]
allFUN1s =
  concat
  [ [ fun
    , fun \/ (ext /\ (yy =~ xx))
    , fun \/ (ext /\ (yy =: Int 0))
    , fun \/ (xx =: Int 4 /\ yy =: Int 5)
    ]
  | ijs <- base [0..1] [0..2]
  , let fun = orl [ xx =: Int i /\ yy =: Int j | (i,j) <- ijs ]
        ext = andl [ nt (xx =: Int i) | (i,_) <- ijs ]
  ]
 where
  base []     js = [ [] ]
  base (i:is) js = [ (i,j):ijs | j <- js, ijs <- base is js ]

xx, yy, yy1, yy2 :: Var
xx  = Idf "𝓍"
yy  = yys !! 0
yy1 = yys !! 1
yy2 = yys !! 2

yys :: [Var]
yys = [ Idf ("𝓎" ++ l) | l <- "" : map show [1..] ]

infix 5 @@

(@@) :: FUN -> [Var] -> ENV
fun @@ xs = foldr (\(x,y) p -> subst y (Var x) p) fun (xs `zip` (xx : yys))

data Val
  = Int Int
  | Fun FUN1
 deriving ( Eq, Ord )

instance Show Val where
  show (Int n) = show n
  show (Fun f) = showFUN f

showFUN p
  | showp == "fail" = "{}"
  | otherwise       = "{" ++ showTuple xs
                          ++ "|"
                          ++ showCompr showp
                          ++ "}"
 where
  showp = show p

  vs = [ x | x <- support p, isSpecial (head (show x)) ]
  isSpecial c = c `elem` concat (map (take 1 . show) (take 5 (xx : yys)))

  xs = case vs of
         [] -> []
         _  -> takeXs vs (xx:yys)

  takeXs [] _      = []
  takeXs vs (y:ys) = y : takeXs (vs \\ [y]) ys

  showTuple [x] = show x
  showTuple xs  = "(" ++ intercalate "," (map show xs) ++ ")"

  showCompr ('{':'{':s) = showCompr' s
  showCompr s           = s

  showCompr' ('}':'}':s) = showCompr'' s
  showCompr' (c:s)       = c : showCompr' s
  showCompr' s           = s -- shouldn't happen

  showCompr'' ('U':'{':'{':s) = "\\/" ++ showCompr' s
  showCompr'' s               = s
  
{-
pi :: Set a -> (a -> Set b) -> Set (a->b)
pi A fB = { f∈A->B | All x∈A. f(x)∈fB(x) } where B = UNION{ fB(x) | x∈A }

pi :: Set Val -> (Val -> Set Val) -> Set FUN
-}

type Set_Val = FUN0

type Val_to_Set_Val = FUN1

type Set_FUN = FUN0

{-
type M a

apply :: Atom -> Atom -> M Atom
apply f a =
  do b <- new
     require $
       orl [ qexi xx $ qexi yy $
               f .=. Val (Fun fun)
            /\ fun
            /\ Var xx .=. a
            /\ Var yy .=. b
           | fun <- allFUNs
           ]
     return b
-}


pi :: Set_Val -> Val_to_Set_Val -> Set_FUN
pi dom range =
  orl
  [ xx =: Fun fun
 /\ qall x (qall y (
      fun@@[x,y] ==> dom@@[x]
    ))
 /\ qall x (qexi y (
      dom@@[x] ==> (fun@@[x,y] /\ range@@[x,y])
    ))
  | fun <- allFUN1s
  ]
 where
  x:y:_ = fresh ["x","y"] (support dom ++ support range)

fresh :: [String] -> [Var] -> [Var]
fresh templ forb = go templ bad
 where
  bad = S.fromList forb

  names x = Idf x : [ Idf (x ++ show i) | i <- [1..] ]

  go [] bad = []
  go (x:xs) bad = v : go xs (S.insert v bad)
   where
    v:_ = filter (not . (`S.member` bad)) (names x)

main = print (pi dom range @@ [f])
 where
  dom   = orl [ xx =: Int i | i <- [0..1] ]
  range = orl
          [ xx =: Int i /\ a =: Int j /\ yy =: Int (i+j)
          | i <- [0..2]
          , j <- [0..2]
          ]
