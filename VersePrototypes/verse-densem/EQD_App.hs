module Main where

import Prelude hiding ( pi )
import EQD
import qualified Data.Set as S
import Data.List( intercalate, (\\) )

---------------------------------------------------------------------------
-- Var

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

fresh :: [String] -> [Var] -> [Var]
fresh templ forb = go templ bad
 where
  bad = S.fromList forb

  names x = Idf x : [ Idf (x ++ show i) | i <- [1..] ]

  go [] bad = go ["$"] bad
  go (x:xs) bad = v : go xs (S.insert v bad)
   where
    v:_ = filter (not . (`S.member` bad)) (names x)

---------------------------------------------------------------------------
-- Val

data Val
  = Int Int
  | Fun FUN1
 deriving ( Eq, Ord )

instance Show Val where
  show (Int n) = show n
  show (Fun f) = showFUN f

---------------------------------------------------------------------------
-- ENV

type ENV = EQD Var Val

---------------------------------------------------------------------------
-- FUN/REL

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
  | ijs <- base [0..2] [0..3]
  , let fun = orl [ xx =: Int i /\ yy =: Int j | (i,j) <- ijs ]
        ext = andl [ nt (xx =: Int i) | (i,_) <- ijs ]
  ]
 where
  base []     js = [ [] ]
  base (i:is) js = [ (i,j):ijs | j <- js, ijs <- base is js ]

xx, yy, zz, uu :: Var
xx = Idf "𝑥"
yy = Idf "𝑦"
zz = Idf "𝑧"
uu = Idf "𝑢"

xxs :: [Var]
xxs = xx : yy : zz : uu : [ Idf (show xx ++ show i) | i <- [1..] ]

infix 5 @@

(@@) :: FUN -> [Var] -> ENV
fun @@ xs = foldr (\(x,y) p -> subst x (Var y) p) fun sub
 where
  vxs = xxs `zip` xs
  ws  = [ v | (v,_) <- vxs, v `elem` map snd vxs ]
  wzs = ws `zip` fresh [] (xs ++ support fun)

  sub = [ (z,w)
        | (w,z) <- wzs
        ]
        ++
        [ (v, head $ [ z | (w,z) <- wzs, w == x ] ++ [ x ])
        | (v,x) <- vxs
        ]

showFUN :: FUN -> String
showFUN p
  | showp == show (false :: FUN) = "{}"
  | otherwise           = "{" ++ showTuple xs
                              ++ "|"
                              ++ showCompr showp
                              ++ "}"
 where
  showp = show p

  vs = [ x | x <- support p, isSpecial (head (show x)) ]
  isSpecial c = c `elem` concat (map (take 1 . show) (take 10 xxs))

  xs = case vs of
         [] -> []
         _  -> takeXs vs xxs

  takeXs [] _      = []
  takeXs vs (y:ys) = y : takeXs (vs \\ [y]) ys

  showTuple [x] = show x
  showTuple xs  = "(" ++ intercalate "," (map show xs) ++ ")"

  showex = show ((xx =~ yy) :: FUN)
  bopen  = head showex
  bclose = last showex

  showCompr (c:s) | c == bopen = showCompr' s
  showCompr s                  = s

  showCompr' (c:s) | c == bclose = showCompr'' s
  showCompr' (c:s)               = c : showCompr' s
  showCompr' s                   = s -- shouldn't happen

  showCompr'' (_:c:s) | c == bopen = "∨" ++ showCompr' s
  showCompr'' s                    = s
  
---------------------------------------------------------------------------
-- pi

{-
pi :: Set a -> (a -> Set b) -> Set (a->b)
pi A fB = { f∈A->B | All x∈A. f(x)∈fB(x) } where B = UNION{ fB(x) | x∈A }

pi :: Set Val -> (Val -> Set Val) -> Set FUN
-}

type Set_Val = FUN0

type Val_to_Set_Val = FUN1

type Set_FUN = FUN0

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

---------------------------------------------------------------------------
-- example

main = print (pi dom range @@ [f])
 where
  dom   = orl [ xx =: Int i | i <- [0..2] ]
  range = plus@@[xx,a,yy]

plus :: REL
plus = orl
       [ xx =: Int i /\ yy =: Int j /\ zz =: Int (i+j)
       | i <- [0..3]
       , j <- [0..3]
       ]

---------------------------------------------------------------------------
