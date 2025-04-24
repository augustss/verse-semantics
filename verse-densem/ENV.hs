{-# OPTIONS_GHC -Wno-x-partial #-}
module ENV(
  Ident(..),
  univ,
  fcnAdd, fcnLE, fcnInt,
  ENV,
    univE, failE,
    hide, compl,
    (%=), (%/\), (%\/), (%\\),
    vals,
  bigUnion,
  bigIntersect,
  clean,
  ) where

import Data.List( sort, group, intercalate )
import Data.Maybe(fromJust)
--import qualified Data.Map as M

import Dom

----------------------------------------------------------------------------------------

maxInt :: Integer
maxInt = 2

ints :: [Integer]
ints = [0..2]

univInt :: [Value]
univInt = map Int ints

-- 0-, 1-, and 2-tuples of Int
univTuples :: [Value]
univTuples = [Tup []] ++ [ Tup [x] | x <- univInt ] ++ [ Tup [x,y] | x <- univInt, y <- univInt ]

univ :: [Value]
univ = usort $
     univInt
  ++ univTuples  -- Comment out this for better speed
  ++ [ Fun [ PFun [0] id, PFun [1] id, PFun [2] id ]  -- = <0,1,2>
     , Fun [ PFun univInt id ]
     , Fun [ PFun [0,1] id ]
     ]
  ++ concat
     [ [ Fun [ PFun [0] f, PFun [1] f, PFun [2] f ]
       , Fun [ PFun univInt f ]
       ]
     | k <- ints
     , let f _ = Int k
     ]
  ++ [Fun [fcnAdd], Fun [fcnLE], Fun [fcnInt] ]

usort :: Ord a => [a] -> [a]
usort = map head . group . sort

fcnAdd :: Value :->? Value
fcnAdd = PFun { dom = map fst xyz, apply = \ xy -> fromJust $ lookup xy xyz }
  where xyz = [ (Tup [Int x, Int y], Int ((x + y) `rem` (maxInt + 1))) | x <- ints, y <- ints ]

fcnLE :: Value :->? Value
fcnLE = PFun { dom = map fst xyz, apply = \ xy -> fromJust $ lookup xy xyz }
  where xyz = [ (Tup [Int x, Int y], Int x) | x <- ints, y <- ints, x <= y ]

fcnInt :: Value :->? Value
fcnInt = PFun { dom = map fst xy, apply = \ x -> fromJust $ lookup x xy }
  where xy = [ (Int x, Int x) | x <- ints ]

----------------------------------------------------------------------------------------

newtype Ident = Ident String
 deriving ( Eq, Ord )

instance Show Ident where
  show (Ident x) = x

----------------------------------------------------------------------------------------

newtype ENV = ENV [ [(Ident,Value)] ] -- disj (conj pair)
 deriving ( Eq, Ord )

instance Show ENV where
  show (ENV []) = "fail"
  show (ENV es) = intercalate "/" (map showE es)

showE :: [(Ident,Value)] -> String
showE []  = "()"
showE xvs = intercalate ";" [ show x ++ "=" ++ show v | (x,v) <- xvs ]

(%=) :: Ident -> Value -> ENV
x %= v = if x == Ident "_" then error "_ in %=" else
         ENV [ [(x,v)] ]

univE :: ENV
univE = ENV [ [] ]

failE :: ENV
failE = ENV []

hide :: [Ident] -> ENV -> ENV
hide xs (ENV xvss) =
  ENV (usort [ usort [ (x,v) | (x,v)<-xvs, x `notElem` xs ]
             | xvs <- xvss
             ])

compl :: ENV -> ENV
compl (ENV xvss) =
  bigIntersect [ ENV (usort [ [(x,v')] | (x,v)<-xvs, v'<-univ, v/=v' ])
               | xvs <- xvss
               ]

(%/\) :: ENV -> ENV -> ENV
ENV xvss %/\ ENV yvss =
  ENV (usort [ zvs
             | xvs <- xvss
             , yvs <- yvss
             , zvs <- xvs `merge` yvs
             ])
 where
  []          `merge` yvs         = [yvs]
  xvs         `merge` []          = [xvs]
  ((x,v):xvs) `merge` ((y,w):yvs) =
    case x `compare` y of
      LT -> [ (x,v):zvs | zvs <- merge xvs ((y,w):yvs) ]
      EQ -> [ (x,v):zvs | v==w, zvs <- merge xvs yvs ]
      GT -> [ (y,w):zvs | zvs <- merge ((x,v):xvs) yvs ]

(%\/) :: ENV -> ENV -> ENV
ENV xvss %\/ ENV yvss = ENV (usort (xvss ++ yvss))

infix  5 %=
infixr 4 %/\
infixr 3 %\/

----------------------------------------------------------------------------------------

vals :: Ident -> ENV -> [Value]
vals x (ENV xvss) =
  usort [ v | xvs <- xvss, v <- case lookup x xvs of
                                  Nothing -> univ
                                  Just v  -> [v] ]

----------------------------------------------------------------------------------------
-- derived operators

bigUnion :: [ENV] -> ENV
bigUnion = foldr (%\/) failE

bigIntersect :: [ENV] -> ENV
bigIntersect = foldr (%/\) univE

(%\\) :: ENV -> ENV -> ENV
env1 %\\ env2 = env1 %/\ compl env2

----------------------------------------------------------------------------------------

clean :: [ENV] -> [ENV]
clean ss = [ s | s <- ss, s /= ENV [] ]

----------------------------------------------------------------------------------------
