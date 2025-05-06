{-# OPTIONS_GHC -Wno-x-partial -Wno-name-shadowing #-}
module ENV(
  Ident(..),
  univ, univInt,
  numInt,
  fcnAdd, fcnLE, fcnInt, fcnAny, fcnFun,
  ENV,
    univE, failE,
    hide, compl,
    (%=), (%/\), (%\/), (%\\),
    extract,
    extracts,
  bigUnion,
  bigIntersect,
  bigUnique,
  quant,
  clean,
  ) where

import Data.List( sort, group, intercalate, subsequences )
import Data.Maybe(fromJust)
--import qualified Data.Map as M

import Dom

----------------------------------------------------------------------------------------

numInt :: Integer
numInt = 3

ints :: [Integer]
ints = [0..numInt-1]

univInt :: [Value]
univInt = map Int ints

-- 0-, 1-, and 2-tuples of Int
univTuples :: [Value]
univTuples = [Tup []] ++ [ Tup [x] | x <- univInt ] ++ [ Tup [x,y] | x <- univInt, y <- univInt ]

-- All int->int functions
univIntToInt :: [Value]
univIntToInt = [ Fun [f] | d <- subsequences univInt, f <- mkDomRng d univInt ]

mkDomRng :: [Value] -> [Value] -> [Value :->? Value]
mkDomRng fdom frng =
  let rs = sequence $ replicate (length fdom) frng
      fs = map (zip fdom) rs
      mkMapping xys = mkFun (map fst xys) (\ x -> fromJust $ lookup x xys)
  in  map mkMapping fs

univ :: [Value]
univ = usort $ {- Fun [fcnAny] : -} univ'

univ' :: [Value]
univ' = usort $
     univInt
  -- ++ univTuples  -- Comment out this for better speed
  -- ++ univIntToInt
  ++ [ Fun [ mkFun [0] id, mkFun [1] id, mkFun [2] id ]  -- = <0,1,2>
     , Fun [ mkFun univInt id ]
     , Fun [ mkFun [0,1] id ]
     ]
  ++ concat
     [ [ Fun [ mkFun [0] f, mkFun [1] f, mkFun [2] f ]
       , Fun [ mkFun univInt f ]
       ]
     | k <- ints
     , let f _ = Int k
     ]
  -- ++ [Fun [fcnAdd], Fun [fcnLE], Fun [fcnInt] ]

usort :: Ord a => [a] -> [a]
-- `usort` canonicalises a list by
--     sorting it
--     removing duplicates
usort = map head . group . sort

fcnAdd :: Value :->? Value
fcnAdd = mkFun (map fst xyz) (\ xy -> fromJust $ lookup xy xyz)
  where xyz = [ (Tup [Int x, Int y], Int ((x + y) `rem` numInt)) | x <- ints, y <- ints ]

fcnLE :: Value :->? Value
fcnLE = mkFun (map fst xyz) (\ xy -> fromJust $ lookup xy xyz)
  where xyz = [ (Tup [Int x, Int y], Int x) | x <- ints, y <- ints, x <= y ]

fcnInt :: Value :->? Value
fcnInt = mkFun (map fst xy) (\ x -> fromJust $ lookup x xy)
  where xy = [ (Int x, Int x) | x <- ints ]

fcnAny :: Value :->? Value
fcnAny = mkFun univ' id

fcnFun :: Value :->? Value
fcnFun = mkFun [ f | f@(Fun _) <- univ' ] id

----------------------------------------------------------------------------------------

newtype Ident = Ident String
 deriving ( Eq, Ord )

instance Show Ident where
  show (Ident x) = x

----------------------------------------------------------------------------------------
{- Note [ENV]
~~~~~~~~~~~~~
* An Env is a total function from Ident to Value

* An EnvSet represents a Set of Envs
  * It is represented by a [Constraint], where the constraints specify
    which Envs are in the set
  * Invariant (I think): the list is kept canonicalised by `usort`

* A ENV represents a Set(Env)
  * It is represented by a [EnvSet], which represents the union of all
    the Envs in the EnvSet
  * Invariant (I think): the list is kept canonicalised by `usort`
-}

-- See Note [ENV]
type Constraint = (Ident, Value)  -- The constraint x = v
type EnvSet     = [Constraint]    -- Conjunction of constraints

newtype ENV = ENV [ EnvSet ] -- See Note [ENV]
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
             -- SPJ: is this inner usort necessary?
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

extract :: Ident -> ENV -> [(Value, ENV)]
extract x env@(ENV xvss) =
  [ (v, hide [x] (env %/\ x%=v))
  | v <- usort [ v
               | xvs <- xvss
               , v <- case lookup x xvs of
                        Nothing -> univ
                        Just v  -> [v]
               ]
  ]

extracts :: [Ident] -> ENV -> [([Value], ENV)]
extracts []     env = [([],env)]
extracts (x:xs) env = [ (v:vs, env'')
                      | (v,env') <- extract x env
                      , (vs,env'') <- extracts xs env'
                      ]

----------------------------------------------------------------------------------------
-- derived operators

bigUnion :: [ENV] -> ENV
bigUnion = foldr (%\/) failE

bigIntersect :: [ENV] -> ENV
bigIntersect = foldr (%/\) univE

bigUnique :: [ENV] -> ENV
bigUnique envs = go envs cenvs (tail nenvs)
 where
  cenvs = map compl envs
  nenvs = scanr (%/\) univE cenvs

  go [] _ _ = failE
  go (env:envs) (cenv:cenvs) (nenv:nenvs) =
    (env %/\ nenv) %\/ (cenv %/\ go envs cenvs nenvs)
  go _ _ _ = error "bigUnique"

quant :: Ident -> ([ENV] -> ENV) -> ENV -> ENV
quant x bigOp env = bigOp [ env' | (_,env') <- extract x env ]

(%\\) :: ENV -> ENV -> ENV
env1 %\\ env2 = env1 %/\ compl env2

----------------------------------------------------------------------------------------

clean :: [ENV] -> [ENV]
clean ss = [ s | s <- ss, s /= ENV [] ]

----------------------------------------------------------------------------------------
