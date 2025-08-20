{-# OPTIONS_GHC -Wno-x-partial -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
#define DEBUG_SHOW 0
module ENVS(
  ENV,
  empty, univ,
  (/\), (\/), (\\\), compl,
  (.=), (.=.),
  (./=),
  hide, hides,
  bigUnion, bigIntersect,
  extractVar,  -- dubious
  disj, (&&&), add,
  )where
import Data.Function(on)
import Data.List(intercalate, groupBy, sortBy)
import ValueS

default ()

-- ENV API

infix  5 .=, .=., ./=
infixr 4 /\ -- dummy
infixr 3 \/

empty :: ENV
empty = OR []

univ :: ENV
univ = OR [ true ]

(/\) :: ENV -> ENV -> ENV
OR as /\ OR bs = disj [ a &&& b | a <- as, b <- bs ]

(\/) :: ENV -> ENV -> ENV
OR as \/ OR bs = disj (as ++ bs)

(\\\) :: ENV -> ENV -> ENV
x \\\ y = x /\ compl y

(.=) :: Ident -> Value -> ENV
x .= v = OR [ YES [x :=: Value v] ]

(./=) :: Ident -> Value -> ENV
x ./= v = compl (x .= v)

(.=.) :: Ident -> Ident -> ENV
x .=. y = OR [ YES (case x `compare` y of
                      LT -> [y :=: Ident x]
                      EQ -> []
                      GT -> [x :=: Ident y]) ]

hide :: Ident -> ENV -> ENV
hide x (OR cs) = disj [ hideCase x c | c <- cs ]

hides :: [Ident] -> ENV -> ENV
hides xs env = foldr hide env xs

compl :: ENV -> ENV
compl (OR as) =
  foldr (/\) univ
  [ OR [YES [neg c] | c <- cs]
  | YES cs <- as
  ]

bigUnion :: [ENV] -> ENV
bigUnion = foldr (\/) empty

bigIntersect :: [ENV] -> ENV
bigIntersect = foldr (/\) univ

----------------------------------

data Thing
  = Value Value
  | Ident Ident
 deriving ( Eq, Ord )

instance Show Thing where
  show (Value v) = show v
  show (Ident x) = show x

data CONSTR
  = Ident :=: Thing
  | Ident :/=: Thing
--x  | FuncHas   Ident Int Thing Thing  -- FuncHas   hs i x y means  (x,y) `elem` hs[i]
--x  | FuncLacks Ident Int Thing Thing  -- FuncLacks hs i x y means  (x,y) `notElem` hs[i]
 deriving ( Eq, Ord )

instance Show CONSTR where
  show (x :=: t)  = show x ++ "=" ++ show t
  show (x :/=: t) = show x ++ "≠" ++ show t
--  show (FuncHas   f i x y)  = show x ++ "↦" ++ show y ++ "∈" ++ show f ++ "[" ++ show i ++ show "]"
--  show (FuncLacks f i x y)  = show x ++ "↦" ++ show y ++ "∉" ++ show f ++ "[" ++ show i ++ show "]"

neg :: CONSTR -> CONSTR
neg (x :=: t)  = x :/=: t
neg (x :/=: t) = x :=: t
--neg (FuncHas   f i x y) = FuncLacks f i x y
--neg (FuncLacks f i x y) = FuncHas   f i x y

data ENV = OR [ CASE ]
 deriving ( Eq, Ord )
#if DEBUG_SHOW
 deriving (Show)
#else
instance Show ENV where
  show (OR [])              = "∅"
  show (OR cs)              = intercalate " \x222a " (map show cs)
#endif

disj :: [CASE] -> ENV
disj as = OR (usort [ a | a@(YES _) <- as ])

data CASE
  = YES [CONSTR]
  | NO
 deriving ( Eq, Ord )
#if DEBUG_SHOW
 deriving (Show)
#else
instance Show CASE where
  show (YES []) = "U"
  show (YES cs) = "{{" ++ intercalate "," (map showEqs eqss ++ map show neqs) ++ "}}"
    where eqs = [ (i, v) | i :=: Value v <- cs ]
          neqs = filter (\ c -> case c of _ :=: Value _ -> False; _ -> True) cs
          eqss = groupBy ((==) `on` snd) $ sortBy (compare `on` snd) eqs
          showEqs ivs = intercalate "=" (map (show . fst) ivs) ++ "=" ++ show (snd (ivs!!0))
  show NO       = "∅"
#endif

true :: CASE
true = YES []

(&&&) :: CASE -> CASE -> CASE
YES []      &&& alt     = alt
YES (c:cs1) &&& YES cs2 = case add c cs2 of
                            Just cs2' -> YES cs1 &&& YES cs2'
                            Nothing   -> NO
_           &&& _       = NO

hideCase :: Ident -> CASE -> CASE
hideCase x NO       = NO
hideCase x (YES cs) =
  (case [ u | (u :=: Ident x') <- cs, x' == x ] of
     -- x was not a rep
     [] -> true
     -- x was a rep
     us -> YES [ c'
               | c <- cs
               , c' <- case c of
                         y :=: Ident x'  | x' == x -> [ y :=: Ident u ]
                         x' :/=: t       | x' == x -> [ u :/=: t ]
                         t :/=: Ident x' | x' == x -> [ t :/=: Ident u ]
                         _                         -> []
               ]
      where
       u = minimum us {- u is the new rep for x's equivalence class -})
  &&&
  YES [ c
      | c <- cs
      , case c of
          y :=: _        | y == x -> False
          _ :=: Ident y  | y == x -> False
          y :/=: _       | y == x -> False
          _ :/=: Ident y | y == x -> False
          _                       -> True
      ]

usort :: Ord a => [a] -> [a]
--usort = map head . group . sort
-- with the very short lists we have it's faster to use insertion sort
usort = foldr uinsert []

uinsert :: Ord a => a -> [a] -> [a]
uinsert x []     = [x]
uinsert x (y:ys) = case x `compare` y of
                     LT -> x : y : ys
                     EQ -> y : ys
                     GT -> y : uinsert x ys

add :: CONSTR -> [CONSTR] -> Maybe [CONSTR]
add constr cs0 = normConstr constr
 where
  norm :: Thing -> [CONSTR] -> Thing
  norm (Value v) _  = Value v
  norm t         [] = t
  norm (Ident x) ((y :=: t):cs)
    | x == y        = t
    | otherwise     = norm (Ident x) cs
  norm t (_:cs)     = norm t cs

  normConstr (x :=: t) =
    case (norm (Ident x) cs0, norm t cs0) of
      (Value a, Value b)
        | a /= b -> Nothing

      (t1, t2) ->
        case t1 `compare` t2 of
          EQ -> Just cs0
          GT -> addEq t1 t2 cs0
          LT -> addEq t2 t1 cs0
{- minimal speedup
      (t1@(Value x), t2@(Value y)) ->
        if x /= y then Nothing else Just cs0
      (t1@(Ident x), t2@(Ident y)) ->
        case compare x y of
          EQ -> Just cs0
          GT -> addEq t1 t2 cs0
          LT -> addEq t2 t1 cs0
      (t1@(Value _), t2@(Ident _)) -> addEq t2 t1 cs0
      (t1@(Ident _), t2@(Value _)) -> addEq t1 t2 cs0
-}

  normConstr (x :/=: t) =
    case (norm (Ident x) cs0, norm t cs0) of
      (Value a, Value b)
        | a /= b -> Just cs0

      (t1, t2) ->
        case t1 `compare` t2 of
          EQ -> Nothing
          GT -> addNeq t1 t2 cs0
          LT -> addNeq t2 t1 cs0
{- minimal speedup
      (t1@(Value x), t2@(Value y)) ->
        if x == y then Nothing else Just cs0
      (t1@(Ident x), t2@(Ident y)) ->
        case compare x y of
          EQ -> Nothing
          GT -> addNeq t1 t2 cs0
          LT -> addNeq t2 t1 cs0
      (t1@(Value _), t2@(Ident _)) -> addNeq t2 t1 cs0
      (t1@(Ident _), t2@(Value _)) -> addNeq t1 t2 cs0
-}
      
  addEq  (Ident x) t cs = ((x :=: t) `uinsert`) `fmap` subst x t cs
  addNeq (Ident x) t cs = Just ((x :/=: t) `uinsert` cs)

  subst x t ((y :=: Ident x'):cs) | x == x' =
    ((y :=: t) `uinsert`) `fmap` subst x t cs
    
  subst x t ((x' :/=: t'):cs) | x == x' =
    case (t, t') of
      (t, t')            | t == t' -> Nothing
      (Value a, Value b) | a /= b  -> subst x t cs
      (Ident z, t') | Ident z > t' -> ((z :/=: t') `uinsert`) `fmap` subst x t cs
      (t, Ident z)                 -> ((z :/=: t)  `uinsert`) `fmap` subst x t cs
  
  subst x t ((y :/=: Ident x'):cs) | x == x' =
    case t of
      t | t == Ident y -> Nothing
        | otherwise    -> ((y :/=: t) `uinsert`) `fmap` subst x t cs

  subst x t (c:cs) = (c `uinsert`) `fmap` subst x t cs
  subst x t []     = Just []

-- extract definite values of a variable from ENV or die
extractVar :: ENV -> Ident -> [Value]
extractVar (OR cs) x = map extr cs
  where extr NO = error $ "extractVar: NO " ++ show x
        extr (YES os) =
          case [ v | y :=: v <- os, x == y ] of
            [] -> error $ "extractVar: no value " ++ show x
            [Ident _] -> error $ "extractVar: variable " ++ show x
            [Value v] -> v
            _ -> error $ "extractVar: conflicting values " ++ show x
