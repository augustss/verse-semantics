{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
module OpSemLA where
import Data.List
import Control.Monad.State.Strict
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
import Data.String
import GHC.Stack
import Ex

--------------------------------
--
-- Machine state
--
--------------------------------

{-
-- ExnState: the execution state during evaluation of an expression
data ExnState
  = Exn { context :: Context
        , frame   :: Frame
        , ops     :: [Op] }
  deriving (Show)

-- Context for expression evaluation
data Context
  = Cxt { suspension :: [Suspension]
        , next       :: Maybe ExnState  -- Nothing => no forks
        , parent     :: Maybe Context
        }
  deriving (Show)
-}

type Frame = Map Name Value

data Value = VInteger Integer
           | VArray [Reg]
           | VUnresolved
  deriving (Show)

type Nat = Int

data Reg = Reg { reg_frame :: Nat  -- 0 is the outermost frame
               , reg_name  :: Name }
--  deriving (Show)
instance Show Reg where
  show (Reg f n) = n ++ "{" ++ show f ++ "}"

data Suspension
  = Susp Frame Continuation
  deriving (Show)

data Continuation
  = AddWaitingFirstArg
  | AddWaitingForSecondArg
  deriving (Show)

data Atom
  = AnInteger Integer
  deriving (Show)

data Op
  = Atom Reg Atom
  | Unify Reg Reg
  | Choice [ [Op] ]
  | MkArray Reg [Reg]
  | Failure
  | Add Reg Reg Reg
  | NewFrame Nat [Name] [Op]
  | PopFrame
  | Stop Reg   -- Just for testing, print the reg and stop
  deriving (Show)

--------------------------------
--
-- Code
--
--------------------------------

{- BNF syntax for the language
   e ::= x
      |  k
      |  (s1 | s2)
      |  (e = k)
      |  x := e
      |  (e1,...,en)
      |  e[i]
      |  e1 + e2
      |  :false
      |  for(s1){e2}
      |  do{s}
      |  :e
   s ::= def {x1,...} in e
-}

type Name = String

data Exp = Var Name
         | Con Integer
         | Semi Exp Exp  -- e1; e2
         | Alt SExp SExp
         | Equal Exp Exp
         | Set Name Exp
         | Array [Exp]   -- (e1, ..., en)  aka  array{e1, ..., en}
         | AppS Exp Exp
         | Plus Exp Exp
         | Fail
         | For SExp SExp
         | If SExp SExp SExp
         | Do SExp
         | Range Exp     -- :e
  deriving (Show)

data SExp     -- A scope-limiting construct
  = Def [Name]   -- Bring these variables into scope
        Exp      -- In this expression
  deriving (Show)

---------------------
--      Sugar
---------------------

instance Num Exp where
  (+) = Plus
  fromInteger = Con

instance IsString Exp where
  fromString = Var

infixl 4 |||
(|||) :: Exp -> Exp -> Exp
x ||| y = Alt (addDef x) (addDef y)

infixl 3 #
(#) :: Exp -> Exp -> Exp
(#) = Pair

infixl 5 ===
(===) :: Exp -> Exp -> Exp
(===) = Equal

infix 2 :=
pattern (:=) :: Name -> Exp -> Exp
pattern (:=) x e = Set x e

pattern Fst :: Exp -> Exp
pattern Fst e = AppS e (Con 0)
pattern Snd :: Exp -> Exp
pattern Snd e = AppS e (Con 1)
pattern Pair :: Exp -> Exp -> Exp
pattern Pair e1 e2 = Array [e1, e2]

-- Sequencing, evaluate both and return second
infixl 1 `semi`
semi :: Exp -> Exp -> Exp
semi x y = Semi x y

-- Sequencing, evaluate both and return first
infix 1 `wher`
wher :: Exp -> Exp -> Exp
wher x y = Fst (Pair x y)

for :: Exp -> Exp -> Exp
for e1 e2 = For (addDef e1) (addDef e2)

doo :: Exp -> Exp
doo e = Do (addDef e)

--lam :: Name -> Exp -> Exp
--lam n e = Lam n (addDef e)

-- Add all variables defined in the current scope.
addDef :: HasCallStack => Exp -> SExp
addDef e | xs /= nub xs = error $ "Duplicate := " ++ show (e, xs)
         | otherwise = Def xs e
  where xs = findSet e

findSet :: Exp -> [Name]
findSet Var {}   = []
findSet Con {}   = []
findSet (Semi e1 e2) = findSet e1 ++ findSet e2
findSet Alt {}   = []
findSet Fail     = []
findSet For {}   = []
findSet If {}   = []
findSet Do {}    = []
--findSet Lam {}   = []
findSet (AppS  e1 e2) = findSet e1 ++ findSet e2
--findSet (AppI  e1 e2) = findSet e1 ++ findSet e2
findSet (Equal e1 e2) = findSet e1 ++ findSet e2
findSet (Set x e) = x : findSet e
findSet (Array es) = concatMap findSet es
findSet (Plus e1 e2) = findSet e1 ++ findSet e2
findSet (Range e) = findSet e

--------------------------
--
-- Convert an Exp to a list of Op
--
--------------------------

data CompileState = CompileState
  { env :: !(Map Name Reg),
    curFrame :: !Nat,
    nextTemp :: !Int,
    cops :: [Op]  -- generated ops so far
  }
  deriving (Show)

type C = State CompileState

cLookup :: Name -> C Reg
cLookup n = do
  m <- gets env
  case M.lookup n m of
    Nothing -> error $ "cLookup: " ++ show n
    Just r -> pure r

newReg :: C Reg
newReg = do
  s <- get
  let t = succ (nextTemp s)
  put s{nextTemp = t}
  pure $ Reg { reg_frame = curFrame s, reg_name = tmpName t }

tmpName :: Int -> Name
tmpName t = "%" ++ show t

emit :: Op -> C ()
emit op = modify $ \ s -> s { cops = cops s ++ [op] }

newFrame :: [Name] -> C a -> C ([Op], a)
newFrame ns ca = do
  olds <- get
  let fr = curFrame olds + 1
  put olds { env = foldr (uncurry M.insert) (env olds) [ (n, Reg { reg_name = n, reg_frame = fr }) | n <- ns]
           , curFrame = fr
           , cops = []
           }
  a <- ca
  s <- get
  put olds{ nextTemp = nextTemp s }
  let tmps = [ tmpName t | t <- [nextTemp olds + 1 .. nextTemp s] ]
  pure ([NewFrame fr (ns ++ tmps) (cops s ++ [PopFrame])], a)

expToReg :: Exp -> C Reg
expToReg (Var n) = cLookup n
expToReg (Con i) = do t <- newReg; emit $ Atom t (AnInteger i); pure t
expToReg (Semi e1 e2) = expToReg e1 >> expToReg e2
expToReg (Alt e1 e2) = do
  t <- newReg
  ops1 <- sexpToOps t e1
  ops2 <- sexpToOps t e2
  emit $ Choice [ops1, ops2]
  pure t
expToReg (Equal e1 e2) = do
  r1 <- expToReg e1
  r2 <- expToReg e2
  emit $ Unify r1 r2
  pure r1
expToReg (Set n e) =
  expToReg $ Equal (Var n) e
expToReg (Array es) = do
  rs <- mapM expToReg es
  t <- newReg
  emit $ MkArray t rs
  pure t
expToReg (AppS _e1 _e2) = undefined
expToReg (Plus e1 e2) = do
  r1 <- expToReg e1
  r2 <- expToReg e2
  t <- newReg
  emit $ Add t r1 r2
  pure t
expToReg Fail = do
  emit Failure
  newReg                -- we must return something, but this reg will never be set
expToReg (For _e1 _e2) = undefined
expToReg (If _e1 _e2 _e3) = undefined
expToReg (Do _e) = undefined
expToReg (Range _e) = undefined

sexpToOps :: Reg -> SExp -> C [Op]
sexpToOps t (Def ns e) = do
  (os, r) <- newFrame ns $ expToReg e
  pure $ os ++ [Unify t r]

comp :: SExp -> [Op]
comp e = evalState se cs
  where cs = CompileState{ env = M.empty, curFrame = 0, nextTemp = 1, cops = [] }
        se = do
          t <- newReg
          (++) <$> sexpToOps t e <*> pure [Stop t]

-----------------------

data RunState = RunState
  { rs_frame :: Frame
  , rs_outer :: RunState  -- surrounding lexical scope
  , rs_ops :: [Op]
  , rs_cur :: Nat
  }
  deriving (Show)

type R = State RunState

assert :: String -> Bool -> R ()
assert s False = error $ "assert: " ++ s
assert _ True = pure ()

getOp :: R Op
getOp = do
  ops <- gets rs_ops
  case ops of
    [] -> undefined
    op : ops' -> do modify $ \ s -> s {rs_ops = ops'}; pure op

assign :: Reg -> Value -> R ()
assign Reg{..} v = do
  fr <- gets rs_frame
  cur <- gets rs_cur
  assert "assign" (reg_frame == cur)
  case M.lookup reg_name fr of
    Just VUnresolved -> modify $ \ s -> s{rs_frame = M.insert reg_name v fr}
    x -> error $ "assign: " ++ show x

getReg :: Reg -> R Value
getReg r = do
  let look rs | reg_frame r == rs_cur rs = pure $ fromMaybe (error "getReg") $ M.lookup (reg_name r) (rs_frame rs)
              | otherwise = look (rs_outer rs)
  get >>= look

step :: R ()
step = do
  op <- getOp
  stepOp op

stepOp :: Op -> R ()
stepOp (Atom r (AnInteger i)) = assign r (VInteger i)
stepOp (Unify _r1 _r2) = undefined
stepOp (Choice opss) = undefined
stepOp (MkArray r rs) = assign r (VArray rs)
stepOp Failure = undefined
stepOp (Add r1 r2 r3) = undefined
stepOp (NewFrame f ns ops) = do
  s <- get
  put RunState{ rs_frame = M.fromList (zip ns (repeat VUnresolved)), rs_cur = f, rs_outer = s, rs_ops = ops }
stepOp PopFrame = do
  s <- get
  put $ rs_outer s
stepOp (Stop r) = do
  v <- getReg r
  error $ "Stop " ++ show v

run :: [Op] -> ()
run ops =
  evalState (forever step)
    RunState{ rs_frame = M.empty, rs_cur = -1, rs_outer = error "rs_outer", rs_ops = ops }

---------------------
--      Tests
---------------------

{-
ok :: (Show a) => String -> a -> Exp -> Ex String
ok n r e = Ex n (Just $ show r) (show $ ev e)

bad :: String -> Exp -> Ex String
bad n e = Ex n Nothing (show $ ev e)

bug :: (Show a) => String -> a -> Exp -> Ex String
bug n _r e = Ex ("bug: " ++ n) Nothing (show $ ev e)
-}
ok _ _ e = addDef e
bad _ _ e = addDef e

---------------------
-- Simple, single valued tests.
---------------------
test101 = ok "test101" [5] $
  5

test102 = ok "test102" [42] $
  5 + 37

test103 = ok "test103" [(5,37)] $
  5 # 37

test104 = ok "test104" [(1,2,3,4)] $
  Array [1,2,3,4]

---------------------
-- Variable scopes
---------------------
test201 = ok "test201" [(5,5)] $
  ("x" := 5) # "x"

test202 = ok "test202" [(5,5)] $
  "x" # ("x" := 5)

test203 = ok "test203" [(7,6)] $
  "x"+1 # ("x" := 6)

test204 = ok "test204" [(7,6,6,5)] $
  Array ["x"+1, "x" := "y", "y" := "z"+1, "z" := 5]

test205 = bad "test205" $
  ("x" := 1) # ("x" := 2)

test206 = bad "test206" $
  "x"

test207 = ok "test207" [(3,4)] $
  3 # doo ("x":= 4)

test208 = bad "test208" $
  "x" # doo ("x":= 4)

-- Check that mutual recursion fails
test209 = bad "test209" $
  "x" := "y" `semi` "y" := "x"

test210 = ok "test210" [(1,(2,3))] $
  "x" := (1 # "y") `semi`
  "y" := (2 # "z") `semi`
  "z" := 3 `semi`
  "x"

test211 = bad "test211" $
  "x" := 1 `semi` "x" := 2

-- The x1 used to be x, but shadowing is not allowed
test212 = ok "test212" [(1,2)] $
  "x" := 2 `semi` (doo ("x1" `wher` "x1" := 1) # "x")

