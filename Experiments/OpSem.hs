module OpSem where

--------------------------------
--
-- Machine state
--
--------------------------------

-- ExnState: the execution state during evaluation of an expression
data ExnState
  = Exn { context :: Context

-- Context for expression evaluation
data Context
  = Cxt { suspension :: [Suspension]
        , next       :: Maybe Context  -- Nothing => no forks
        , parent     :: Maybe Context
        , frame      :: Frame
        , ops        :: [Exp] }
        }

type Frame = Map Name Value   -- Like a little "store"
  -- Def initialises the map to [ xi :-> VUnresolved ]
  -- We could use the Name not being in the Map to mean VUnresolved.

data Value = VInt Int
           | VArray [Reg]
           | VUnresolved

-- Reg plays the role of Lenient
data Reg = Reg { reg_frame :: Nat  -- 0 is the outermost frame
               , reg_name  :: Name }

data Suspension
  = Susp Frame Continuation

data Continuation
  = Add-waiting-first-arg
  | Add-waiting-for-second-arg
  | ...



x := f(y)

tmp := f(y)
x := tmp


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

data Exp = Var Name
         | Con Integer
         | Semi Exp Exp  -- e1; e2
         | Alt SExp SExp
         | Equal Name Name
         | Set Name Exp
         | Array [Exp]   -- (e1, ..., en)  aka  array{e1, ..., en}
         | Sel Exp Int   -- The Int needs to be generalized to Exp
         | Plus Exp Exp
         | Fail
         | For SExp SExp
         | Do SExp
         | Range Exp     -- :e
         | Error  -- to test strictness
  deriving (Show)

data SExp     -- A scope-limiting construct
  = Def [Name]   -- Bring these variables into scope
        Exp      -- In this expression
  deriving (Show)

--------------------------
--
-- Execution
--
--------------------------

data StepResult = Step Context
                | Fail
                | Done
                     Frame    -- We can lookup "result" in here
                     Context  -- Maybe with new suspensions

run :: [Op] -> Context -> Context
run [] 

step :: Context -> Context
step cxt@(Cxt { ops = [] }) = drain cxt

step ex@(Exn { ops = Equal a b : ops, context = cxt })
  = let va = getValue cxt a

