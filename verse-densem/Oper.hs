module Oper(
  Oper(..),
  Ident(..),
  exis,
  free,
  ) where
import Data.List(union, (\\), nub, intercalate)
import ENV(Ident(..))

data Oper
  = Ident :=: Ident                      -- x=y
  | Ident :=  Integer                    -- x=k
  | Ident :<= Ident                      -- x<=y
  | Ident :=<> [Ident]                   -- x=<y1,y2,...>
  | Exi Ident                            -- ∃x
  | Ident :=@ (Ident,Ident)              -- y=f[x]
  | Ident :=\ (Ident, Oper, Oper, Ident) -- f=\x.(op1){op2}(y)
  | Oper :>: Oper                        -- op1;op2
  | Oper :|: Oper                        -- op1|op2
  | Fail                                 -- fail
  | Scope Oper                           -- {op}
  | If Oper Oper Oper                    -- if(op1){op2}else{op3}
  | NoOp
 deriving ( Eq, Ord )

infix  5 :=:, :=, :<=, :=@, :=\, :=<>
infixr 4 :>:
infixr 3 :|:

instance Show Oper where
  show (x :=: y)           = show x ++ "=" ++ show y
  show (x := k)            = show x ++ "=" ++ show k
  show (x :<= y)           = show x ++ "<=" ++ show y
  show (x :=<> ys)         = show x ++ "=<" ++ intercalate "," (map show ys) ++ ">"
  show (Exi x)             = "∃" ++ show x
  show (y:=@(f,x))         = show y ++ "=" ++ show f ++ "[" ++ show x ++ "]"
  show (f:=\(x,op1,op2,y)) = show f ++ "=\\" ++ show x ++ ".(" ++ show op1 ++ ")"
                             ++ "{" ++ show op2 ++ "}(" ++ show y ++ ")"
  show (op1 :>: op2)       = show1 ";" op1 ++ "; " ++ show1 ";" op2
  show (op1 :|: op2)       = show1 "|" op1 ++ " | " ++ show1 "|" op2
  show Fail                = "fail"
  show (Scope op)          = "{" ++ show op ++ "}"
  show (If op1 op2 op3)    = "if(" ++ show op1 ++ "){" ++ show op2 ++ "}else{" ++ show op3 ++ "}"
  show NoOp                = "nop"

show1 :: String -> Oper -> String
show1 op e@(_ :>: _) = if op==";" then show e else showp e
show1 op e@(_ :|: _) = if op=="|" then show e else showp e
show1 _  e           = show e

showp :: Oper -> String
showp e = "(" ++ show e ++ ")"

free :: Oper -> [Ident]
free (x :=: y)           = nub [x,y]
free (x := _k)           = [x]
free (x :<= y)           = nub [x,y]
free (x :=<> ys)         = nub (x:ys)
free (Exi x)             = [x]
free (y:=@(f,x))         = nub [f,x,y]
free (f:=\(x,op1,op2,y)) = nub [f,y] `union` (free (Scope op1) \\ [x]) `union` (free (Scope op2) \\ (x:exis op1))
free (op1 :>: op2)       = free op1 `union` free op2
free (op1 :|: op2)       = free (Scope op1) `union` free (Scope op2)
free Fail                = []
free (Scope op)          = free op \\ exis op
free (If op1 op2 op3)    = free (Scope op1) `union` (free (Scope op2) \\ exis op1) `union` free (Scope op3)
free NoOp                = []

exis :: Oper -> [Ident]
exis (Exi x)       = [x]
exis (op1 :>: op2) = exis op1 `union` exis op2
exis _             = []

