{-# LANGUAGE ConstraintKinds #-}
import Control.Applicative
import Control.Monad
import Debug.Trace

type Ident = String

-- e ::= x | k |  (e1 | e2)  |  (e = k)  |  defrec { x := e } in e | (e1,e2) | fst(e) | snd(e)
data Exp = Var Ident | Con Integer |
           Alt Exp Exp | Fail |
           Pair Exp Exp | Fst Exp | Snd Exp |
           Def Ident Exp Exp |
           Equal Exp Exp |
           Succ Exp
  deriving (Show)

data Value = VCon Integer | VPair Value Value
  deriving (Eq)

instance Show Value where
  show (VCon i) = show i
  show (VPair v1 v2) = "(" ++ show v1 ++ ", " ++ show v2 ++ ")"

data Cell = Val Value | Clo Env Exp

instance Show Cell where
  show (Val v) = "Val " ++ show v
  show (Clo _ e) = "Clo _ (" ++ show e ++ ")"

type Env = [(Ident, Cell)]

class FirstAlt f where
  firstAlt :: f a -> f a

instance FirstAlt Maybe where
  firstAlt a = Just $ case a of Just x -> x

instance FirstAlt [] where
  firstAlt a = [case a of x:_ -> x]

type MonadEval m = (Monad m, Alternative m, FirstAlt m)

eval :: (MonadEval m) => Exp -> Env -> m Value
eval (Var i) rho = evalVar i rho
eval (Con k) _ = pure $ VCon k
eval (Alt e1 e2) rho = eval e1 rho <|> eval e2 rho
eval Fail _ = empty
eval (Equal e1 e2) rho = do v1 <- eval e1 rho; v2 <- eval e2 rho; guard (v1 == v2); pure v1
eval (Pair e1 e2) rho = VPair <$> eval e1 rho <*> eval e2 rho
eval (Fst e1) rho = vfst <$> eval e1 rho
  where vfst (VPair v _) = v
        vfst v = error $ "vfst " ++ show v
eval (Snd e1) rho = vsnd <$> eval e1 rho
  where vsnd (VPair _ v) = v
        vsnd v = error $ "vsnd " ++ show v
eval (Succ e1) rho = vsucc <$> eval e1 rho
  where vsucc (VCon i) = VCon (succ i)
        vsucc v = error $ "vsucc " ++ show v
  
eval (Def x r e) rho = do xv <- eval r rho'; eval e ((x, Val xv) : rho)
  where rho' = (x, Clo rho' r) : rho

evalVar :: (MonadEval m) => Ident -> Env -> m Value
--evalVar i rho | trace ("evalVar " ++ show (i, rho)) False = undefined
evalVar i rho =
  case lookup i rho of
    Nothing -> error $ "not found " ++ show i
    Just (Val v) -> pure v
    Just (Clo rho' e) ->
{- Deadlock on ex2
      case eval e rho' of
        [] -> error "it happened"
        v : _ -> [v]
-}
      firstAlt $ eval e rho'

evalMaybe :: Exp -> Maybe Value
evalMaybe = flip eval []

evalList :: Exp -> [Value]
evalList = flip eval []

ixy = "xy"
xy = Var ixy
x = Fst xy
y = Snd xy

-- eval ex1 [] == [(1, 2),(1, 3),(2, 2),(2, 3)]
ex1 = Def ixy (Pair (Con 1 `Alt` Con 2) (Con 2 `Alt` Con 3)) xy

-- eval ex2 [] == [(2, 1),(2, 2),(3, 1),(3, 2)]
ex2 = Def ixy (Pair (Succ y `Alt` Con 3) (Con 1 `Alt` Con 2)) xy

-- eval ex5 [] == bottom (eval loops)
ex5 = Def ixy (Pair (Equal y (Con 4) `Alt` Con 2) (Con 3 `Alt` Con 4)) xy
