{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Eval where
import Control.Applicative
import Control.Monad
import Data.Foldable(asum)

import CoreExpr
import Value

snoc :: [a] -> a -> [a]
snoc xs x= xs ++ [x]

------

newtype E a = E { runE :: [a] }
  deriving (Show, Functor, Applicative, Monad, Alternative)


eval :: Env -> Expr -> E Value
eval r (Var x) | Just v <- lookup x r = pure v
               | otherwise = error $ "eval: undefined " ++ show x
eval r (Int i) = pure $ VInt i
eval r (Unify e1 e2) = bind2 unify (eval r e1) (eval r e2)
eval r (Apply e1 e2) = bind2 (apply True) (eval r e1) (eval r e2)
eval r (Call e1 e2) = bind2 (apply False) (eval r e1) (eval r e2)
eval r (Lambda x e) = pure $ VLambda r x e
eval r (Alt e1 e2) = eval r e1 <|> eval r e2
eval r (Array es) = VArray <$> traverse (eval r) es
eval r (If e1 e2 e3) = do
  case evalEnvs r e1 of
    [] -> eval r e3
    r' : _ -> eval (r' ++ r) e2
eval r (For e1 e2) =
  let rs = evalEnvs r e1
      vss = map (\ q -> runE (eval (q ++ r) e2)) rs
      vs = map VArray $ sequence vss
  in  E vs
eval r (Seq es) = last <$> mapM (eval r) es
eval r (DefIn [] e) = eval r e
eval r (DefIn (x:xs) e) = asum [ eval ((x, v):r) (DefIn xs e) | v <- allValues ]
eval _ e = error $ "eval: unexpected " ++ show e

evalEnvs :: Env -> Expr -> [Env]
evalEnvs r ee =
  case ee of
    DefIn xs e -> defs xs e
    e -> defs [] e
  where
    defs xs e =
      let arrs = runE $ eval r $ DefIn xs $ Seq [e, Array $ map Var xs]
          envs = map (\ (VArray vs) -> zip xs vs) arrs
      in  envs


nonEmpty :: E Value -> E Value
nonEmpty e = if null (runE e) then pure $ VWrong "nonEmpty" else e

bind2 :: (Value -> Value -> E Value) -> E Value -> E Value -> E Value
bind2 f ma mb =
  ma >>= \case
    VWrong s -> pure $ VWrong s
    a ->
      mb >>= \case
        VWrong s -> pure $ VWrong s
        b -> f a b

unify :: Value -> Value -> E Value
--unify v@(VInt i) (VInt i') | i == i' = pure v
--unify (VArray vs) (VArray vs') | length vs == length vs' = VArray <$> zipWithM unify vs vs'
--unify VLambda{} VLambda{} = pure (VWrong "unify lambda")
--unify VPrim{} VPrim{} = pure (VWrong "unify prim")
unify v1 v2 | v1 == v2 = pure v1
unify _ _ = empty

apply :: Bool -> Value -> Value -> E Value
apply mayFail fcn arg =
  (if mayFail
  then id
  else nonEmpty) $
    case (fcn, arg) of
      (VPrim p, _) -> primApply p arg
      (VLambda r x e, v) -> eval ((x,v):r) e
      (VArray vs, v) | VInt i <- v, let i' = fromInteger i, i' >= 0 && i' < length vs -> pure $ vs !! i'
                     | otherwise -> empty
      _ -> pure $ VWrong "not a function"

primApply :: String -> Value -> E Value
primApply "any" v = pure v
primApply "int" v | VInt _ <- v = pure v
                  | otherwise = empty
primApply "false" _ = empty
primApply "operator'+'" v | VArray [VInt i1, VInt i2] <- v = pure $ VInt $ i1 + i2
                          | otherwise = empty
primApply "operator'-'" v | VArray [VInt i1, VInt i2] <- v = pure $ VInt $ i1 - i2
                          | otherwise = empty
primApply "operator'*'" v | VArray [VInt i1, VInt i2] <- v = pure $ VInt $ i1 * i2
                          | otherwise = empty
primApply "operator'<'" v | VArray [v1, v2] <- v, v1 < v2 = pure v1
                          | otherwise = empty
primApply "operator'<='" v | VArray [v1, v2] <- v, v1 <= v2 = pure v1
                           | otherwise = empty
primApply "operator'>'" v | VArray [v1, v2] <- v, v1 > v2 = pure v1
                          | otherwise = empty
primApply "operator'>='" v | VArray [v1, v2] <- v, v1 >= v2 = pure v1
                           | otherwise = empty
primApply s _ = error $ "primApply: unknown " ++ show s

initialEnv :: Env
initialEnv = [
  prim "int",
  prim "any",
  prim "false",
  prim "operator'+'",
  prim "operator'-'",
  prim "operator'*'",
  prim "operator'<'",
  prim "operator'<='",
  prim "operator'>'",
  prim "operator'>='"
  ]
  where prim s = (Ident s, VPrim s)
