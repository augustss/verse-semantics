{-# LANGUAGE PatternSynonyms #-}
module DenSem where
import Expr(Ident(..))
import Core
import Print(prettyShow)

denSem :: Core -> [Core]
denSem e = map (CValue . valueW) $ evalE emptyEnv e

valueW :: W -> Value
valueW (WInt i) = HNF (HInt i)
valueW (WTuple ws) = VArray (map valueW ws)
valueW _ = undefined

pattern CHasType :: Core -> Core -> Core
pattern CHasType e1 e2 <- CMacro (Ident _ "hastype") (CSeq [e1, e2])

type S a = [a]

unit :: a -> S a
unit a = [a]

empty :: S a
empty = []

union :: S a -> S a -> S a
union s1 s2 = s1 ++ s2

unions :: [S a] -> S a
unions ss = concat ss

isect :: Eq a => S a -> S a -> S a
isect s1 s2 = [ w1 | w1 <- s1, w2 <- s2, w1 == w2 ]

sequ :: S a -> S a -> S a
sequ s1 s2 = [ w2 | _w1 <- s1, w2 <- s2 ]

sOne :: S a -> S a
sOne [] = []
sOne (a:_) = [a]

sAll :: S W -> W
sAll ws = WTuple ws

data W
  = WInt Integer
  | WTuple [W]
  | WFunction (Func W (S W))
  | Wrong
  deriving (Eq)

allW :: [W]
allW = [WInt i | i <- [0 .. 3]]

newtype Func a b = Func (a -> b)
func :: (a -> b) -> Func a b
func f = Func f
apFunc :: Func a b -> a -> b
apFunc (Func f) a = f a
instance Eq (Func a b) where (==) = undefined

type Env = Ident -> W
ext :: Env -> Ident -> W -> Env
ext r x w = \ x' -> if x == x' then w else r x'
emptyEnv :: Env
emptyEnv x = error $ "emptyEnv " ++ show x

evalE :: Env -> Core -> S W
evalE r (CValue v) = unit $ evalV r v
evalE r (CBar e1 e2) = evalE r e1 `union` evalE r e2
evalE _ CFail = empty
evalE r (CUnify e1 e2) = evalE r e1 `isect` evalE r e2
evalE r (CSeq [e]) = evalE r e
evalE r (CSeq (e: es)) = evalE r e `sequ` evalE r (CSeq es)
evalE r (CApply v1 v2) = apply (evalV r v1) (evalV r v2)
evalE r (CDef [] e) = evalE r e
evalE r (CDef (x:xs) e) = unions [ evalE (ext r x w) (CDef xs e) | w <- allW ]
evalE r (COne e) = sOne (evalE r e)
evalE r (CAll e) = unit (sAll (evalE r e))
evalE r (CHasType e1 _e2) | undefined = u
                         | otherwise = unit Wrong
  where u = evalE r e1
evalE r (CSucceeds e) = evalE r e  -- XXX
evalE _ e = error $ "evalE " ++ prettyShow e

evalV :: Env -> Value -> W
evalV r (Var i) = r i
evalV _ (HNF (HInt i)) = WInt i
evalV r (VLam x e) = WFunction $ func $ \ w -> evalE (ext r x w) e
evalV r (VArray vs) = WTuple $ map (evalV r) vs
evalV _ (VPrim "in'+'") = WFunction $ func f
  where f (WTuple [WInt x, WInt y]) = unit $ WInt $ x+y
        f _ = unit Wrong
evalV _ (VPrim "in'>'") = WFunction $ func f
  where f (WTuple [WInt x, WInt y]) | x > y = unit $ WInt x
                                    | otherwise = empty
        f _ = unit Wrong
evalV _ _ = undefined

apply :: W -> W -> S W
apply WInt{} _ = unit Wrong
apply WTuple{} _ = unit Wrong -- XXX
apply (WFunction f) w = apFunc f w
apply Wrong _ = unit Wrong
