{-# LANGUAGE LambdaCase #-}
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
allW = ints ++ tuples ++ fcns ++ [Wrong]
  where
    ints = [WInt i | i <- [0 .. 3]]
    tuples = [WTuple []] ++ [WTuple [w1,w2] | w1 <- ints, w2 <- ints]
    fcns = map WFunction $ [wAdd, wGt, wId, wInc, wGt0] ++
                           [ func (const (unit i)) | i <- ints ]
    wId = func $ unit
    wInc = func $ \case WInt x -> unit (WInt (x+1)); _ -> empty
    wGt0 = func $ \case WInt x | x > 0 -> unit (WInt x); _ -> empty

newtype Func a b = Func (a -> b)
func :: (a -> b) -> Func a b
func f = Func f
apFunc :: Func a b -> a -> b
apFunc (Func f) a = f a

instance Eq (Func W (S W)) where
  f == g = and [eqs (apFunc f w) (apFunc g w) | w <- allW ]
    where
      eqs ws1 ws2 | length ws1 == length ws2 = and $ zipWith eq ws1 ws2
                  | otherwise = False
      eq WFunction{} WFunction{} = True  -- pretend all function are equal
      eq w1 w2 = w1 == w2

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
evalV _ (VPrim "in'+'") = WFunction wAdd
evalV _ (VPrim "in'>'") = WFunction wGt
evalV _ _ = undefined

wAdd :: Func W (S W)
wAdd = func f
  where f (WTuple [WInt x, WInt y]) = unit $ WInt $ x+y
        f _ = empty

wGt :: Func W (S W)
wGt = func f
  where f (WTuple [WInt x, WInt y]) | x > y = unit $ WInt x
        f _ = empty

apply :: W -> W -> S W
apply WInt{} _ = unit Wrong
apply WTuple{} _ = unit Wrong -- XXX
apply (WFunction f) w = apFunc f w
apply Wrong _ = unit Wrong
