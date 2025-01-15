module Main where

import Test.QuickCheck
import Data.List

--------------------------------------------------------------------------------
-- programs

type Var
  = String

vars :: [Var]
vars = ["x","y","z"] -- ,"v","w","u"]

data Val
  = O | I | Var Var
 deriving ( Eq, Ord )

instance Show Val where
  show O       = "O"
  show I       = "I"
  show (Var v) = v

data Program
  = Val Val
  | Var :=: Val
  | Program :>: Program
  | Program :|: Program
  | If Var Program Program
 deriving ( Eq, Ord )

instance Show Program where
  show (Val a)    = show a
  show (v :=: a)  = v ++ "=" ++ show a
  show (p :>: q)  = showSeq [p,q]
  show (p :|: q)  = show1 p ++ " | " ++ show1 q
  show (If v p q) = "if (" ++ v ++  "=I) then " ++ show p ++ " else " ++ show q

show1 p | parens p  = "(" ++ show p ++ ")"
        | otherwise = show p
 where
  parens (Val a) = False
  parens _       = True

showSeq ((p:>:q):ps) = showSeq (p:q:ps)
showSeq [p]          = show1 p
showSeq (p:ps)       = show1 p ++ "; " ++ showSeq ps
showSeq []           = ""

genProgram :: Int -> [Var] -> Gen Program
genProgram n vs =
  frequency $
  [ (1, do a <- genVal
           return (Val a))
  , (1, do v <- elements vs
           a <- genVal
           return (v :=: a))
  , (n, do p <- genProgram n2 vs
           q <- genProgram n2 vs
           return (p :>: q))
  , (n, do p <- genProgram n2 vs
           q <- genProgram n2 vs
           return (p :|: q))
  , (n, do v <- elements vs
           p <- genProgram n2 vs
           q <- genProgram n2 vs
           return (If v p q))
  ]
 where
  n2 = n `div` 2

genVal :: Gen Val
genVal = oneof [ elements [O,I], elements [ Var v | v <- vars ] ]

instance Arbitrary Program where
  arbitrary = sized $ \n -> genProgram n vars
  
  shrink (Val a)    = [ Val a' | a' <- shrink a ]
  shrink (v :=: a)  = Val a : [ v :=: a' | a' <- shrink a ]
  shrink (p :>: q)  = [p,q] ++ [ p' :>: q | p' <- shrink p ] ++ [ p :>: q' | q' <- shrink q ]
  shrink (p :|: q)  = [p,q] ++ [ p' :|: q | p' <- shrink p ] ++ [ p :|: q' | q' <- shrink q ]
  shrink (If v p q) = [p,q]
                   ++ [ If v p' q | p' <- shrink p ]
                   ++ [ If v p q' | q' <- shrink q ]

instance Arbitrary Val where
  arbitrary = genVal

  shrink I       = []
  shrink O       = [I]
  shrink (Var v) = [O,I]

--------------------------------------------------------------------------------
-- semantic sequences

data Seq a
  = a :- Seq a
  | Nil
  | WRONG
 deriving ( Eq, Ord, Show ) 

mkSeq :: [[a]] -> Seq a
mkSeq []       = Nil
mkSeq ([] :qs) = mkSeq qs
mkSeq ([a]:qs) = a :- mkSeq qs
mkSeq _        = WRONG

(+++) :: Seq a -> Seq a -> Seq a
(x :- xs) +++ ys = x :- (xs +++ ys)
Nil       +++ ys = ys
WRONG     +++ _  = WRONG

--------------------------------------------------------------------------------
-- denotational semantics

data L = L | R
  deriving ( Eq, Ord, Show )

type Env = [(Var,Int)]

type Set a = [a]

den :: Program -> Env -> Set ([L],Int)
den (Val (Var v)) env = [ ([], a) | Just a <- [lookup v env] ]
den (Val a)       env = [ ([], if a==I then 1 else 0) ]
den (v :=: Var w) env = [ ([], a) | Just a <- [lookup v env], lookup w env == Just a ]
den (v :=: a)     env = [ ([], if a==I then 1 else 0) | lookup v env == Just (if a==I then 1 else 0) ]
den (p :>: q)     env = [ (l1++l2, b) | (l1,_) <- den p env, (l2,b) <- den q env ]
den (p :|: q)     env = [ (L:l, a) | (l,a) <- den p env ]
                     ++ [ (R:l, b) | (l,b) <- den q env ]
den (If v p q) env
  | lookup v env == Just 1 = [ (L:l, a) | (l,a) <- den p env ]
  | otherwise              = [ (R:l, b) | (l,b) <- den q env ]
  
sem :: Program -> Seq Int
sem p = mkSeq
      . map (nub . map snd)
      . groupBy (\v w -> fst v == fst w)
      . sort
      . concat
      $ [ den p env | env <- envs vars ]
 where
  envs []     = [[]]
  envs (v:vs) = [ (v,a):env | env <- envs vs, a <- [0,1] ]

--------------------------------------------------------------------------------
-- operational semantics

type Heap = [([Var],Maybe Int)]

run :: Program -> Heap -> Seq Int
run p heap =
  case step p heap of
    New p' heap' -> run p' heap'
    Fail         -> Nil
    Stuck        ->
      case p of
        Val a ->
          case valueOf a heap of
            Just b  -> b :- Nil
            Nothing -> WRONG
        _     -> expand p []
 where
  valueOf (Var x) heap = head $ [ ma | (vs,ma) <- heap, x `elem` vs ] ++ [ Nothing ]
  valueOf I       _    = Just 1
  valueOf O       _    = Just 0

  expand (p :>: q) rs = expand p (q:rs)
  expand (p :|: q) rs = run (p >>> rs) heap +++ run (q >>> rs) heap
  expand _         _  = WRONG

  p >>> []     = p
  p >>> (q:qs) = (p :>: q) >>> qs

data Step
  = New Program Heap
  | Stuck
  | Fail
 deriving ( Eq, Ord, Show )

step :: Program -> Heap -> Step
step (Val v) heap =
  Stuck

step (x :=: v) heap =
  case unify x v heap of
    Just heap' -> New (Val v) heap'
    Nothing    -> Fail

step (Val _ :>: q) heap =
  New q heap

step (p :>: q) heap =
  case step p heap of
    Fail         -> Fail
    New p' heap' -> New (p' :>: q) heap'
    Stuck        -> case step q heap of
                      Fail         -> Fail
                      New q' heap' -> New (p :>: q') heap'
                      Stuck        -> Stuck

step (p :|: q) heap =
  case step p heap of
    Fail   -> step q heap
    step_p -> case step q heap of
                Fail -> step_p
                _    -> Stuck

step (If v p q) heap =
  case look v of
    Just 1 -> step p heap
    Just 0 -> step q heap
    _      -> Stuck
 where
  look v = head $ [ ma | (vs,ma) <- heap, v `elem` vs ] ++ [Nothing]

unify :: Var -> Val -> Heap -> Maybe Heap
unify x a heap =
  case (look (Var x), look a) of
    ((xs,Nothing),(ys,mb))           -> yes xs ys mb
    ((xs,ma),(ys,Nothing))           -> yes xs ys ma
    ((xs,Just a),(ys,Just b)) | a==b -> yes xs ys (Just a)
    _                                -> Nothing
 where
  look O       = ([],Just 0)
  look I       = ([],Just 1)
  look (Var v) = head $ [(vs,ma)| (vs,ma) <- heap, v `elem` vs]++[([v],Nothing)]
  
  yes xs ys ma = Just
               $ (nub (xs++ys),ma)
               : [ (vs,mb)
                 | (vs,mb) <- heap
                 , all (`notElem` xs) vs
                 , all (`notElem` ys) vs
                 ]

--------------------------------------------------------------------------------
-- property

main :: IO ()
main = quickCheckWith stdArgs{ maxSuccess = 9999999 } prop1

prop1 p =
 let seq1 = run p []
     seq2 = sem p
  in whenFail (do putStrLn ("run: " ++ show seq1)
                  putStrLn ("den: " ++ show seq2)) $
       seq1 ~<~ seq2

(a :- p) ~<~ (b :- q) = a == b && (p ~<~ q)
Nil      ~<~ Nil      = True
WRONG    ~<~ _        = True
_        ~<~ _        = False

--------------------------------------------------------------------------------

