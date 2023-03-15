module Main where
import Verifier.FOL
import Verifier.Verify
import Rules.Core
import TRS.Bind

main :: IO ()
main =
  do putStrLn "-- PROGRAM --"
     print e2
     putStrLn "-- FORMULA --"
     let [q] = success e1
         pr  = Forall $ Bind a $ q
     putStrLn (show pr)
     b <- prove pr
     if b then
       putStrLn "==> program does not fail"
      else
       putStrLn "==> program may fail"
 where
  e0  = One (Exi $ Bind x $
              (Var x :=: Var a)
          :>: Var x)
 
 
  e1  = One (Exi $ Bind x $ Exi $ Bind y $
              (Var x :=: One ( (Var y :=: Var a :>: Var y) :|: Int 2 ))
          :>: (Var y :=: Int 1)
          :>: (Var x :=: Int 2)
          :>: Var x)

  e2  = One (Exi $ Bind x $
              (Var x :=: One ( (Op Ge :@: Arr [Var a, Int 3]) :|: Int 2 ))
          :>: (Op Ge :@: Arr [Var x, Var a])
        )

  x  = ident "x"
  y  = ident "y"
  a  = ident "input"

ifThenElse :: [Ident] -> Expr -> Expr -> Expr -> Expr
ifThenElse xs c p q =
  One ((foldr (\x -> Exi . Bind x) (c :>: Lam (Bind y p)) xs) :|: Lam (Bind y q)) :@: Arr []
 where
  y = identNotIn (free (p,q))

isNat :: Expr -> Expr
isNat e =
  Op Ge :@: Arr [e,Int 0]

f :: (Expr -> Expr) -> Expr -> Expr
f frec x = ifThenElse
            [x']
            (Var x' :=: isNat (Op Sub :@: Arr [x,Int 1]))
            (Op Mul :@: Arr [x,frec (Var x')])
            (Int 1)
 where
  x' = identNotIn (free x)

--------------------------------------------------------------------------------

