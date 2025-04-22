{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import Data.List( union )

import Dom
import ENV
import Oper

----------------------------------------------------------------------------------------

-- dodgy union-hat
unionHat :: [ENV] -> [ENV] -> [ENV]
envs1        `unionHat` []           = envs1
[]           `unionHat` envs2        = envs2
(env1:envs1) `unionHat` (env2:envs2) = (env1 %\/ env2) : (envs1 `unionHat` envs2)

bigUnionHat :: [ [ENV] ] -> [ENV]
bigUnionHat = foldr unionHat []

----------------------------------------------------------------------------------------

sem :: Oper -> [ENV]
sem (x := k) =
  [ x %= Int k ]
     
sem (x :=: y) =
  [ bigUnion [ (x %= v) %/\ (y %= v)
             | v <- univ
             ]
  ]

sem (x :<= y) =
  [ bigUnion [ (x %= v) %/\ (y %= w)
             | v@(Int a) <- univ
             , w@(Int b) <- univ
             , a<=b
             ]
  ]

sem (Exi _x) =
  [ univE ]

sem (y:=@(f,x)) = -- y=f[x]
  bigUnionHat
  [ [ (f %= Fun hs) %/\
       bigUnion [ (x %= v) %/\ (y %= apply h v) | v <- dom h ]
    | h <- hs
    ] 
  | Fun hs <- univ
  ]

sem (f:=\(x,op1,op2,y)) = -- f=\x.(op1){op2}(y)
  clean
  [ bigUnion
    [ (f %= Fun hs) %/\ isFun hs (x,zs,sem op1,bigUnion (sem (Scope op2)),y)
    | Fun hs <- univ
    ]
  ]
 where
  zs = exis op1

sem (op1 :|: op2) =
  sem op1 ++ sem op2

sem (op1 :>: op2) =
  clean
  [ env1 %/\ env2
  | env1 <- sem op1
  , env2 <- sem op2
  ]

sem Fail =
  []
  
sem (Scope op) =
  [ hide zs env
  | env <- sem op
  ]
 where
  zs = exis op
  
sem (If op1 op2 op3) =
  clean $
    [ hide zs (env %/\ env2) | env2 <- sem op2 ]
  `unionHat`
    [ env3 %\\ hide zs env   | env3 <- sem op3 ]
 where
  zs  = exis op1
  env = first zs (sem op1)

{-
sem op =
  error ("no semantics yet for '" ++ show op ++ "'")
-}

-- helper function for if

first :: [Ident] -> [ENV] -> ENV
first _ys []         = failE
first  ys (env:envs) = env %\/ first ys [env' %\\ hide ys env | env'<-envs]

-- helper function for lambdas
isFun :: [Value :->? Value] -> (Ident,[Ident],[ENV],ENV,Ident) -> ENV
isFun [] (_,_,[],_,_) =
  univE

isFun (h:hs) (x,zs,env1:envs1,env2,y) =
  isPartialFun h (x,zs,env1,env2,y)
    %/\ isFun hs (x,zs,envs1,env2,y)

isFun _ _ =
  failE

isPartialFun :: (Value :->? Value) -> (Ident,[Ident],ENV,ENV,Ident) -> ENV
isPartialFun h (x,zs,env1,env2,y) =
  bigIntersect
  [ hide ([x,y] `union` zs)
      (env1 %/\ env2 %/\ x%=v %/\ y%=apply h v)
  | v <- dom h 
  ]

  %/\

  hide ([x,y] `union` zs)
    (compl (env1 %/\ env2 %/\ compl (bigUnion [ x%=v %/\ y%=apply h v | v <- dom h ])))

{-
  -- 


  -- domain of h is accepted by env1
  bigIntersect [ hide (x:zs) (env1 %/\ (x %= v)) | v <- dom h ]
  :/\
  -- anything outside domain of h is not accepted by env1
  compl (bigUnion [ hide (x:zs) (env1 %/\ (x %= v)) | v <- univ, v `notElem` dom h ])
  :/\
  -- 


  
  :/\ forAll x (dom h) (unique y 



  | dom h == vals x env1 =  
  | otherwise            = failE
-}

----------------------------------------------------------------------------------------

main :: IO ()
main =
  do putStrLn ("univ = " ++ show univ)
     putStrLn ""
     sequence_ [ printSem e >> putStrLn "" | e <- examples ]

printSem :: Oper -> IO ()
printSem op =
  do putStrLn ("> " ++ show op)
     putStrLn ("--> " ++ show (sem op))

----------------------------------------------------------------------------------------

-- These are somewhat error prone.
-- If you forget, e.g., and binder for x in the semantic
-- equations, you'll get the x here.
x,y,z,f,g :: Ident
x = Ident "x"
y = Ident "y"
z = Ident "z"
f = Ident "f"
g = Ident "g"

examples :: [Oper]
examples =
  [ Scope $ Exi y :>: Exi z :>: y:=1 :>: z:=2 :>: y:<=x :>: x:<=z
  , (x:=1):|:(x:=2)
  , If(x:=1)(x:=1)(x:=2) :>: ((x:=1):|:(x:=2))
  , ((x:=1):|:(x:=2)) :>: If(x:=1)(x:=1)(x:=2)
  , If(x:=1)(y:=2)(y:=3) :>: ((x:=1):|:(x:=2))
  , ((x:=1):|:(x:=2)) :>: If(x:=1)(y:=2)(y:=3) 
  , y:<=y :>: If(((y:=1) :|: (y:=2))) (x:=1)(x:=2)
  , If(Exi y :>: ((y:=1) :|: (y:=2))) (x:=:y)(x:=2)

  , f:=\(x,(x:=1):|:(x:=2):|:(x:=3),x:=:y,y) -- :>: y :=@ (f,x)
  , z:<=z :>: f:=\(x,x:<=x,y:=:z,y) -- :>: y :=@ (f,x)
  , f:=\(x,x:<=x,Exi z :>: y:=:z,y) -- :>: y :=@ (f,x)
  , f:=\(x,x:<=x :>: Exi z,z:=3 :>: y:=:x,y)
  ]

----------------------------------------------------------------------------------------
