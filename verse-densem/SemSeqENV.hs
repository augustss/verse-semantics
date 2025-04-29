{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TypeOperators #-}
module SemSeqENV where
import Control.Monad

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

sem (x :=<> ys) = -- x = <y0,y1,...yn>
  [ bigUnion
    [ bigIntersect ((x %= Tup vs) : zipWith (%=) ys vs)
    | vs <- sequence (map (const univ) ys)
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

sem (y:=@@pxs) = -- y=p[x]
  case pxs of
    (Pint, [x])     -> [ bigUnion [ y %= v %/\ x %= v | v <- univInt ] ]
    (Pany, [x])     -> [ bigUnion [ y %= v %/\ x %= v | v <- univ ] ]
    (Padd, [x1,x2]) -> [ bigUnion [ y %= add v1 v2 %/\ x1 %= v1 %/\ x2 %= v2 | v1 <- univInt, v2 <- univInt ] ]
    (PLE,  [x1,x2]) -> [ bigUnion [ y %= v1 %/\ x1 %= v1 %/\ x2 %= v2 | v1 <- univInt, v2 <- univInt, v1 <= v2 ] ]
    _ -> error "bad primop use"
  where add (Int a) (Int b) = Int ((a + b) `mod` numInt)
        add _ _ = error "add"

sem (f:=\(x,op1,op2,y)) = -- f=\x.(op1){op2}(y)
  clean
  [ bigUnion
    [ (f %= Fun hs) %/\ env
    | (hs, env) <-
        combine
        [ pfuns x y env
        | env <- sem (Scope (op1 :>: op2))
        ]
    ]
  ]
 where
  combine :: [[(a,ENV)]] -> [([a],ENV)]
  combine []       = [([], univE)]
  combine (xe:xes) =
    filter ((failE /=) . snd) $
    concat
    [ [ (x:xs, env1 %/\ env2)
      , (xs,   compl env1 %/\ env2)
      ]
    | (x, env1) <- xe
    , (xs,env2) <- combine xes
    ]

  pfuns :: Ident -> Ident -> ENV -> [(Value :->? Value, ENV)]
  pfuns x y env =
    [ (mkFun (map fst vws) (\v -> head [ w | (v',w) <- vws, v'==v ]), env)
    | (vws, env) <- combine
                    [ [ ((v,w), hide [x,y] (env' %/\ (y%=w) %/\ compl (env' %/\ compl (y%=w))))
                      | let env' = env %/\ (x%=v)
                      , w <- vals y env'
                      ]
                    | v <- vals x env
                    ]
    ]

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
    [ hide zs (env %/\ env2) | env2 <- sem (Scope op2) ]
  `unionHat`
    [ env3 %\\ hide zs env   | env3 <- sem (Scope op3) ]
 where
  zs  = exis op1
  env = first zs (sem op1)

sem (All x op y) =
  [ tuples x y (sem (Scope op))
  ]

sem NoOp =
  [ univE ]

{-
sem op =
  error ("no semantics yet for '" ++ show op ++ "'")
-}

-- helper function for if

semPrim :: PrimOp -> (Value :->? Value)
semPrim Padd = fcnAdd
semPrim PLE  = fcnLE
semPrim Pint = fcnInt
semPrim Pany = fcnAny

first :: [Ident] -> [ENV] -> ENV
first _ys []         = failE
first  ys (env:envs) = env %\/ first ys [env' %\\ hide ys env | env'<-envs]

tuples :: Ident -> Ident -> [ENV] -> ENV
tuples x y envs =
  bigUnion
  [ isTuple vs y envs %/\ x%=Tup vs
  | Tup vs <- univ
  ]

isTuple :: [Value] -> Ident -> [ENV] -> ENV
isTuple []    y []         = univE
isTuple (_:_) y []         = failE
isTuple vs    y (env:envs) =
  bigUnion $
  [ hide [y] (y%=v %/\ env) %/\ isTuple vs' y envs
  | v:vs' <- [vs]
  ] ++
  [ compl (hide [y] env) %/\ isTuple vs y envs
  ]

-- helper function for lambdas
isFun :: [Value :->? Value] -> (Ident,[ENV],Ident) -> ENV
isFun [] (_,[],_) =
  univE

isFun (h:hs) (x,env:envs,y) =
  isPartialFun h (x,env,y)
    %/\ isFun hs (x,envs,y)

isFun _ _ =
  failE

isPartialFun :: (Value :->? Value) -> (Ident,ENV,Ident) -> ENV
isPartialFun h (x,env,y) =
  bigIntersect
  [ hide [x,y]
      (env %/\ x%=v %/\ y%=apply h v)
  | v <- dom h 
  ]

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
--     mapM_ printTest tests

printSem :: Oper -> IO ()
printSem op =
  do putStrLn ("> " ++ show op)
     putStrLn ("--> " ++ show (sem op))

printTest :: Test -> IO ()
printTest (op,res) = do
  let r = sem op
  when (show r /= res) $
    putStrLn $ "test failed: " ++ show op ++ " " ++ show r ++ " /= " ++ res

----------------------------------------------------------------------------------------
-- ∃h ; f=\i.(i=0){∃k_4; ∃y_5; k_4=h_1[x_3]; k_4=1; y_5=1}(y_5); res=f}




-- These are somewhat error prone.
-- If you forget, e.g., a binder for x in the semantic
-- equations, you'll get the x here.
x,y,z,w,f,g :: Ident
x = Ident "x"
y = Ident "y"
z = Ident "z"
w = Ident "w"
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
  , x:=1 :>: y:=2 :>: z:=<>[x,y]
  , All y ((x:=1):|:(x:=2)) x
  , All y ((x:=1):|:((x:=2) :|||: (x:=0))) x

  , f:=\(x,(x:=0):|:(x:=1):|:(x:=2),x:=:y,y) -- :>: y :=@ (f,x)
  , z:<=z :>: f:=\(x,x:<=x,y:=:z,y) -- :>: y :=@ (f,x)
  , f:=\(x,x:<=x,Exi z :>: y:=:z,y) -- :>: y :=@ (f,x)
  , f:=\(x,x:<=x :>: Exi z,z:=2 :>: y:=:x,y)
  ]

infix 0 -->
type Test = (Oper, String)
(-->) :: Oper -> String -> Test
(-->) = (,)

tests :: [Test]
tests =
  [ Scope (Exi y :>: Exi z :>: y:=1 :>: z:=2 :>: y:<=x :>: x:<=z)
    --> "[x=1/x=2]"
  , (x:=1):|:(x:=2)
    --> "[x=1,x=2]"
  , If(x:=1)(x:=1)(x:=2) :>: ((x:=1):|:(x:=2))
    --> "[x=1,x=2]"
  , ((x:=1):|:(x:=2)) :>: If(x:=1)(x:=1)(x:=2)
    --> "[x=1,x=2]"
  , If(x:=1)(y:=2)(y:=3) :>: ((x:=1):|:(x:=2))
    --> "[x=1;y=2,x=2;y=3]"
  , ((x:=1):|:(x:=2)) :>: If(x:=1)(y:=2)(y:=3) 
    --> "[x=1;y=2,x=2;y=3]"
  , y:<=y :>: If(((y:=1) :|: (y:=2))) (x:=1)(x:=2)
    --> "[x=1;y=1/x=1;y=2/x=2;y=0]"
  , If(Exi y :>: ((y:=1) :|: (y:=2))) (x:=:y)(x:=2)
    --> "[x=1]"
--SLOW  , x:=1 :>: y:=2 :>: z:=<>[x,y]
--    --> "[x=1;y=2;z=<1,2>]"
  , All y ((x:=1):|:(x:=2)) x
    --> "[y=<1,2>]"
  , All y ((x:=1):|:((x:=2) :|||: (x:=0))) x
    --> "[y=<1,0>/y=<1,2>]"
  , z:<=z :>: All y (x:=:z :>: x:=2) x
    --> "[y=<>;z=0/y=<>;z=1/y=<2>;z=2]"

  {-
  , f:=\(x,(x:=0):|:(x:=1):|:(x:=2),x:=:y,y) -- :>: y :=@ (f,x)
    --> "[f=<0,1,2>]"
  , f:=\(x,x:<=x,Exi z :>: y:=:z,y) -- :>: y :=@ (f,x)
    --> "[f=<0>/f=[{0->0,1->0,2->0}]/f=[{0->0,1->1}]/f=[{0->0,1->1,2->2}]/f=<1>/f=[{0->1,1->1,2->1}]/f=<2>/f=[{0->2,1->2,2->2}]]"
  , f:=\(x,x:<=x :>: Exi z,z:=2 :>: y:=:x,y)
    --> "[f=<0>/f=[{0->0,1->1}]/f=[{0->0,1->1,2->2}]]"
  -}
  ]

----------------------------------------------------------------------------------------

