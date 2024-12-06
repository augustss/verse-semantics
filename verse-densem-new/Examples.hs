module Examples where
import System.IO
import Exp

infix 0 ===

type Example = (Exp, String)

(===) :: Exp -> String -> Example
(===) = (,)

runExamples :: (Show a) => (Exp -> a) -> [Example] -> IO ()
runExamples eval = mapM_ (runExample eval)

runExample :: (Show a) => (Exp -> a) -> Example -> IO ()
runExample eval (e, r) = do
  putStr (show e); hFlush stdout
  let r' = eval e
      r'' = show r'
  putStrLn $ "   === " ++ show r'
  if r == r'' then
    return ()
   else
    putStrLn $ "*** ERROR ***\n" ++
               "eval " ++ show e ++ "\n" ++
               "  = " ++ show r'' ++ "\n" ++
               " /= " ++ show r

--------------------
---- Examples

cint :: Exp
cint = Colon (Prim Oint)

-- x:=2; y:=1; add[(x,y)]
exp1 :: Example
exp1 = Def "x" (Int 2) `Seq` Def "y" (Int 1) `Seq` (App (Prim Oadd) (Tup [Var "x", Var "y"]))
     === "3"

-- fun_c(x:int){x}
exp2 :: Example
exp2 = Fun Closed (Def "x" (Colon (Var "int"))) (Var "x")
     === "int"

-- fun_o(x:int){x}
exp3 :: Example
exp3 = Fun Open (Def "x" (Colon (Var "int"))) (Var "x")
     === "Wrong[comparable,int]"

-- fun_c(x:int){add[(x,1)]}
exp4 :: Example
exp4 = Fun Closed (Def "x" (Colon (Var "int"))) (App (Prim Oadd) (Tup [Var "x", Int 1]))
     === "succ"

exp5 :: Example
exp5 = App (fst exp4) (Int 2)
     === "3"

exp6 :: Example
exp6 = App (fst exp3) (Int 1)
     === "1"

-- fun_c(f := fun_c(:int){:int}){f[1]}
exp7 :: Example
exp7 = Fun Closed arg (App (Var "f") (Int 1))
     === "ho1"
  where arg = Def "f" (Fun Closed cint cint)

exp8 :: Example
exp8 = App (fst exp7) (Var "succ")
     === "2"

exp9 :: Example
exp9 = App (fst exp7) (Var "int")
     === "1"

exp10 :: Example
exp10 = App (fst exp7) (fst exp4)
      === "2"

-- fun_c(f := fun_c(:succ){:int}){f[1]}
exp11 :: Example
exp11 = Fun Closed arg (App (Var "f") (Int 1))
      === "ho2"
  where arg = Def "f" (Fun Closed csucc cint)
        csucc = Colon (Var "succ")

exp12 :: Example
exp12 = App (fst exp11) (Var "int")
      === "2"

-- Should fail, function domain not large enough.
-- ex7[fun_c(0){0}]
exp13 :: Example
exp13 = App (fst exp7) (Fun Closed (Int 0) (Int 0))
      === "Wrong[]"

-- Should fail, function domain not large enough,
-- even though it handles the f[1].
-- ex7[fun_c(1){1}]
exp14 :: Example
exp14 = App (fst exp7) (Fun Closed (Int 1) (Int 1))
      === "Wrong[]"

exp15 :: Example
exp15 = App (fst exp7) (Fun Closed (Colon (Var "int")) (Int 0))
      === "0"

exp16 :: Example
exp16 = App (fst exp11) (Fun Closed (Colon (Var "int")) (Int 0))
      === "0"

-- fun_c(f := fun_c(:int){:succ}){f[1]}
exp17 :: Example
exp17 = Fun Closed arg (App (Var "f") (Int 1))
      === "ho3"
  where arg = Def "f" (Fun Closed cint csucc)
        csucc = Colon (Var "succ")

exp18 :: Example
exp18 = App (fst exp17) (Var "int")
      === "2"

exp19 :: Example
exp19 = App (fst exp17) (Fun Closed (Colon (Var "int")) (Int 0))
      === "1"

-- if (1=1){2}else{0}
exp20 :: Example
exp20 = If (Int 1 `Equ` Int 1) (Int 2) (Int 0)
      === "2"

-- if (1=3){2}else{0}
exp21 :: Example
exp21 = If (Int 1 `Equ` Int 3) (Int 2) (Int 0)
      === "0"

-- if (x:int){x}{999} = 3
exp22 :: Example
exp22 = If (Def "x" (Colon (Var "int"))) (Var "x") (Int 999) `Equ` Int 3
      === "3"

exp23 :: Example
exp23 = All (Choice (Int 1) (Int 2))
      === "[1,2]"

exp24 :: Example
exp24 = All (Colon $ Tup [Int 2, Int 3])
      === "[2,3]"

-- fun_c(x:=(0|1)){x}
--  denotation id01LR = { [0->L0, 1->R1] }
exp25 :: Example
exp25 = Fun Closed (Def "x" (Choice (Int 0) (Int 1))) (Var "x")
      === "id01LR"

exp26 :: Example
exp26 = All (Colon (fst exp25))
      === "[0,1]"

-- fun_c(x:=(1|0)){x}
--  denotation id01RL = { [0->R0, 1->L1] }
exp27 :: Example
exp27 = Fun Closed (Def "x" (Choice (Int 1) (Int 0))) (Var "x")
      === "id01RL"

exp28 :: Example
exp28 = All (Colon (fst exp27))
      === "[1,0]"

-- if (1 | 2){2}else{0}
exp29 :: Example
exp29 = If (Int 1 `Choice` Int 2) (Int 2) (Int 0)
      === "2"

-- fun_c(0){1|2}
exp30 :: Example
exp30 = Fun Closed (Int 0) (Int 1 `Choice` Int 2)
      === "Wrong"

-- all{exp30[0]}
exp31 :: Example
exp31 = All (App (fst exp30) (Int 0))
      === "[1,2]"

-- fun_c(y:=1|2; 0) := if (y = 1) then (1, :int) else (:int, 2)
exp32 :: Example
exp32 = Fun Closed (Def "y" (Choice (Int 1) (Int 2)) `Seq` Int 0)
                   (If (Var "y" `Equ` Int 1) (Tup [Int 1, cint]) (Tup [cint, Int 2]))
      === "f0t12"

-- (1, :int) = (:int, 2)
exp33 :: Example
exp33 = Tup [Int 1, cint] `Equ` Tup [cint, Int 2]
      === "[1,2]"

-- fun_c(x:=:int; :int){0}
exp34 :: Example
exp34 = Fun Closed (Def "x" cint `Seq` cint) (Int 0)
      === "const0"

-- fun_c((x:=a; x:int) where a:int){x}
exp35 :: Example
exp35 = Fun Closed (((Var "x" `Equ` Var "a") `Seq` Def "x" cint) `Where` Def "a" cint) (Var "x")
      === "int"

-- fun_c(a:=0|1; x:=a){x}
exp36 :: Example
exp36 = Fun Closed ((Def "a" (Int 0 `Choice` Int 1)) `Seq` (Def "x" (Var "a"))) (Var "x")
      === "XXX2"

-- fun_c(x:=0|1|2){x}
exp37 :: Example
exp37 = Fun Closed (Def "x" (Int 0 `Choice` Int 1 `Choice` Int 2)) (Var "x")
      === "XXX3"

-- fun_c(x:=3|1|0){x}
exp38 :: Example
exp38 = Fun Closed (Def "x" (Int 3 `Choice` Int 1 `Choice` Int 0)) (Var "x")
      === "XXX4"

-- fun_c(x:=0|1|2){x} = fun_c(x:=3|1|0){x}
-- denotation {}
exp39 :: Example
exp39 = fst exp37 `Equ` fst exp38
      === "XXX5"

-- fun_c(a:=0|1; x:=if(a=0)(0|1|2)else(3|1|0)){x}
-- 0->L,LL0, 1->L,RL1, 2->L,R2, 3->R,LL3, 1->R,RL1, 0->R,R0
exp40 :: Example
exp40 = Fun Closed (Def "a" (Int 0 `Choice` Int 1) `Seq`
                    Def "x" (If (Var "a" `Equ` Int 0)
                                (Int 0 `Choice` Int 1 `Choice` Int 2)
                                (Int 3 `Choice` Int 1 `Choice` Int 0)))
                   (Var "x")
      === "XXX6"

exp43 :: Example
exp43 = Def "x" (Int 1 `Choice` Int 2) `Seq` If (Var "x" `Equ` Int 1) (Int 0 `Choice` Int 1) (Int 2 `Choice` Int 1 `Choice` Int 0)
      === "XXX7"

exp44 :: Example
exp44 = If (Var "x" `Equ` Int 1) (Int 0 `Choice` Int 1) (Int 2 `Choice` Int 1 `Choice` Int 0) `Seq` Def "x" (Int 1 `Choice` Int 2)
      === "XXX7"

-- fun_c(x:=1){x}
exp45 :: Example
exp45 = Fun Closed (Def "x" (Int 1)) (Var "x")
      === "id1"

-- fun_c(x:int){x=1}
exp46 :: Example
exp46 = Fun Closed (Def "x" (Colon (Var "int"))) (Var "x" `Equ` Int 1)
      === "Wrong[]"

-- f:=fun_c(fun_c(0){1}){2}; h:= :any; f[h]; h
exp47 :: Example
exp47 = --Def "f" (Fun Closed (Fun Closed (Int 0) (Int 1)) (Int 2)) `Seq`
        Def "h" (Colon (Var "any")) `Seq`
        App {-(Var "f")-}f (Var "h") `Seq`
        Var "h"
      === "succ0"
  where f = Fun Closed (Fun Closed (Int 0) (Int 1)) (Int 2)

exp48 :: Example
exp48 = App (fst exp47) (Int 0)
      === "1"

-- f:=fun_c(fun_o(0){1}){2}; h:= :any; f[h]; h
exp49 :: Example
exp49 = --Def "f" (Fun Closed (Fun Closed (Int 0) (Int 1)) (Int 2)) `Seq`
        Def "h" (Colon (Var "any")) `Seq`
        App {-(Var "f")-}f (Var "h") `Seq`
        Var "h"
      === "Wrong[const1,succ,succ0]"
  where f = Fun Closed (Fun Open (Int 0) (Int 1)) (Int 2)

exp50 :: Example
exp50 = App (fst exp49) (Int 0)
      === "1"

-- fun_x(x~>y:= :succ) := x
exp51 :: Example
exp51 = Fun Closed (Def2 "x" "y" (Colon (Var "succ"))) (Var "x")
      === "int"

-- fun_x(x~>y:= :succ) := y
exp52 :: Example
exp52 = Fun Closed (Def2 "x" "y" (Colon (Var "succ"))) (Var "y")
      === "succ"

exp53 :: Example
exp53 = Fun Open (Def "x" cint) (Var "x")
      === "Wrong[comparable,int]"

exp54 :: Example
exp54 = Fun Closed (Int 0) (fst exp53)
      === "Wrong[ho6,ho7]"

exp55 :: Example
exp55 = App (App (fst exp54) (Int 0)) (Int 2)
      === "2"
