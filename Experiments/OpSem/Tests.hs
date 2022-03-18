{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module OpSem.Tests(module OpSem.Tests) where
import Ex
import OpSem.DSL
import OpSem.Exp(eval)
--import OpSem.Op(Value)
import OpSem.OpX(Value)
import OpSem.Eval()
import OpSem.EvalExp()

ev :: Exp -> String
ev = show . (eval :: Exp -> [Value])

---------------------
--      Tests
---------------------

ok :: (Show a) => String -> a -> Exp -> Ex String
ok n r e = Ex n (Just $ show r) (ev e)

bad :: String -> Exp -> Ex String
bad n e = Ex n Nothing (ev e)

bug :: (Show a) => String -> a -> Exp -> Ex String
bug n _r e = Ex ("bug: " ++ n) Nothing (ev e)

unimp :: (Show a) => String -> a -> Exp -> Ex String
unimp n _r e = Ex ("unimp: " ++ n) Nothing (ev e)

---------------------
-- Simple, single valued tests.
---------------------
test101 = ok "test101" [5] $
  5

test102 = ok "test102" [42] $
  5 + 37

test103 = ok "test103" [(5,37)] $
  5 # 37

test104 = ok "test104" [(1,2,3,4)] $
  array [1,2,3,4]

test100s = mapM_ testEx
  [test101,test102,test103,test104
  ]

---------------------
-- Variable scopes
---------------------
test201 = ok "test201" [(5,5)] $
  ("x" := 5) # "x"

test202 = ok "test202" [(5,5)] $
  "x" # ("x" := 5)

test203 = ok "test203" [(7,6)] $
  "x"+1 # ("x" := 6)

test204 = ok "test204" [(7,6,6,5)] $
  array ["x"+1, "x" := "y", "y" := "z"+1, "z" := 5]

test205 = bad "test205" $
  ("x" := 1) # ("x" := 2)

test206 = bad "test206" $
  "x"

test207 = ok "test207" [(3,4)] $
  3 # do_ ("x":= 4)

test208 = bad "test208" $
  "x" # do_ ("x":= 4)

-- Check that mutual recursion fails
test209 = bad "test209" $
  "x" := "y" % "y" := "x"

test210 = ok "test210" [(1,(2,3))] $
  "x" := (1 # "y") %
  "y" := (2 # "z") %
  "z" := 3 %
  "x"

test211 = bad "test211" $
  "x" := 1 % "x" := 2

-- The x1 used to be x, but shadowing is not allowed
test212 = ok "test212" [(1,2)] $
  "x" := 2 % (do_ ("x1" `where_` "x1" := 1) # "x")

test213 = ok "test213" [(1,3)] $
  "x" := 1 %
  "y" := let_ ("x" := 2) ("x" + 1) %
  ("x" # "y")

test214 = bad "test214" $
  "y" := let_ ("x" := 2) ("x" + 1) %
  ("x" # "y")

test200s :: IO ()
test200s = mapM_ testEx
  [test201,test202,test203,test204,test205,test206,test207,test208,test209,test210,test211,test212,test213,test214
  ]

---------------------
-- 0/1 results
---------------------

test301 = ok "test301" [(3,3)] $
  ("x" := 3) # ("x" === 3)

test302 = ok "test302" [3] $
  ("x" := 1+"y") % "y" := 2 % ("x" === 3)

test303 = ok "test303" [(3,3)] $
  ("x" === 3) # ("x" := 3)

test304 = ok "test304" [20] $
  ("a" := array [10,20,30]) % Sel "a" 1

test305 = ok "test305" [20] $
  Sel "a" 1 `where_` ("a" := array [10,20,30])

test306 = ok "test306" ([]::[()]) $
  ("a" := array [10,20,30]) % Sel "a" 3

test307 = ok "test307" [(1,1)] $
  "t" := Pair 1 (Fst "t")

-- Test that when evaluating z the x is fully determined.
test308 = ok "test308" [5] $
  "x" := "y" % "y" := 5 % "z" := ("x"===5)

test309 = ok "test309" ([]::[()]) $
  ("x" := 3) # ("x" === 4)

test310 = ok "test310" ([]::[()]) $
  ("x" === 4) # ("x" := 3)

-- Deadlock
test311 = bad "test311" $
  "y" := if_ ("z"===1) (1|||2) (3|||4) % "z":= 5|||6 % ("y" # "z")

test300s :: IO ()
test300s = mapM_ testEx
  [test301,test302,test303,test304,test305,test306,test307,test308,test309,test310,test311
  ]

---------------------
-- Multi-valued
---------------------

test401 = ok "test401" [1,2] $
  1 ||| 2

test402 = ok "test402" [2,3,3,4] $
  (1 ||| 2) + (1 ||| 2)

test403 = ok "test403" [2,4] $
  ("x" := 1 ||| 2) + "x"

-- Should fail, since variables in ||| do not escape
test404 = bad "test404" $
  (("x" := 1) ||| 2) + "x"

test405 = ok "test405" [(4,(1,3)),(5,(1,4)),(5,(2,3)),(6,(2,4))] $
  ("x" := 1 ||| 2) + ("y" := 3 ||| 4) # ("x" # "y")

test406 = ok "test406" [(2,(1,1)),(5,(1,4)),(4,(2,2)),(6,(2,4))] $
  ("x" := 1 ||| 2) + ("y" := "x" ||| 4) # ("x" # "y")

test407 = ok "test407" [4] $
  ("x" := 1 ||| 2) + ("x" === 2)

test408 = ok "test408" [(1,1),(2,2)] $
  Pair "x" ("x" := 1 ||| 2)

test409 = ok "test409" [(7,(1,1)),(7,(2,2)),(1,(1,1)),(2,(2,2))] $
  Pair ("y" := (7 ||| "x")) (Pair "x" ("x" := (1 ||| 2)))

-- x's value should not be delayed, because x's RHS has no depenedncies
test410 = ok "test410" [((1,7),1)] $
         Pair ("x" := (Pair 1 7 |||
                       Pair "y" ("y" := 2)))
              (Fst "x" === 1)

test411 = ok "test411" [(1,1)] $
  "x" := 1 ||| 2 % "y" := ("x" === 1) % ("x" # "y")

-- Fails (equalLenient)
test412 = ok "test412" [(1,1)] $
  "y" := ("x" === 1) % "x" := 1 ||| 2 % ("x" # "y")

-- Cascaded forward references
test413 = ok "test413" [3,7,2,2] $
  "x" := ("y" ||| 2)  %
  "y" := (3 ||| "z")  %
  "z" := 7            %
  "x"

-- Choice under if
test414 = ok "test414" [(1,5),(1,6),(2,5),(2,6)] $
  "x" := 1 %
  if_ ("x" === 1) (1|||2) (3|||4) # (5|||6)

-- Choice under if, must suspend
test415 = ok "test415" [(1,5),(1,6),(2,5),(2,6)] $
  if_ ("x" === 1) (1|||2) (3|||4) # (5|||6) `where_`
  "x" := 1

test400s :: IO ()
test400s = mapM_ testEx
  [test401,test402,test403,test404,test405,test406,test407,test408,test409,test410,test411,test412,test413,test414,test415
  ]

---------------------
-- Error/strictness
---------------------

-- Generates an error, as it should
test501 = bad "test501"
  err

-- Generates an error, as it should
test502 = bad "test502" $
  err % 1

-- Generates an error, as it should
test503 = bad "test504" $
   (2 # err) % 1

test500s :: IO ()
test500s = mapM_ testEx
  [test501,test502,test503
  ]

---------------------
-- for
---------------------

test601 = ok "test601" [(5,5,5)] $
  for (1|||2|||3) 5

test602 = ok "test602" [(1,2,3)] $
  for ("x" := 1|||2|||3) "x"

test603 = ok "test603" [((1,4),(1,5),(2,4),(2,5),(3,4),(3,5))] $
  for ("x" := 1|||2|||3 % "y" := 4|||5) ("x" # "y")

test604 = ok "test604" [((1,4),(2,4),(3,4)),
                        ((1,5),(2,5),(3,5))] $
  "y" := 4|||5 % for ("x" := 1|||2|||3) ("x" # "y")

test605 = ok "test605" [(((1,4),(2,4),(3,4)),
                       ((1,5),(2,5),(3,5)))] $
  for ("y" := 4|||5) $ for ("x" := 1|||2|||3) ("x" # "y")

test606 = ok "test606" [(88,88),(88,99),(99,88),(99,99)] $
  for (0|||1) (88 ||| 99)

x606 = for (0|||1) (88 ||| 99)

test607 = ok "test607" [(1,2,3)] $
  for ("x" := 1|||2|||"y" % "y" := "z" % "z" := 3) "x"

test608 = ok "test608" [(2,3,4)] $
  for ("x" := 1|||2|||3) ("y" `where_` "y" := "x" + 1)

test609 = ok "test609" [(1,2,3),(1,2,99),(1,99,3),(1,99,99),(99,2,3),(99,2,99),(99,99,3),(99,99,99)] $
  for ("x" := 1|||2|||3) ("x" ||| 99)

test610 = ok "test610"
            [((11,11),(21,21)),((11,11),(21,42)),((11,11),(32,21)),((11,11),(32,42))
            ,((11,42),(21,21)),((11,42),(21,42)),((11,42),(32,21)),((11,42),(32,42))
            ,((32,11),(21,21)),((32,11),(21,42)),((32,11),(32,21)),((32,11),(32,42))
            ,((32,42),(21,21)),((32,42),(21,42)),((32,42),(32,21)),((32,42),(32,42))]
            $
  for ("x" := 10|||20) $
    for ("y" := 30|||40)
      (("x1"|||"y1") `where_` ("x1" := "x" + 1 % "y1" := "y" + 2))

test600s :: IO ()
test600s = mapM_ testEx
  [test601,test602,test603,test604,test605,test606,test607,test608,test609,test610
  ]

---------------------
-- Functions
---------------------

test701 = ok "test701" [5] $
  "f" := lam "v" ("v" + 1) %
  app "f" 4

x701 =
  "f" := lam "v" ("v" + 1) %
  app "f" 4

test702 = ok "test702" [11] $
  "w" := 7 %
  "f" := lam "v" ("w" + "v") %
  app "f" 4

test703 = ok "test703" [11] $
  "f" := lam "v" ("w" + "v") %
  "w" := 7 %
  app "f" 4

test704 = ok "test704" [11] $
  "f" := lam "v" ("w" + "v") %
  "w" := 7 %
  "y" := app "f" "t" %
  "t" := 4 %
  "y"

-- f is called before it is defined
test705 = ok "test705" [11] $
  "y" := app "f" "t" %
  "w" := 7 %
  "t" := 4 %
  "f" := lam "v" ("w" + "v") %
  "y"

test706 = ok "test706" [11] $
  "f" := do_ ("w" := 7 % lam "v" ("w" + "v")) %
  "y" := app "f" "t" %
  "t" := 4 %
  "y"

-- Function defined after it is used;
test707 = ok "test707" [11] $
  "y" := app "f" "t" %
  "w" := 7 %
  "t" := 4 %
  "f" := lam "v" ("w" + "v") %
  "y"

test708 = ok "test708" [10,11] $
  "f" := lam "v" ("v" ||| "v" + 1) %
  app "f" 10


test709 = bad "test709" $
  "f" := lam "v" failure %
  appS "f" 10

test710 = ok "test710" [5] $
  "f" := (var "a" # var "b") ==> "a" + "b" %
  "f" @@ (2 # 3)

test711 = ok "test711" [(999,888,13)] $
  "f" := lam "n" (
    case_ ("n" + 1) [
      1 ==> 999,
      2 ==> 888,
      var "x" ==> "x" + 10
      ]) %
  array ["f" @@ 0, "f" @@ 1, "f" @@ 2]

test712 = ok "test712" [12] $
  "twice" := (var "f" ==> var "x" ==> "f" @@ ("f" @@ "x")) %
  "dbl" := var "x" ==> "x" + "x" %
  "twice" @@ "dbl" @@ 3

test713 = ok "test712" [16] $
  "twice" := (var "f" ==> var "x" ==> "f" @@ ("f" @@ "x")) %
  "dbl" := var "x" ==> "x" + "x" %
  "twice" @@ "twice" @@ "dbl" @@ 1

test714 = ok "test714" [(2,6,4)] $
  "map" := ((var "f" # var "xs") ==> for ("x" := range "xs") ("f" @@ "x")) %
  "inc" := lam "x" ("x" + "c") %
  "c" := 1 %
  "a" := array [1,5,3] %
  "map" @@ ("inc" # "a")

test700s :: IO ()
test700s = mapM_ testEx
  [test701,test702,test703,test704,test705,test706,test707
  ,test708,test709,test710,test711,test712,test713,test714
  ]

---------------------
-- Unification
---------------------
test801 = ok "test801" [1] $
  var "x" %
  "x" === 1 %
  "x"

test802 = ok "test802" [1] $
  var "x" %
  ("x" # 2) === (1 # 2) %
  "x"

test803 = ok "test803" [(1,2)] $
  var "x" %
  var "y" %
  ("x" # 2) === (1 # "y")

test804 = ok "test804" [1] $
  "f" := lam "xy" (Fst "xy" === Snd "xy") %
  var "x" %
  app "f" ("x" # 1) %
  "x"

test805 = ok "test805" [6] $
  "f" := lam "xyz" ((var "x" # var "y" # var "z") === "xyz" % "x" + "y" + "z") %
  app "f" (1 # 2 # 3)

test806 = bad "test806" $
  var "x" % "x"+1

test800s :: IO ()
test800s = mapM_ testEx
  [test801,test802,test803,test804,test805,test806--,test807,test808,test809
  ]

---------------------
-- Conditional
---------------------

test901 = ok "test901" [1] $
  if_ (1 === 1) 1 2
x901 =
  if_ (1 === 1) 1 2

test902 = ok "test902" [2] $
  if_ (0 === 1) 1 2

test903 = ok "test903" [10] $
  if_ ("x" := 10) "x" 2

test904 = ok "test904" [2] $
  if_ failure 1 2

test905 = ok "test905" [1] $
  if_ ("x" := 1 % "x" === 1) 1 2

test906 = ok "test906" [2] $
  if_ ("x" := 1 % "x" === 0) 1 2

test907 = ok "test907" [1] $
  if_ ("x" === 1 % "x" := 1) 1 2

test908 = ok "test908" [2] $
  if_ ("x" === 0 % "x" := 1) 1 2

test909 = ok "test909" [1] $
  "x" := 10 %
  if_ ("x" === 10) 1 2

test910 = ok "test910" [2] $
  "x" := 0 %
  if_ ("x" === 10) 1 2

test911 = ok "test911" [1] $
  "y" := if_ ("x" === 10) 1 2 %
  "x" := 10 %
  "y"

test912 = ok "test912" [2] $
  "y" := if_ ("x" === 10) 1 2 %
  "x" := 0 %
  "y"

test913 = ok "test913" [1] $
  if_ ("x":=1) "x" 20

test914 = ok "test914" [1] $
  if_ ("x" := (1 ||| 2)) 1 20

test915 = ok "test915" [2] $
  if_ ("x" := (failure ||| 2)) "x" 20

test916 = ok "test916" [1] $
  if_ ("x" := (1 ||| failure)) "x" 20

test917 = ok "test917" [20] $
  if_ ("x" := (failure ||| failure)) "x" 20

test918 = ok "test918" [3] $
  if_ ("x" := (failure ||| (failure ||| 3))) "x" 20

test919 = ok "test919" [7] $
  "f" := lam "n" (if_ ("n" <=. 0) ("n"+1) ("n"+2)) %
  "r" := "f" `app` "five" %
  "five" := 5 %
  "r"

test920 = ok "test920" [6] $
  "f" := lam "n" (if_ ("n" <=. 10) ("n"+1) ("n"+2)) %
  "r" := "f" `app` "five" %
  "five" := 5 %
  "r"

test900s :: IO ()
test900s = mapM_ testEx
  [test901,test902,test903,test904,test905,test906,test907,test908,test909,test910,test911,test912
  ,test913,test914,test915,test916,test917,test918,test919,test920
  ]

---------------------
-- range
---------------------

test1001 = ok "test1001" [(1,2,3)] $
  for ("x" := range (array [1,2,3])) "x"

test1002 = ok "test1002" [(102,103,104)] $
  "xs" := for ("x" := 1|||2|||3) ("x" + 1) %
  for ("y" := range "xs") ("y" + 100)

test1003 = ok "test1003" [((1,2),(1,4),(1,5))] $
  "xys" := array [1#2, 2#3, 1#4, 2#4, 1#5] %
  for ("xy" := range "xys" % Fst "xy" === 1) "xy"

test1004 = ok "test1004" ([]::[()]) $
  "xys" := array [1#2, 2#3, 1#4, 2#4, 1#5] %
  for ("xy" := range "xys" % Fst "xy" === 2) ("xy" `where_` Snd "xy" === 3)

test1005 = ok "test1005" [((2,3),(2,3))] $
  "xys" := array [1#2, 2#3, 1#4, 2#3, 1#5] %
  for ("xy" := range "xys" % Fst "xy" === 2) ("xy" `where_` Snd "xy" === 3)

test1006 = ok "test1006" [(2,3)] $
  "a" := for ("x" := range "xs") ("x" + 1) %
  "xs" := array[1,2] %
  "a"

test1007 = ok "test1007" [2] $
  if_ (range (array [])) 1 2

test1000s :: IO ()
test1000s = mapM_ testEx
  [test1001,test1002,test1003,test1004,test1005,test1006,test1007
  ]

---------------------
-- Arithmetic, comparisons
---------------------

test1011 = ok "test1011" [10] $
  6 + 4

test1012 = ok "test1012" [2] $
  6 - 4

test1013 = ok "test1013" [24] $
  6 * 4

test1014 = ok "test1014" [1] $
  6 `div` 4

test1015 = ok "test1015" ([]::[()]) $
  6 `div` 0

test1016 = ok "test1016" [2] $
  if_ (6 <. 4) 1 2

test1017 = ok "test1017" [1] $
  if_ (2 <. 4) 1 2

test1018 = ok "test1018" [120] $
  "fac" := lam "n" (if_ ("n" <=. 0) 1 ("n" * "fac" `app` ("n" - 1))) %
  "fac" `app` 5

test1019 = ok "test1019" [120] $
  "fac" := lam "n" (if_ ("n" <=. 0) "one" ("n" * "fac" `app` ("n" - 1))) %
  "res" := "fac" `app` 5 %
  "one" := 1 %
  "res"

test1020 = ok "test1020" [120] $
  "res" := "fac" `app` "five" %
  "fac" := lam "n" (if_ ("n" <=. 0) "one" ("n" * "fac" `app` ("n" - 1))) %
  "five" := 5 %
  "one" := 1 %
  "res"

test1010s :: IO ()
test1010s = mapM_ testEx
  [test1011,test1012,test1013,test1014,test1015,test1016,test1017,test1018,test1019,test1020
  ]

--------

testAll :: IO ()
testAll = do
  test100s
  test200s
  test300s
  test400s
  test500s
  test600s
  test700s
  test800s
  test900s
  test1000s
  test1010s
