{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module OpSem.Tests(module OpSem.Tests) where
import Ex
import OpSem.Comp(compExp)
import OpSem.Exp
import OpSem.Op(Value)
import OpSem.Eval(run)

ev :: Exp -> [Value]
ev = run . compExp

---------------------
--      Tests
---------------------

ok :: (Show a) => String -> a -> Exp -> Ex String
ok n r e = Ex n (Just $ show r) (show $ ev e)

bad :: String -> Exp -> Ex String
bad n e = Ex n Nothing (show $ ev e)

bug :: (Show a) => String -> a -> Exp -> Ex String
bug n _r e = Ex ("bug: " ++ n) Nothing (show $ ev e)

unimp :: (Show a) => String -> a -> Exp -> Ex String
unimp n _r e = Ex ("unimp: " ++ n) Nothing (show $ ev e)

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
  Array [1,2,3,4]

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
  Array ["x"+1, "x" := "y", "y" := "z"+1, "z" := 5]

test205 = bad "test205" $
  ("x" := 1) # ("x" := 2)

test206 = bad "test206" $
  "x"

test207 = ok "test207" [(3,4)] $
  3 # doo ("x":= 4)

test208 = bad "test208" $
  "x" # doo ("x":= 4)

-- Check that mutual recursion fails
test209 = bad "test209" $
  "x" := "y" `semi` "y" := "x"

test210 = ok "test210" [(1,(2,3))] $
  "x" := (1 # "y") `semi`
  "y" := (2 # "z") `semi`
  "z" := 3 `semi`
  "x"

test211 = bad "test211" $
  "x" := 1 `semi` "x" := 2

-- The x1 used to be x, but shadowing is not allowed
test212 = ok "test212" [(1,2)] $
  "x" := 2 `semi` (doo ("x1" `wher` "x1" := 1) # "x")

test200s :: IO ()
test200s = mapM_ testEx
  [test201,test202,test203,test204,test205,test206,test207,test208,test209,test210,test211,test212
  ]

---------------------
-- 0/1 results
---------------------

test301 = ok "test301" [(3,3)] $
  ("x" := 3) # ("x" === 3)

test302 = ok "test302" [3] $
  ("x" := 1+"y") `semi` "y" := 2 `semi` ("x" === 3)

test303 = ok "test303" [(3,3)] $
  ("x" === 3) # ("x" := 3)

test304 = ok "test304" [20] $
  ("a" := Array [10,20,30]) `semi` Sel "a" 1

test305 = ok "test305" [20] $
  Sel "a" 1 `wher` ("a" := Array [10,20,30])

test306 = ok "test306" ([]::[()]) $
  ("a" := Array [10,20,30]) `semi` Sel "a" 3

test307 = ok "test307" [(1,1)] $
  "t" := Pair 1 (Fst "t")

-- Test that when evaluating z the x is fully determined.
test308 = ok "test308" [5] $
  "x" := "y" `semi` "y" := 5 `semi` "z" := ("x"===5)

test309 = ok "test309" ([]::[()]) $
  ("x" := 3) # ("x" === 4)

test310 = ok "test310" ([]::[()]) $
  ("x" === 4) # ("x" := 3)

-- Deadlock
test311 = bad "test311" $
  "y" := iF ("z"===1) (1|||2) (3|||4) `semi` "z":= 5|||6 `semi` ("y" # "z")

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
  "x" := 1 ||| 2 `semi` "y" := ("x" === 1) `semi` ("x" # "y")

-- Fails (equalLenient)
test412 = ok "test412" [(1,1)] $
  "y" := ("x" === 1) `semi` "x" := 1 ||| 2 `semi` ("x" # "y")

-- Cascaded forward references
test413 = ok "test413" [3,7,2,2] $
  "x" := ("y" ||| 2)  `semi`
  "y" := (3 ||| "z")  `semi`
  "z" := 7            `semi`
  "x"

-- Choice under if
test414 = ok "test414" [(1,5),(1,6),(2,5),(2,6)] $
  "x" := 1 `semi`
  iF ("x" === 1) (1|||2) (3|||4) # (5|||6)

-- Choice under if, must suspend
test415 = ok "test415" [(1,5),(1,6),(2,5),(2,6)] $
  iF ("x" === 1) (1|||2) (3|||4) # (5|||6) `wher`
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
  Error

-- Generates an error, as it should
test502 = bad "test502" $
  Error `semi` 1

-- Generates an error, as it should
test503 = bad "test504" $
   (2 # Error) `semi` 1

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
  for ("x" := 1|||2|||3 `semi` "y" := 4|||5) ("x" # "y")

test604 = ok "test604" [((1,4),(2,4),(3,4)),
                        ((1,5),(2,5),(3,5))] $
  "y" := 4|||5 `semi` for ("x" := 1|||2|||3) ("x" # "y")

test605 = ok "test605" [(((1,4),(2,4),(3,4)),
                       ((1,5),(2,5),(3,5)))] $
  for ("y" := 4|||5) $ for ("x" := 1|||2|||3) ("x" # "y")

test606 = ok "test606" [(88,88),(88,99),(99,88),(99,99)] $
  for (0|||1) (88 ||| 99)

test607 = ok "test607" [(1,2,3)] $
  for ("x" := 1|||2|||"y" `semi` "y" := "z" `semi` "z" := 3) "x"

test608 = ok "test608" [(2,3,4)] $
  for ("x" := 1|||2|||3) ("y" `wher` "y" := "x" + 1)

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
      (("x1"|||"y1") `wher` ("x1" := "x" + 1 `semi` "y1" := "y" + 2))

test600s :: IO ()
test600s = mapM_ testEx
  [test601,test602,test603,test604,test605,test606,test607,test608,test609,test610
  ]

---------------------
-- Functions
---------------------

test701 = ok "test701" [5] $
  "f" := lam "v" ("v" + 1) `semi`
  AppS "f" 4

test702 = ok "test702" [11] $
  "w" := 7 `semi`
  "f" := lam "v" ("w" + "v") `semi`
  AppS "f" 4

test703 = ok "test703" [11] $
  "f" := lam "v" ("w" + "v") `semi`
  "w" := 7 `semi`
  AppS "f" 4

test704 = ok "test704" [11] $
  "f" := lam "v" ("w" + "v") `semi`
  "w" := 7 `semi`
  "y" := AppS "f" "t" `semi`
  "t" := 4 `semi`
  "y"

-- f is called before it is defined
test705 = ok "test705" [11] $
  "y" := AppS "f" "t" `semi`
  "w" := 7 `semi`
  "t" := 4 `semi`
  "f" := lam "v" ("w" + "v") `semi`
  "y"

test706 = ok "test706" [11] $
  "f" := doo ("w" := 7 `semi` lam "v" ("w" + "v")) `semi`
  "y" := AppS "f" "t" `semi`
  "t" := 4 `semi`
  "y"

-- Function defined after it is used;
-- but the call is f[e], so we deadlock
test707 = ok "test707" [11] $
  "y" := AppI "f" "t" `semi`
  "w" := 7 `semi`
  "t" := 4 `semi`
  "f" := lam "v" ("w" + "v") `semi`
  "y"

test708 = ok "test708" [10,11] $
  "f" := lam "v" ("v" ||| "v" + 1) `semi`
  AppI "f" 10

{- AppS doesn't check for single value
test709 = bad "test709" $
  "f" := lam "v" ("v" ||| "v" + 1) `semi`
  AppS "f" 10
-}

test700s :: IO ()
test700s = mapM_ testEx
  [test701,test702,test703,test704,test705,test706,test707,test708 --,test709
  ]

---------------------
-- Unification
---------------------
test801 = ok "test801" [1] $
  var "x" `semi`
  "x" === 1 `semi`
  "x"

test802 = ok "test802" [1] $
  var "x" `semi`
  ("x" # 2) === (1 # 2) `semi`
  "x"

test803 = ok "test803" [(1,2)] $
  var "x" `semi`
  var "y" `semi`
  ("x" # 2) === (1 # "y")

test804 = ok "test804" [1] $
  "f" := lam "xy" (Fst "xy" === Snd "xy") `semi`
  var "x" `semi`
  AppS "f" ("x" # 1) `semi`
  "x"

test805 = ok "test805" [6] $
  "f" := lam "xyz" ((var "x" # var "y" # var "z") === "xyz" `semi` "x" + "y" + "z") `semi`
  AppS "f" (1 # 2 # 3)

test806 = bad "test806" $
  var "x" `semi` "x"+1

test800s :: IO ()
test800s = mapM_ testEx
  [test801,test802,test803,test804,test805,test806--,test807,test808,test809
  ]

---------------------
-- Conditional
---------------------

test901 = ok "test901" [1] $
  iF (1 === 1) 1 2

test902 = ok "test902" [2] $
  iF (0 === 1) 1 2

test903 = ok "test903" [10] $
  iF ("x" := 10) "x" 2

test904 = ok "test904" [2] $
  iF Fail 1 2

test905 = ok "test905" [1] $
  iF ("x" := 1 `semi` "x" === 1) 1 2

test906 = ok "test906" [2] $
  iF ("x" := 1 `semi` "x" === 0) 1 2

test907 = ok "test907" [1] $
  iF ("x" === 1 `semi` "x" := 1) 1 2

test908 = ok "test908" [2] $
  iF ("x" === 0 `semi` "x" := 1) 1 2

test909 = ok "test909" [1] $
  "x" := 10 `semi`
  iF ("x" === 10) 1 2

test910 = ok "test910" [2] $
  "x" := 0 `semi`
  iF ("x" === 10) 1 2

test911 = ok "test911" [1] $
  "y" := iF ("x" === 10) 1 2 `semi`
  "x" := 10 `semi`
  "y"

test912 = ok "test912" [2] $
  "y" := iF ("x" === 10) 1 2 `semi`
  "x" := 0 `semi`
  "y"

test913 = ok "test913" [1] $
  iF ("x":=1) "x" 20

test914 = ok "test914" [1] $
  iF ("x" := (1 ||| 2)) 1 20

test915 = ok "test915" [2] $
  iF ("x" := (Fail ||| 2)) "x" 20

test916 = ok "test916" [1] $
  iF ("x" := (1 ||| Fail)) "x" 20

test917 = ok "test917" [20] $
  iF ("x" := (Fail ||| Fail)) "x" 20

test918 = ok "test918" [3] $
  iF ("x" := (Fail ||| (Fail ||| 3))) "x" 20

test900s :: IO ()
test900s = mapM_ testEx
  [test901,test902,test903,test904,test905,test906,test907,test908,test909,test910,test911,test912
  ,test913,test914,test915,test916,test917,test918
  ]

---------------------
-- Range
---------------------

test1001 = ok "test1001" [(1,2,3)] $
  for ("x" := Range (Array [1,2,3])) "x"

test1002 = ok "test1002" [(102,103,104)] $
  "xs" := for ("x" := 1|||2|||3) ("x" + 1) `semi`
  for ("y" := Range "xs") ("y" + 100)

test1003 = ok "test1003" [((1,2),(1,4),(1,5))] $
  "xys" := Array [1#2, 2#3, 1#4, 2#4, 1#5] `semi`
  for ("xy" := Range "xys" `semi` Fst "xy" === 1) "xy"

test1004 = ok "test1004" ([]::[()]) $
  "xys" := Array [1#2, 2#3, 1#4, 2#4, 1#5] `semi`
  for ("xy" := Range "xys" `semi` Fst "xy" === 2) ("xy" `wher` Snd "xy" === 3)

test1005 = ok "test1005" [((2,3),(2,3))] $
  "xys" := Array [1#2, 2#3, 1#4, 2#3, 1#5] `semi`
  for ("xy" := Range "xys" `semi` Fst "xy" === 2) ("xy" `wher` Snd "xy" === 3)

test1006 = ok "test1006" [(2,3)] $
  "a" := for ("x" := Range "xs") ("x" + 1) `semi`
  "xs" := Array[1,2] `semi`
  "a"

test1000s :: IO ()
test1000s = mapM_ testEx
  [test1001,test1002,test1003,test1004,test1005,test1006
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
