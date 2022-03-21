{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module OpSem.Tests(module OpSem.Tests) where
import Ex
import OpSem.DSL
import OpSem.Exp(eval, evalMany)
import OpSem.OpX(Value)
import OpSem.EvalExp()

ev :: Exp -> String
ev = show . (eval :: Exp -> Value)

evm :: Exp -> String
evm = show . (evalMany :: Exp -> [Value])

---------------------
--      Tests
---------------------

ok :: (Show a) => String -> a -> Exp -> Ex String
ok nm rr e = Ex nm (Just $ show rr) (ev e)

okm :: (Show a) => String -> a -> Exp -> Ex String
okm nm rr e = Ex nm (Just $ show rr) (evm e)

bad :: String -> Exp -> Ex String
bad nm e = Ex nm Nothing (ev e)

bug :: (Show a) => String -> a -> Exp -> Ex String
bug nm _r e = Ex ("bug: " ++ nm) Nothing (ev e)

unimp :: (Show a) => String -> a -> Exp -> Ex String
unimp nm _r e = Ex ("unimp: " ++ nm) Nothing (ev e)

---------------------
-- Some variables to make the tests look nicer.
---------------------
x, y, z, x1, y1, xs, xy, xys, xyz, a, b, c, f, i, r, t, n, v, w
  , twice, dbl, fac, res, one, five, map_, inc :: Exp
x = "x"
x1 = "x1"
xs = "xs"
xy = "xy"
xys = "xys"
xyz = "xyz"
y = "y"
y1 = "y1"
z = "z"
a = "a"
b = "b"
c = "c"
f = "f"
i = "i"
n = "n"
t = "t"
r = "r"
v = "v"
w = "w"
twice = "twice"
dbl = "dbl"
fac = "fac"
res = "res"
one = "one"
five = "five"
map_ = "map"
inc = "inc"

---------------------
-- Simple, single valued tests.
---------------------
test101 = ok "test101" 5 $
  5

test102 = ok "test102" 42 $
  5 + 37

test103 = ok "test103" (5,37) $
  5 # 37

test104 = ok "test104" (1,2,3,4) $
  array [1,2,3,4]

test100s = mapM_ testEx
  [test101,test102,test103,test104
  ]

---------------------
-- Variable scopes
---------------------
test201 = ok "test201" (5,5) $
  (x := 5) # x

test202 = ok "test202" (5,5) $
  x # (x := 5)

test203 = ok "test203" (7,6) $
  x+1 # (x := 6)

test204 = ok "test204" (7,6,6,5) $
  array [x+1, x := y, y := z+1, z := 5]

test205 = bad "test205" $
  (x := 1) # (x := 2)

test206 = bad "test206" $
  x

test207 = ok "test207" (3,4) $
  3 # do_ (x:= 4)

test208 = bad "test208" $
  x # do_ (x:= 4)

-- Check that mutual recursion fails
test209 = bad "test209" $
  x := y % y := x

test210 = ok "test210" (1,(2,3)) $
  x := (1 # y) %
  y := (2 # z) %
  z := 3 %
  x

test211 = bad "test211" $
  x := 1 %
  x := 2

-- The x1 used to be x, but shadowing is not allowed
test212 = ok "test212" (1,2) $
  x := 2 % (do_ (x1 `where_` x1 := 1) # x)

test213 = ok "test213" (1,3) $
  x := 1 %
  y := let_ (x := 2) (x + 1) %
  (x # y)

test214 = bad "test214" $
  y := let_ (x := 2) (x + 1) %
  (x # y)

test200s :: IO ()
test200s = mapM_ testEx
  [test201,test202,test203,test204,test205,test206,test207,test208,test209,test210,test211,test212,test213,test214
  ]

---------------------
-- 0/1 results
---------------------

test301 = okm "test301" [(3,3)] $
  (x := 3) # (x === 3)

test302 = okm "test302" [3] $
  (x := 1+y) %
  y := 2 %
  (x === 3)

test303 = okm "test303" [(3,3)] $
  (x === 3) # (x := 3)

test304 = okm "test304" [20] $
  (a := array [10,20,30]) %
  Sel a 1

test305 = okm "test305" [20] $
  Sel a 1 `where_` (a := array [10,20,30])

test306 = okm "test306" ([]::[()]) $
  (a := array [10,20,30]) %
  Sel a 3

test307 = okm "test307" [(1,1)] $
  t := 1 # Fst t

-- Test that when evaluating z the x is fully determined.
test308 = okm "test308" [5] $
  x := y % y := 5 % z := (x===5)

test309 = okm "test309" ([]::[()]) $
  (x := 3) # (x === 4)

test310 = okm "test310" ([]::[()]) $
  (x === 4) # (x := 3)

-- Deadlock
test311 = bad "test311" $
  y := if_ (z===1) (1|||2) (3|||4) % z:= 5|||6 % (y # z)

test300s :: IO ()
test300s = mapM_ testEx
  [test301,test302,test303,test304,test305,test306,test307,test308,test309,test310,test311
  ]

---------------------
-- Multi-valued
---------------------

test401 = okm "test401" [1,2] $
  1 ||| 2

test402 = okm "test402" [2,3,3,4] $
  (1 ||| 2) + (1 ||| 2)

test403 = okm "test403" [2,4] $
  (x := 1 ||| 2) + x

-- Should fail, since variables in ||| do not escape
test404 = bad "test404" $
  ((x := 1) ||| 2) + x

test405 = okm "test405" [(4,(1,3)),(5,(1,4)),(5,(2,3)),(6,(2,4))] $
  (x := 1 ||| 2) + (y := 3 ||| 4) # (x # y)

test406 = okm "test406" [(2,(1,1)),(5,(1,4)),(4,(2,2)),(6,(2,4))] $
  (x := 1 ||| 2) + (y := x ||| 4) # (x # y)

test407 = okm "test407" [4] $
  (x := 1 ||| 2) + (x === 2)

test408 = okm "test408" [(1,1),(2,2)] $
  x # (x := 1 ||| 2)

test409 = okm "test409" [(7,(1,1)),(7,(2,2)),(1,(1,1)),(2,(2,2))] $
  (y := (7 ||| x)) # (x # (x := (1 ||| 2)))

-- x's value should not be delayed, because x's RHS has no depenedncies
test410 = okm "test410" [((1,7),1)] $
  (x := ((1 # 7) ||| (y # (y := 2)))) # (Fst x === 1)

test411 = okm "test411" [(1,1)] $
  x := 1 ||| 2 %
  y := (x === 1) %
  (x # y)

-- Fails (equalLenient)
test412 = okm "test412" [(1,1)] $
  y := (x === 1) %
  x := 1 ||| 2 %
  (x # y)

-- Cascaded forward references
test413 = okm "test413" [3,7,2,2] $
  x := (y ||| 2) %
  y := (3 ||| z) %
  z := 7 %
  x

-- Choice under if
test414 = okm "test414" [(1,5),(1,6),(2,5),(2,6)] $
  x := 1 %
  if_ (x === 1) (1|||2) (3|||4) # (5|||6)

-- Choice under if, must suspend
test415 = okm "test415" [(1,5),(1,6),(2,5),(2,6)] $
  if_ (x === 1) (1|||2) (3|||4) # (5|||6) `where_`
  x := 1

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

test601 = ok "test601" (5,5,5) $
  for (1|||2|||3) 5

test602 = ok "test602" (1,2,3) $
  for (x := 1|||2|||3) x

test603 = ok "test603" ((1,4),(1,5),(2,4),(2,5),(3,4),(3,5)) $
  for (x := 1|||2|||3 % y := 4|||5)
    (x # y)

test604 = okm "test604" [((1,4),(2,4),(3,4)),
                         ((1,5),(2,5),(3,5))] $
  y := 4|||5 %
  for (x := 1|||2|||3)
    (x # y)

test605 = ok "test605" (((1,4),(2,4),(3,4)),
                       ((1,5),(2,5),(3,5))) $
  for (y := 4|||5) $
    for (x := 1|||2|||3)
      (x # y)

test606 = okm "test606" [(88,88),(88,99),(99,88),(99,99)] $
  for (0|||1)
    (88 ||| 99)

test607 = ok "test607" (1,2,3) $
  for (x := 1|||2|||y % y := z % z := 3)
    x

test608 = ok "test608" (2,3,4) $
  for (x := 1|||2|||3)
    (y `where_` y := x + 1)

test609 = okm "test609" [(1,2,3),(1,2,99),(1,99,3),(1,99,99),(99,2,3),(99,2,99),(99,99,3),(99,99,99)] $
  for (x := 1|||2|||3)
    (x ||| 99)

test610 = okm "test610"
            [((11,11),(21,21)),((11,11),(21,42)),((11,11),(32,21)),((11,11),(32,42))
            ,((11,42),(21,21)),((11,42),(21,42)),((11,42),(32,21)),((11,42),(32,42))
            ,((32,11),(21,21)),((32,11),(21,42)),((32,11),(32,21)),((32,11),(32,42))
            ,((32,42),(21,21)),((32,42),(21,42)),((32,42),(32,21)),((32,42),(32,42))]
            $
  for (x := 10|||20) $
    for (y := 30|||40)
      ((x1|||y1) `where_` (x1 := x + 1 % y1 := y + 2))

test600s :: IO ()
test600s = mapM_ testEx
  [test601,test602,test603,test604,test605,test606,test607,test608,test609,test610
  ]

---------------------
-- Functions
---------------------

test701 = ok "test701" 5 $
  f := var v ==> (v + 1) %
  f @@ 4

test702 = ok "test702" 11 $
  w := 7 %
  f := var v ==> (w + v) %
  f @@ 4

test703 = ok "test703" 11 $
  f := var v ==> (w + v) %
  w := 7 %
  f @@ 4

test704 = ok "test704" 11 $
  f := var v ==> (w + v) %
  w := 7 %
  y := f @@ t %
  t := 4 %
  y

-- f is called before it is defined
test705 = ok "test705" 11 $
  y := f @@ t %
  w := 7 %
  t := 4 %
  f := var v ==> (w + v) %
  y

test706 = ok "test706" 11 $
  f := do_ (w := 7 % var v ==> (w + v)) %
  y := f @@ t %
  t := 4 %
  y

-- Function defined after it is used;
test707 = ok "test707" 11 $
  y := f @@ t %
  w := 7 %
  t := 4 %
  f := var v ==> (w + v) %
  y

test708 = okm "test708" [10,11] $
  f := var v ==> (v ||| v + 1) %
  f @@ 10


test709 = bad "test709" $
  f := var v ==> failure %
  appS f 10

test710 = ok "test710" 5 $
  f := (var a # var b) ==> a + b %
  f @@ (2 # 3)

test711 = ok "test711" (999,888,13) $
  f := var n ==> (
    case_ (n + 1) [
      1 ==> 999,
      2 ==> 888,
      var x ==> x + 10
      ]) %
  array [f @@ 0, f @@ 1, f @@ 2]

test712 = ok "test712" 12 $
  twice := (var f ==> var x ==> f @@ (f @@ x)) %
  dbl := var x ==> x + x %
  twice @@ dbl @@ 3

test713 = ok "test713" 16 $
  twice := (var f ==> var x ==> f @@ (f @@ x)) %
  dbl := var x ==> x + x %
  twice @@ twice @@ dbl @@ 1

test714 = ok "test714" (2,6,4) $
  map_ := ((var f # var xs) ==> for (x := range xs) (f @@ x)) %
  inc := lam x (x + c) %
  c := 1 %
  a := array [1,5,3] %
  map_ @@ (inc # a)

test700s :: IO ()
test700s = mapM_ testEx
  [test701,test702,test703,test704,test705,test706,test707
  ,test708,test709,test710,test711,test712,test713,test714
  ]

---------------------
-- Unification
---------------------
test801 = okm "test801" [1] $
  var x %
  x === 1 %
  x

test802 = okm "test802" [1] $
  var x %
  (x # 2) === (1 # 2) %
  x

test803 = okm "test803" [(1,2)] $
  var x %
  var y %
  (x # 2) === (1 # y)

test804 = okm "test804" [1] $
  f := lam xy (Fst xy === Snd xy) %
  var x %
  f @@ (x # 1) %
  x

test805 = okm "test805" [6] $
  f := lam xyz ((var x # var y # var z) === xyz % x + y + z) %
  f @@ (1 # 2 # 3)

test806 = bad "test806" $
  var x % x+1

test800s :: IO ()
test800s = mapM_ testEx
  [test801,test802,test803,test804,test805,test806--,test807,test808,test809
  ]

---------------------
-- Conditional
---------------------

test901 = ok "test901" 1 $
  if_ (1 === 1) 1 2

test902 = ok "test902" 2 $
  if_ (0 === 1) 1 2

test903 = ok "test903" 10 $
  if_ (x := 10) x 2

test904 = ok "test904" 2 $
  if_ failure 1 2

test905 = ok "test905" 1 $
  if_ (x := 1 % x === 1) 1 2

test906 = ok "test906" 2 $
  if_ (x := 1 % x === 0) 1 2

test907 = ok "test907" 1 $
  if_ (x === 1 % x := 1) 1 2

test908 = ok "test908" 2 $
  if_ (x === 0 % x := 1) 1 2

test909 = ok "test909" 1 $
  x := 10 %
  if_ (x === 10) 1 2

test910 = ok "test910" 2 $
  x := 0 %
  if_ (x === 10) 1 2

test911 = ok "test911" 1 $
  y := if_ (x === 10) 1 2 %
  x := 10 %
  y

test912 = ok "test912" 2 $
  y := if_ (x === 10) 1 2 %
  x := 0 %
  y

test913 = ok "test913" 1 $
  if_ (x:=1) x 20

test914 = ok "test914" 1 $
  if_ (x := (1 ||| 2)) 1 20

test915 = ok "test915" 2 $
  if_ (x := (failure ||| 2)) x 20

test916 = ok "test916" 1 $
  if_ (x := (1 ||| failure)) x 20

test917 = ok "test917" 20 $
  if_ (x := (failure ||| failure)) x 20

test918 = ok "test918" 3 $
  if_ (x := (failure ||| (failure ||| 3))) x 20

test919 = ok "test919" 7 $
  f := var n ==> (if_ (n <=. 0) (n+1) (n+2)) %
  r := f @@ five %
  five := 5 %
  r

test920 = ok "test920" 6 $
  f := var n ==> (if_ (n <=. 10) (n+1) (n+2)) %
  r := f @@ five %
  five := 5 %
  r

test900s :: IO ()
test900s = mapM_ testEx
  [test901,test902,test903,test904,test905,test906,test907,test908,test909,test910,test911,test912
  ,test913,test914,test915,test916,test917,test918,test919,test920
  ]

---------------------
-- range
---------------------

test1001 = ok "test1001" (1,2,3) $
  for (x := range (array [1,2,3]))
    x

test1002 = ok "test1002" (102,103,104) $
  xs := for (x := 1|||2|||3)
    (x + 1) %
  for (y := range xs)
    (y + 100)

test1003 = ok "test1003" ((1,2),(1,4),(1,5)) $
  xys := array [1#2, 2#3, 1#4, 2#4, 1#5] %
  for (xy := range xys % Fst xy === 1)
    xy

test1004 = okm "test1004" ([]::[()]) $
  xys := array [1#2, 2#3, 1#4, 2#4, 1#5] %
  for (xy := range xys % Fst xy === 2)
    (xy `where_` Snd xy === 3)

test1005 = okm "test1005" [((2,3),(2,3))] $
  xys := array [1#2, 2#3, 1#4, 2#3, 1#5] %
  for (xy := range xys % Fst xy === 2)
    (xy `where_` Snd xy === 3)

test1006 = ok "test1006" (2,3) $
  a := for (x := range xs) (x + 1) %
  xs := array[1,2] %
  a

test1007 = ok "test1007" 2 $
  if_ (range (array [])) 1 2

test1000s :: IO ()
test1000s = mapM_ testEx
  [test1001,test1002,test1003,test1004,test1005,test1006,test1007
  ]

---------------------
-- Arithmetic, comparisons
---------------------

test1101 = ok "test1101" 10 $
  6 + 4

test1102 = ok "test1102" 2 $
  6 - 4

test1103 = ok "test1103" 24 $
  6 * 4

test1104 = okm "test1104" [1] $
  6 `div` 4

test1105 = okm "test1105" ([]::[()]) $
  6 `div` 0

test1106 = ok "test1106" 2 $
  if_ (6 <. 4) 1 2

test1107 = ok "test1107" 1 $
  if_ (2 <. 4) 1 2

test1108 = ok "test1108" 120 $
  fac := var n ==> (if_ (n <=. 0) 1 (n * fac @@ (n - 1))) %
  fac @@ 5

test1109 = ok "test1109" 120 $
  fac := var n ==> (if_ (n <=. 0) one (n * fac @@ (n - 1))) %
  res := fac @@ 5 %
  one := 1 %
  res

test1110 = ok "test1110" 120 $
  res := fac @@ five %
  fac := var n ==> (if_ (n <=. 0) one (n * fac @@ (n - 1))) %
  five := 5 %
  one := 1 %
  res

test1111 = ok "test1111" (-10) $
  negate 10

test1112 = ok "test1112" 10 $
  abs (10 - 20)

test1100s :: IO ()
test1100s = mapM_ testEx
  [test1101,test1102,test1103,test1104,test1105,test1106,test1107,test1108,test1109
  ,test1110,test1111,test1112
  ]

---------------------
-- Print
---------------------

-- XXX These tests print using trace, so they are hard to test correctly.

test1201 = ok "test1201" () $
  print_ 99

test1202 = ok "test1202" 88 $
  print_ x %
  print_ 99 %
  x := 88

test1203 = ok "test1203" 88 $
  print_ x %
  for(y:=1|||2|||3) (print_ y) %
  x := 88

test1204 = bad "test1204" $
  if_ (print_ 1) 1 2

test1200s :: IO ()
test1200s = do
  putStrLn "Expect print: [99]; print [88],print [99]; print [88],print [1],print [2],print [3]"
  mapM_ testEx
    [test1201,test1202,test1203,test1204
    ]

---------------------
-- Ref cells
---------------------

test1301 = ok "test1301" 2 $
  r := new_ 2 %
  (r^.)

test1302 = ok "test1302" (2,3) $
  r := new_ 2 %
  x := (r^.) %
  r ^:= x+1 %
  y := (r^.) %
  (x # y)

test1303 = ok "test1303" 2 $
  r := new_ 0 %
  if_ (1 === 1) (r ^:= 2) (r ^:= 3) %
  (r^.)

test1304 = ok "test1304" 6 $
  r := new_ 0 %
  for (i := 1|||2|||3)
    (x := (r^.) % r ^:= x + i) %
  (r^.)

test1305 = ok "test1305" 2 $
  r := new_ 0 %
  for (i := 1|||2|||3 %
       r ^:= i %
       i === 2)
    0 %
  (r^.)

test1306 = ok "test1306" (2,3) $
  x := (r^.) %
  r ^:= x+1 %
  y := (r^.) %
  r := new_ 2 %
  (x # y)

test1300s :: IO ()
test1300s = mapM_ testEx
  [test1301,test1302,test1303,test1304,test1305,test1306
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
  test1100s
  test1200s
  test1300s

