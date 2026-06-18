-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.All
-- Copyright : (c) Epic Games
-- License   : CC0
-- Maintainer: jeffrey.young@epicgames.com
-- Stability : experimental
--
--  Each of these tests come from the file
--  $ROOT/VersePrototypes/parser/test_data/all.verse
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module TestSuite.All
  ( unitTests
  ) where

import Utils

import Parser.Verse

import Test.Tasty

-----------------------------------------------
--
--              Unit Tests
--
-----------------------------------------------

-- TODO: issue #87: Tests marked with 'broken' should pass but I've run out of
-- time. Tackle these as they arise.

unitTests :: TestTree
unitTests = testGroup "parser/test_data/all.verse"
  [ var_ref_set
  , asserts
  , application
  , literals
  , paths
  , identifiers
  , where_all
  , misc
  , fun_def
  , enums
  , field_accesses
  , classes
  , effects
  , infix_
  , postfix
  , prefix
  , ifs
  , chars
  , strings
  , mul
  , add
  , to
  , markup
  , specs
  ]


var_ref_set :: TestTree
var_ref_set =
  let passes = prettyTest pExpr
  in testGroup "var_ref_set" $
  [ passes ("var X:int = 0", "(var X : int) = 0")
  , passes ("var x;", "var x")
  , passes ("set x;", "set x")
  , passes ("ref x;", "ref x")
  , passes ("alias x;", "alias x")
  , passes ("var x = 1;", "var x = 1")
  , passes ("var x := 1;", "var x := 1")
  ] ++
  [ broken "redundant set" $ passes ("set a|b += 1;", "set a|b += 1;")
  , broken "redundant set" $ passes ("set Object.Variable += 1;", "set Object.Variable += 1;")
  , broken "redundant set" $ passes ("set pat /= :C", "set pat /= :C")
  , broken "redundant set" $ passes ("set x += 1;", "set x += 1")
  , broken "redundant set" $ passes ("set x *= 1;", "set x *= 1")
  , broken "redundant set" $ passes ("set x /= 1;", "set x /= 1")
  , broken "redundant set" $ passes ("set x = {1};", "set x = {1}")
  , broken "redundant set" $ passes ("set x = {1; 2};", "set x = {1; 2}")
  ]

asserts :: TestTree
asserts =
  let passes = prettyTest pExpr
  in testGroup "asserts" $
  [ broken "fails on a space"
    $ passes ( "assert:\n    var A:[]int=array{}\n    B:=if (true?):\n        set A += array{0}"
             , "assert{\n  (var A : [[]]int) = array{\n\n  }\n  B := if ((true)?){\n    set set A += array{\n      0\n    }\n  }\n}"
             )
  , passes ( "assert{A := c0{}; B := c0{}; A.ParametricSubobject.Field<>B.ParametricSubobject.Field}"
           , "assert{\n  A := c0{\n\n  }\n  B := c0{\n\n  }\n  (A .ParametricSubobject .Field <> B .ParametricSubobject .Field)\n}"
           )
  , broken "fails on a space"
    $ passes ( "assert:\n    var A:[]int=array{}\n    B:=if (true?):\n        set A += array{0}"
             , "assert{\n  (var A : [[]]int) = array{\n\n  }\n  B := if ((true)?){\n    set set A += array{\n      0\n    }\n  }\n}"
             )
  , passes ( "assert{Concatenate(for(I:=0..100) do array{})=array{}};"
           , "assert{\n  Concatenate(for(I := 0..100) do{\n    array{\n\n    }\n  }) = array{\n\n  }\n}"
           )
  ]

application :: TestTree
application =
  let passes = prettyTest pExpr
  in testGroup "application" $
  [ passes ("x(a){b}", "x(a){\n  b\n}")
  , passes ("x[a]", "x[a]")
  , passes ("var X:int = 1\n prefix'[]'(t:type)(Xs:any) := for (X:Xs). t[X]", "(var X : int) = 1")
  ] ++
  [ broken "unsure what this should be" $ passes ("a^,  (c:)b", "a^,  (c:)b")
  ]

literals :: TestTree
literals =
  let passes = prettyTest pExpr
  in testGroup "literals" $
  [ passes ("1", "1")
  , passes ("+{1;2};", "+ 1\n2")
  , passes ("0x10", "16")
  , passes ("1.5", "1.5")
  ]


paths :: TestTree
paths =
  let passes = prettyTest pExpr
  in testGroup "paths" $
  [ passes ("/apa?", "(/apa)?")
  , passes ("/a-d", "/a-d")
  , passes ("/apa@bepa/xyz", "/apa@bepa/xyz")
  , passes ("/apa@bepa/(/another@label:)z", "/apa@bepa/(/another@label:)z")
  ]


identifiers :: TestTree
identifiers =
  let passes = prettyTest pExpr
  in testGroup "identifiers" $
  [ passes ("verylongidentifier\n  .(c:)b", "verylongidentifier .(c:)b")
  , passes ("x.y", "x .y")
  , passes ("d", "d")
  ]


where_all :: TestTree
where_all =
  let passes = prettyTest pExpr
  in testGroup "where_all" $
  [ passes ("a<b>. c where d", "a<b>{\n  c where{ d }\n}")
  , passes ("a => b where c", "(a) => {\n  b where{ c }\n}")
  , passes ("all . 1 +2 where 3", "all {\n  1 + 2 where{ 3 }\n}")
  , passes ("all. set Z = F(:Xs, Z)", "all {\n  set Z = F(((:Xs), Z))\n}")
  , passes ("all{Y | (:Z)}", "all {\n  (Y | (:Z))\n}")
  ]


fun_def :: TestTree
fun_def =
  let passes = prettyTest pExpr
  in testGroup "function_definitions" $
  [ passes ("f():int = return;", "(f() : int) = return")
  , passes ("f()<x>:int = return;", "(f()<x> : int) = return")
  , passes ( "f3(o:O0)<transacts><decides>:int = (return o?)"
           , "(f3((o : O0))<transacts><decides> : int) = return ((o)?)"
           )
  , passes ("for(X:=1..2 where 3){4}", "for(X := 1..2 where{ 3 }){\n  4\n}")
  , passes ( "Cons := function (Y:int, Z:int). all{Y | (:Z)}"
           , "Cons := function(((Y : int), (Z : int))){\n  all {\n    (Y | (:Z))\n  }\n}"
           )
  , passes ("f():int = return;", "(f() : int) = return")
  , passes ("f():int = return 0=1;", "(f() : int) = return (0 = 1)")
  ]


-- unsure what these are or are supposed to be
misc :: TestTree
misc =
  let passes = prettyTest pExpr
  in testGroup "misc" $
  [ passes ("X->Y := 1..3", "X -> Y := 1..3")
  , passes ("xxx(X:C){1}", "xxx((X : C)){\n  1\n}")
  , passes ("xxxx:", "xxxx{\n\n}")
  , passes ("x<public>:int", "(x<public> : int)")
  , passes ("x<public>():int", "(x<public>() : int)")
  , passes ("map[1] = 1", "map[1] = 1")
  , passes ("map{\"\"=>0}[\"\"]=0", "map{\n  (\"\") => {\n    0\n  }\n}[\"\"] = 0")
  , passes ("map{\"\"=>0}(\"\")=0", "map{\n  (\"\") => {\n    0\n  }\n}(\"\") = 0")
  , passes ("map{\"\"=>0}{\"\"}=0", "map{\n  (\"\") => {\n    0\n  }\n}{\n  \"\"\n} = 0")
  , passes ("x where {y};", "x where{ y }")
  , passes ("x where y;", "x where{ y }")
  , passes ("x where y,z;", "x where{ y\nz }")
  , passes ("x is {y};", "x is{ y }")
  , passes ("x is y;", "x is{ y }")
  , passes ("x over {y};", "x over {\n  y\n}")
  , passes ("x when {y};", "x when {\n  y\n}")
  , passes ("x while {y};", "x while {\n  y\n}")
  , passes ("x over y;", "x over {\n  y\n}")
  , passes ("x over y, z;", "x over {\n  y\n  z\n}")
  , passes ("x => y;", "(x) => {\n  y\n}")
  , passes ("x => {y};", "(x) => {\n  y\n}")
  , passes ("x next y;", "x next y")
  , passes ("x @f();", "x @f()")
  , passes ("x @f() @g();", "x @f() @g()")
  , passes ("@f() x;", "@f() x")
  , passes ("@f() @g() x;", "@f() @g() x")
  , passes ("1,2,3;", "1")
  , passes ("..x;", "..x")
  , passes ("&x;", "&x")
  , passes ("return;", "return")
  , passes ("yield;", "yield")
  , passes ("break;", "break")
  , passes ("continue;", "continue")
  , passes ("return x;", "return (x)")
  , passes ("return {x;y};", "return (x\ny)")
  , passes ("in x;", "(:x)")
  , passes (":x;", "(:x)")
  , passes ("(0);", "0")
  , passes ("0;", "0")
  , passes ("InsertAt(Array:[N]t, Index:int, Value:t where N:int, t:type, 0<=Index<=N):[N+1]t=1;"
           ,"(InsertAt(( (Array : [[N]]t)\n, (Index : int)\n, (Value : t) where{ (N : int)\n(t : type)\n(0 <= (Index <= N)) }\n)) : [[N + 1]]t) = 1"
           )
  , passes ("Func()<decides>:int = 1", "(Func()<decides> : int) = 1")
  , passes ( "F(X:Curry(operator'+')(Y) where Y:int = 1) := X"
           , "F((X : Curry(operator'+')(Y)) where{ (Y : int) = 1 }) := X"
           )
  , passes ( "MFun<protected>(X:int)<transacts>:float = X*10+1"
           , "(MFun<protected>((X : int))<transacts> : float) = X * 10 + 1"
           )
  , passes ( "MFun<public><protected>(X:int)<transacts><suspends>:float = X*10+1"
           , "(MFun<public><protected>((X : int))<transacts><suspends> : float) = X * 10 + 1"
           )
  , passes ("X(Y:int):(Z:Int):int ;", "((X((Y : int)) : (Z : Int)) : int)")
  , passes ( "X(Y:int):(Z:Int):(W:int):int ;"
           , "(((X((Y : int)) : (Z : Int)) : (W : int)) : int)"
           )
  , passes ("F(0);", "F(0)")
  , passes ("X where Y", "X where{ Y }")
  , passes ("x;", "x")
  , passes ("(y:)x", "(y:)x")
  ] ++
  [
  ]

enums :: TestTree
enums =
  let passes = prettyTest pExpr
  in testGroup "enums" $
  [ broken "Unexpected A (65)" $
    passes ( "enum1 := enum<persistable><public>:\n       A"
           , "enum1 := enum<persistable><public>{\n  A\n}"
           )
  ]


field_accesses :: TestTree
field_accesses =
  let passes = prettyTest pExpr
  in testGroup "field_accesses" $
  [ passes ( "A.ParametricSubobject.Field <>  B.ParametricSubobject.Field"
           , "(A .ParametricSubobject .Field <> B .ParametricSubobject .Field)"
           )
  , passes ("class2{}.F();", "class2{\n\n} .F()")
  ]

classes :: TestTree
classes =
  let passes = prettyTest pExpr
  in testGroup "classes" $
  [ passes ( "class<internal>(xxx){x<public>:int}"
           , "class<internal>(xxx){\n  (x<public> : int)\n}"
           )
  , passes ( "ClassA:=xxxx<abstract>:"
           , "ClassA := xxxx<abstract>{\n\n}"
           )
  , passes ( "my_class<public> := class<internal>(xxx){x<public>:int}"
           , "my_class<public> := class<internal>(xxx){\n  (x<public> : int)\n}"
           )
  , passes ( "my_class<public> := class<internal>{x<public>:int}"
           , "my_class<public> := class<internal>{\n  (x<public> : int)\n}"
           )
  , passes ("class<internal>{}", "class<internal>{\n\n}")
  , passes ("class<internal>(xxx):", "class<internal>(xxx){\n\n}")
  ]

effects :: TestTree
effects =
  let passes = prettyTest pExpr
  in testGroup "effects" $
  [ passes ("   x<public>:int", "(x<public> : int)")
  , passes ("X := (:C)", "X := (:C)")
  , passes ("X or (:C)", "X or (:C)")
  , passes ("f<native>() : void", "(f<native>() : void)")
  ]


infix_ :: TestTree
infix_ =
  let passes = prettyTest pExpr
      mixed  = testGroup "mixed" $
        [ passes ("X < Y < Z", "(X < (Y < Z))")
        , passes ("X > Y > Z", "(X > (Y > Z))")
        , passes ("X > Y < Z", "((X > Y) < Z)")
        ] ++
        [ broken "likely ambiguous, drops Z" $ passes ("X < Y > Z", " X < (Y > Z)")
        ]
      choice = testGroup "choice"
        [ passes ("x|y;", "(x | y)")
        , passes ("x|y|z;", "(x | (y | z))")
        ]
      greater_than = testGroup "greater than"
        [ passes ("x>y;", "(x > y)")
        , passes ("x>=y;", "(x >= y)")
        , passes ("x>y>z;", "(x > (y > z))")
        ]
      less_than = testGroup "less than"
        [ passes ("x<y;", "(x < y)")
        , passes ("x<=y;", "(x <= y)")
        , passes ("x<y<z;", "(x < (y < z))")
        ]
      not_equal = testGroup "not equal"
        [ passes ("x<>y;", "(x <> y)")
        , passes ("x<>y<>z;", "((x <> y) <> z)")
        , passes ("x=y;", "x = y")
        , passes ("x=y=z;", "x = y = z")
        ]
      equal = testGroup "equal" $
        [ passes ("x:=y;", "x := y")
        , passes ("x:=y:=z;", "x := y := z")
        , passes ("x:={y};", "x := y")
        , passes ("x:int;", "(x : int)")
        , passes ("x:int=1;",  "(x : int) = 1")
        , passes ("x:int:=1;", "(x : int) := 1")
        , passes ("(x:int)=1;", "(x : int) = 1")
        ]
      and = testGroup "and" $
        [ passes ("x and y;", "x and y")
        , passes ("x and y and z;", "x and y and z")
        ]
      or = testGroup "or" $
        [ passes ("x or y;", "x or y")
        , passes ("x or y or z;", "x or y or z")
        ]
  in testGroup "infix"
  [ choice
  , greater_than
  , less_than
  , not_equal
  , equal
  , mixed
  , and
  , or
  ]

postfix :: TestTree
postfix =
  let passes = prettyTest pExpr
  in testGroup "Postfix" $
  [ passes ("f(0){1};", "f(0){\n  1\n}")
  , passes ("f(0)do{2; 3};", "f(0) do{\n  2\n  3\n}")
  , passes ("f(0)do. 4;", "f(0) do{\n  4\n}")
  , passes ("f(0)do 5;", "f(0) do{\n  5\n}")
  , passes ("f{1};", "f{\n  1\n}")
  , passes ("f{1}do{2};", "f{\n  1\n} do{\n  2\n}")
  , passes ("f{1}until{2};", "f{\n  1\n} until{\n  2\n}")
  , passes ("f{1}until 2;", "f{\n  1\n} until{\n  2\n}")
  , passes ("f{1}catch(2){3};", "f{\n  1\n} catch 2{\n  3\n}")
  , passes ("f(0);", "f(0)")
  , passes ("g(1){1}catch(2){3};", "g(1){\n  1\n} catch 2{\n  3\n}")
  , passes ("f at {1};", "f(1)")
  , passes ("f at 1;", "f(1)")
  , passes ("f of {1};", "f[1]")
  , passes ("f of 1;", "f[1]")
  , passes ("x^;", "(x)^")
  , passes ("x?;", "(x)?")
  , passes ("x[1];", "x[1]")
  , passes ("x.y;", "x .y")
  , passes ("x^[1](2);", "(x)^[1](2)")
  ]


prefix :: TestTree
prefix =
  let passes = prettyTest pExpr
      ops = testGroup "operators"
        [ passes ("^x;", "^x")
        , passes ("?x;", "?x")
        , passes ("[1]x;", "[[1]]x")
        , passes ("+x;", "+ x")
        , passes ("-x;", "- x")
        , passes ("*x;", "* x")
        , passes ("-^x;", "- ^x")
        , passes ("+{1;2};", "+ 1\n2")
        ]
      not = testGroup "not" $
        [ passes ("not x;", "not (x)")
        , passes ("not not x;", "not (not (x))")
        ]
  in testGroup "prefix" $
  [ ops
  , not
  ]


ifs :: TestTree
ifs =
  let passes = prettyTest pExpr
  in testGroup "ifs" $
  [ passes ("if(0){0}else x;", "if (0) {\n  0\n} else {\n  x\n}")
  , passes ("if(0){0};", "if (0){\n  0\n}")
  , passes ("if(0)then 1 else x;", "if (0) {\n  1\n} else {\n  x\n}")
  , passes ("if(0)then 1;", "if (0){\n  1\n}")
  , passes ("if(0)then {1} else x;", "if (0) {\n  1\n} else {\n  x\n}")
  , passes ("if(0)then {1};", "if (0){\n  1\n}")
  , passes ("if(0)then 1 else {2};", "if (0) {\n  1\n} else {\n  2\n}")
  , passes ("if(0)then 1 else. 2;", "if (0) {\n  1\n} else {\n  2\n}")
  , passes ( "if(0)then 1 else if (2) then 3;"
           , "if (0) {\n  1\n} else {\n  if (2){\n    3\n  }\n}"
           )
  , passes ("if(0). 1;", "if (0){\n  1\n}")
  , passes ("if(0). 1 else 2;", "if (0) {\n  1\n} else {\n  2\n}")
  , passes ("if{fail};", "if (fail)")
  , passes ("if(0):\n 1\nelse:\n 2", "(if (0) : 1)")
  ]


chars :: TestTree
chars =
  let passes = prettyTest pExpr
  in testGroup "chars" $
  [ passes ("'a';", "'a'")

  , passes ("'\'';", "'\''")
  , passes ("0o21;", "'!'")
  , passes ("0u00022;", "0u22")
  , passes ("/apa;", "/apa")
  , passes ("(/path:)apa", "(/path:)apa")
  ] ++
  [ broken "throws exception" $ passes ("'\n';", "'\n';")
  ]

strings :: TestTree
strings =
  let passes = prettyTest pExpr
  in testGroup "strings" $
  [ passes ("\"a\";", "\"a\"")
  , passes ("\"abc\";", "\"abc\"")
  , passes ("\"\";", "\"\"")
  , passes ("\"a{2}b\";", "\"a{ 2 }b\"")
  , passes ("\"a{\"c\"}b\";", "\"a{ \"c\" }b\"")
  ] ++
  [ broken "unexpected newline" $ passes ("\"a\nb\\c\";", "")
  ]

mul :: TestTree
mul =
  let passes = prettyTest pExpr
  in testGroup "mul" $
  [ passes ("x*y;", "x * y")
  , passes ("x/y;", "x / y")
  , passes ("x*y*z;", "x * y * z")
  ]


add :: TestTree
add =
  let passes = prettyTest pExpr
  in testGroup "add" $
  [ passes ("x+y;", "x + y")
  , passes ("x-y;", "x - y")
  , passes ("x-y-z;", "x - y - z")
  ]


to :: TestTree
to =
  let passes = prettyTest pExpr
  in testGroup "to" $
  [ passes ("x to y;", "x..y")
  , passes ("x..y;", "x..y")
  , passes ("x->y;", "x -> y")
  , passes ("x->y->z;", "x -> y -> z")
  ]


markup :: TestTree
markup =
  let passes = prettyTest pcExpr
  in testGroup "markup" $
  [ passes ("x;", "x")
  , passes ("0", "0")
  , passes ("if(0). 1;", "if (0){\n  1\n}")
  ] ++
  [ broken "fails on newline"
    $ passes ("# <M; {x}\n<#x#>foo>;   NOTYET", "# <M; {x}\n<#x#>foo>;   NOTYET")
  , broken "comments expect ending sigils" $ passes ("#<M; a<B;b>>;", "#<M; a<B;b>>;")
  , broken "comments expect ending sigils" $ passes ("#<M>foo</M>;", "#<M>foo</M>;")
  , broken "comments expect ending sigils" $ passes ("#<M,N;a>;", "#<M,N;a>;")
  , broken "comments expect ending sigils" $ passes ("#<M(){};a>;", "#<M(){};a>;")
  , broken "comments expect ending sigils" $ passes ("#<M(){}(a){b};a>;", "#<M(){}(a){b};a>;")
  , broken "comments expect ending sigils" $ passes ("#<M.N; a>;", "#<M.N; a>;")
  , broken "comments expect ending sigils" $ passes ("#<M(0).N; a>;", "#<M(0).N; a>;")
  , broken "comments expect ending sigils" $ passes ("#<M; ~a ~b ~c d>;", "#<M; ~a ~b ~c d>;")
  , broken "comments expect ending sigils" $ passes ("#<M:>", "#<M:>")
  , broken "comments expect ending sigils" $ passes ("# a b", "# a b")
  , broken "comments expect ending sigils" $ passes ("# c", "# c")
  , broken "comments expect ending sigils" $ passes ("#<M;>", "#<M;>")
  , broken "comments expect ending sigils" $ passes ("#<M;a", "#<M;a")
  , broken "comments expect ending sigils" $ passes ("#b>;", "#b>;")
  ]

specs :: TestTree
specs =
  let passes = prettyTest pExpr
  in testGroup "specs" $
  [ passes ("x<a>;", "x<a>")
  , passes ("x<a><b>;", "x<a><b>")
  , passes ("a with<a>;", "a<a>")
  ]
