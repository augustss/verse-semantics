{-

A few tests. Can be run with

   make tests && ./tests

or with auto-reload using

   ghcid Tests.hs -W -T main --clear

-}

module Main where

import GHC.Stack
import Control.Exception

import Core
import Desugar
import Eval
import Parse
import Print

-- Test assertions

-- | Tests that two SourceVerse expressions, when desugared and converted to
-- core, evaluate to the same thing

assertEquiv :: HasCallStack => String -> String -> IO ()
assertEquiv = assertEquiv' True

assertFail :: HasCallStack => String -> String -> IO ()
assertFail = assertEquiv' False

assertEquiv' :: HasCallStack => Bool -> String -> String -> IO ()
assertEquiv' expectOK src1 src2 = do
    let p1 = parseDie pFile "<test input 1>" src1
    let p2 = parseDie pFile "<test input 2>" src2
    let d1 = desugar p1
    let d2 = desugar p2
    let c1 = exprToCore d1
    let c2 = exprToCore d2
    let v1 = eval False c1
    let v2 = eval False c2

    let pos = case getCallStack (popCallStack callStack) of
                [] -> "unknown location"
                (_,sloc):_ -> prettySrcLoc sloc

    catch
      ( if (v1 `equivValue` v2) == expectOK
        then do
            putStrLn $ pos ++ if expectOK then " success!" else " failure, expected"
        else do
            if expectOK
            then do
                putStrLn $ pos ++ " failure:"
                putStrLn "The expression"
                pp p1
                putStrLn "evaluates to"
                pp v1
                putStrLn "but"
                pp p2
                putStrLn "evaluates to"
                pp v2
                putStrLn ""
            else do
                putStrLn $ pos ++ " unexpected success, please update test case!"
      ) (\e -> do
            putStrLn $ pos ++ " failure:"
            putStrLn "The expression"
            pp p1
            putStrLn "or the expression"
            pp p2
            putStrLn "caused an exception:"
            print (e :: SomeException)
            putStrLn ""
      )

-- | Equivalence on values (or stuck expressions)
--
--  * Ignores message on `wrong`
--  * TODO: α-equivalence on lambdas
equivValue :: Core -> Core -> Bool
equivValue (CWrong _) (CWrong _) = True
equivValue v1 v2 = v1 == v2

main :: IO ()
main = do
    -- Check that BIND removes _one_ unification only
    assertEquiv "x:any; x = (y:any => 1); x = (y:any => 2)" "(y:any => 1) = (y:any => 2)"
    assertEquiv "x:any; x = (y:any => 1); x = (y:any => 1)" "(y:any => 1) = (y:any => 1)"
    assertEquiv "x:any; y:any; x = y; x = y; x = 1" "1"
    assertEquiv "x:any; y:any; x = y; x = y; x = (z:any => 1)" "(z:any => 1) = (z:any => 1)"

    -- If BIND is implemented in parallel, it may forget to substitute in the substitutions
    assertEquiv "x:any; y:any; x=y; y=1" "1"
    assertEquiv "x:any; y:any; x=1; y=x" "1"
    assertEquiv "x:any; y:any; x=y; y=(x,x)" "x:any; x=(x,x)" -- check for occurs check
    assertEquiv "x:any; y:any; y=(x,x); x=y" "x:any; x=(x,x)" -- check for occurs check

    -- Array access via choices
    assertEquiv "for ((3,4,5)[x:int]) {x}" "(0,1,2)"
    -- Non-ANF array access
    assertEquiv "for (x:int, x=(2,0,2)[x+1]) {x}" "(0)"

    -- Recursion
    assertEquiv "f(n:int) := if (n = 0) {1} else {n * f(n-1)}; f(5)" "120"
    assertEquiv "even(n:int) := if (n = 0) {1} else {odd(n-1)};\
                \odd(n:int)  := if (n = 0) {0} else {even(n-1)};\
                \for(x := 0|1|2|3) {(even(x), odd(x))}"
                "((1,0), (0,1), (1,0), (0,1))"
    assertEquiv "pair := (1, function(n:int) {pair[0]}); pair[0]" "1"
    assertEquiv "pair := (1, function(n:int) {if (n = 0) {1} else {n * pair[1](n-1)}}); pair[1](5)" "120"

    -- Non-retractions as types
    assertEquiv "succ(n:int) := n + 1;\
                \f(m:succ) := m;\
                \f(5)"
                "6"

    -- Evaluation in if
    assertEquiv "if(y:int; y=1) 42 else 23" "42"
    assertEquiv "if(y:int; y=1) y else 23" "1"
    -- Confluence: Lack of local substitution
    assertFail  "x : int; if (y:int; y = x; y = 1; y > 1) {42} else {23}"
                "x : int; if (y:int; y = 1; y = x; y > 1) {42} else {23}"

