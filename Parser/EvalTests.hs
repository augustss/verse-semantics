module EvalTest where

import Control.Monad
import GHC.Stack

import Core
import Desugar
import Eval
import Parse
import Print

-- Test assertions

-- | Tests that two SourceVerse expressions, when desugared and converted to
-- core, evaluate to the same thing
--
-- TODO: Use something more suitable than ==
--  * α-equivalence on values
--  * equating wrong with wrong
assertEquiv :: HasCallStack => String -> String -> IO ()
assertEquiv src1 src2 = do
    let p1 = parseDie pFile "<test input 1>" src1
    let p2 = parseDie pFile "<test input 2>" src2
    let d1 = desugar p1
    let d2 = desugar p2
    let c1 = exprToCore d1
    let c2 = exprToCore d2
    let v1 = eval c1
    let v2 = eval c2

    let pos = case getCallStack callStack of
                [] -> "unknown location"
                (_,sloc):_ -> prettySrcLoc sloc

    if v1 == v2
    then do
        putStrLn $ pos ++ " success!"
    else do
        putStrLn $ pos ++ " failure:"
        putStrLn "The expression"
        pp p1
        putStrLn "evaluates to"
        pp v1
        putStrLn "but"
        pp p2
        putStrLn "evaluates to"
        pp v2

main :: IO ()
main = do
    -- Check that BIND removes _one_ unification only
    assertEquiv "x:any; x = (y:any => 1); x = (y:any => 2)" "(y:any => 1) = (y:any => 2)"
    assertEquiv "x:any; x = (y:any => 1); x = (y:any => 1)" "(y:any => 1) = (y:any => 1)"
    -- If BIND is implemented in parallel, it may forget to substitute in the substitutions
    assertEquiv "x:any; y:any; x=y; y=1" "1"
    assertEquiv "x:any; y:any; x=1; y=x" "1"
    assertEquiv "x:any; y:any; x=x; y=x" "x:int; x=(x,x)" -- check for occurs check


