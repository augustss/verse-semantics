module Main where

import TRS.TRSCore
import TRS.Rules.POPL( systemPOPL )
import TRS.TRS( step, normalFormsFuelTrace, nub )
import TRS.TRSGraph( normalFormsFuelTraceWithGraph )
import TRS.Tarjan
import TRS.Traced
import TRS.Rules
import Test.QuickCheck

--------------------------------------------------------------------------------

main :: IO ()
main =
  quickCheck (prop_Confluence systemPOPL)

prop_Confluence sys p =
  case nub . map (norm sys) . normalForms sys $ p of
    trs@(_:_:_) ->
      whenFail (sequence_
                  [ do putStrLn ("==trace:" ++ show i ++ "==")
                       putStr $ unlines $ showTrace ttr
                  | (ttr,i) <- trs `zip` [1..]
                  ]) False
    
    _ -> property True

---
  
normalForms :: System Expr -> Expr -> [Traced Expr]
normalForms sys
  | rulesHaveStructuralRules sys = normalFormsFuelTraceWithGraph defaultTRSFlags 99 (rules sys)
  | otherwise                    = normalFormsFuelTrace defaultTRSFlags 99 (rules sys)

norm :: System Expr -> Traced Expr -> Traced Expr
norm sys = minimum . head . tarjan tstep
 where
  tstep (t :<-- tr) =
    [ t' :<-- ((n,t):tr)
    | (n, t') <- step (confluence sys) defaultTRSFlags t
    ]

--------------------------------------------------------------------------------

