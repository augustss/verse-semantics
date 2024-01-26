module Rules.Systems(ESystem, allSystems, lookupSystemEx, lookupSystem,
                     TRSystem(..), defaultTRSFlags, isRecursive, systemDescr) where
import Epic.String
import TRS.System
import Rules.Core
import Rules.TRS2024(allSystemsTRS2024)
import Rules.ICFP(allSystemsICFP, isRecursive)
import Rules.PLDI(allSystemsPLDI)
import Rules.POPL(allSystemsPOPL)
import Rules.KoenNaive(allSystemsKoen)
import Rules.Block(allSystemsBlock)
import Rules.Verifier(allSystemsVerify)
import Rules.LeftToRight(allSystemsLeftToRight)

type ESystem = TRSystem Expr

allSystems :: [ESystem]
allSystems =
     allSystemsICFP
  ++ allSystemsTRS2024
  ++ allSystemsPOPL
  ++ allSystemsPLDI
  ++ allSystemsKoen
  ++ allSystemsBlock
  ++ allSystemsVerify
  ++ allSystemsLeftToRight

systemDescr :: ESystem -> String
systemDescr s = sname s ++ ": " ++ description s

lookupSystem :: String -> Either String ESystem
lookupSystem = lookupSystemEx allSystems

lookupSystemEx :: [ESystem] -> String -> Either String ESystem
lookupSystemEx sys n =
  case lookupTRSystem n sys of
    []  -> Left "No system found"
    [s] -> Right s
    ss  ->
      -- Exact match takes priority
      case filter (equalCI n . sname) ss of
        [s] -> Right s
        _   -> Left $ "Multiple systems found: " ++ show (map sname ss)
