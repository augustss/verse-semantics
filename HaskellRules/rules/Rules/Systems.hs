module Rules.Systems(ESystem, allSystems, lookupSystemEx, lookupSystem,
                     TRSystem(..), defaultTRSFlags) where
import Epic.String
import TRS.System
import Rules.Core
import Rules.ICFP(allSystemsICFP)
import Rules.PLDI(allSystemsPLDI)
import Rules.POPL(allSystemsPOPL)
import Rules.KoenNaive(allSystemsKoen)
import Rules.Block(allSystemsBlock)

type ESystem = TRSystem Expr

allSystems :: [ESystem]
allSystems =
     allSystemsICFP
  ++ allSystemsPOPL
  ++ allSystemsPLDI
  ++ allSystemsKoen
  ++ allSystemsBlock

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
