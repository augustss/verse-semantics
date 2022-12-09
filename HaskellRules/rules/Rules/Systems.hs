module Rules.Systems(ESystem, allSystems, lookupSystemEx, lookupSystem,
                     TRSystem(..), defaultTRSFlags) where
import Epic.String
import TRS.System
import Rules.Core
import Rules.PLDI(allSystemsPLDI)
import Rules.POPL(allSystemsPOPL)
import Rules.KoenNaive(allSystemsKoen)

type ESystem = TRSystem Expr

allSystems :: [ESystem]
allSystems =
     allSystemsPOPL
  ++ allSystemsPLDI
  ++ allSystemsKoen

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
