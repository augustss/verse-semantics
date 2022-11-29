module Rules.Systems(ESystem, allSystems, lookupSystem, TRSystem(..)) where
import Epic.String
import TRS.System
import Rules.Core
import Rules.PLDI
import Rules.POPL

type ESystem = TRSystem Expr

allSystems :: [ESystem]
allSystems =
  [ systemPOPL, systemPOPLV
  , systemPLDI, systemPLDIG, systemPLDIS
  ]

lookupSystem :: String -> Either String ESystem
lookupSystem n =
  case lookupTRSystem n allSystems of
    []  -> Left "No system found"
    [s] -> Right s
    ss  ->
      -- Exact match takes priority
      case filter (equalCI n . sname) ss of
        [s] -> Right s
        _   -> Left $ "Multiple systems found: " ++ show (map sname ss)
