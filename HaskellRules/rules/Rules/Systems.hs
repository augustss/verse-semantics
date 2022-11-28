module Rules.Systems(ESystem, allSystems, lookupSystem, TRSystem(..)) where
import TRS.System
import Rules.Core
import Rules.PLDI
import Rules.POPL

type ESystem = TRSystem Expr

allSystems :: [ESystem]
allSystems =
  [ systemPOPL, systemVPOPL
  , systemPLDI
  ]

lookupSystem :: String -> Either String ESystem
lookupSystem n =
  case lookupTRSystem n allSystems of
    []  -> Left "No system found"
    [s] -> Right s
    ss  -> Left $ "Multiple systems found: " ++ show (map sname ss)
