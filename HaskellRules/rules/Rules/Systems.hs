module Rules.Systems(allSystems, lookupSystem) where
import TRS.System
import Rules.Core
import Rules.PLDI
import Rules.POPL

allSystems :: [ESystem]
allSystems =
  [ systemPOPL, systemVPOPL
  , systemPLDI
  ]

lookupSystem :: String -> Either String ESystem
lookupSystem s =
  case lookupTRSystem s allSystems of
    []  -> Left "No system found"
    [s] -> Right s
    ss  -> Left $ "Multiple systems found: " ++ show (map sname ss)
