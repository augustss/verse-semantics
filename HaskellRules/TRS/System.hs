module TRS.System(TRSystem(..), lookupTRSystem) where
import Data.Char
import Data.List
import TRS.TRS

-- | Case insensitive lookup of all systems matching a prefix]
lookupTRSystem :: String -> [TRSystem t] -> [TRSystem t]
lookupTRSystem n = filter (\ s -> map toLower n `isPrefixOf` map toLower (sname s))
