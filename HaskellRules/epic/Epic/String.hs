module Epic.String(equalCI) where
import Data.Char

-- Case insensitive equality
equalCI :: String -> String -> Bool
equalCI s1 s2 = map toLower s1 == map toLower s2
