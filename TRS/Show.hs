module TRS.Show where

--------------------------------------------------------------------------------

class Show a => Parens a where
  parens :: a -> Bool

show' :: Parens a => a -> String
show' e | parens e  = "(" ++ show e ++ ")"
        | otherwise = show e

--------------------------------------------------------------------------------

