module TRS.System(TRSystem(..)) where
import TRS.TRS

data TRSystem t = TRSystem
  { sname               :: String  -- short system name, should be an identfier
  , description         :: String  -- longer system description
  , preProcess          :: t -> t  -- prepare a term for rule application, e.g., ANF
  , rules               :: Rule t  -- rewrite rules
  , rulesHaveStructural :: Bool    -- are any rules structural? (slower)
  , confluenceRules     :: Rule t  -- ???
  }
--  deriving (Show)
