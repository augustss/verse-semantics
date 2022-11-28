module TRS.System(TRSystem(..), lookupTRSystem) where
import Data.Char
import Data.List
import TRS.TRS

data TRSystem t = TRSystem
  { sname               :: !String       -- short system name, should be an identfier
  , description         :: !String       -- longer system description
  , ruleEnv             :: !(RuleEnv t)  -- environment for running rule execution
  , preProcess          :: !(t -> t)     -- prepare a term for rule application, e.g., ANF
  , postProcess         :: !(t -> t)     -- post processing, e.g., undo ANF
  , rules               :: !(Rule t)     -- rewrite rules
  , rulesHaveStructural :: !Bool         -- are any rules structural? (slower)
  , confluenceRules     :: !(Rule t)     -- structural rules for equivalence test
  , validExpr           :: !(t -> Bool)  -- is t valid for reduction
  }
--  deriving (Show)

instance Show (TRSystem t) where
  show _ = "<<TRSystem>>"

-- | Case insensitive lookup of all systems matching a prefix]
lookupTRSystem :: String -> [TRSystem t] -> [TRSystem t]
lookupTRSystem n = filter (\ s -> map toLower n `isPrefixOf` map toLower (sname s))

