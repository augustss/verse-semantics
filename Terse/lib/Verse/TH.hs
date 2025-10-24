module Verse.TH
  ( verse
  , parseQ
  ) where

import Control.Monad

import Data.String
import Data.Text (Text)

import Language.Haskell.TH (Q)
import Language.Haskell.TH.Quote

import Pos

import Verse.Comp
import Verse.Exp
import Verse.Parse

verse :: QuasiQuoter
verse = QuasiQuoter
  { quoteExp = comp <=< parseQ . fromString
  , quotePat = const $ fail "No Verse quasi-quoter for patterns"
  , quoteType = const $ fail "No Verse quasi-quoter for types"
  , quoteDec = const $ fail "No Verse quasi-quoter for declarations"
  }

parseQ :: Text -> Q LExp
parseQ input = case parse input of
  Left pos -> fail . show $ prettyParseError input pos
  Right x -> pure x
