{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
module Verse.TH
  ( verse
  , verseFile
  , parseQ
  ) where

import Control.Monad

import Data.String
import Data.Text (Text)
import Data.Text.IO qualified as Text

import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote (QuasiQuoter, pattern QuasiQuoter)
import Language.Haskell.TH.Quote qualified as TH
import Language.Haskell.TH.Syntax (addDependentFile)

import Pos

import Verse.Comp
import Verse.Exp
import Verse.Parse

verse :: QuasiQuoter
verse = QuasiQuoter
  { TH.quoteExp = comp <=< parseQ . fromString
  , TH.quotePat
  , TH.quoteType
  , TH.quoteDec
  }

verseFile :: QuasiQuoter
verseFile = QuasiQuoter
  { TH.quoteExp = \ x -> do
      addDependentFile x
      comp <=< parseQ <=< runIO $ Text.readFile x
  , TH.quotePat
  , TH.quoteType
  , TH.quoteDec
  }

parseQ :: Text -> Q LExp
parseQ input = case parse input of
  Left pos -> fail . show $ prettyParseError input pos
  Right x -> pure x

quotePat :: String -> Q TH.Pat
quotePat = const $ fail "No Verse quasi-quoter for patterns"

quoteType :: String -> Q TH.Type
quoteType = const $ fail "No Verse quasi-quoter for types"

quoteDec :: String -> Q [TH.Dec]
quoteDec = const $ fail "No Verse quasi-quoter for declarations"
