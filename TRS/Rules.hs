{-# OPTIONS_GHC -Wno-unused-matches -Wno-missing-signatures -Wno-name-shadowing -Wno-orphans -Wno-type-defaults -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
module TRS.Rules where

import TRS.TRS
import TRS.TRSCore

--------------------------------------------------------------------------------

type Context = Expr -> Expr

type ERule = Rule Expr

--------------------------------------------------------------------------------

data System t =
  System
  { rules                    :: Rule t
  , rulesHaveStructuralRules :: Bool
  , confluence               :: Rule t
  }

type ESystem = System Expr

--------------------------------------------------------------------------------

