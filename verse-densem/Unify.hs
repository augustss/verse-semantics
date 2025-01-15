module Unify where

import Control.Monad.State
import qualified Data.Map as M
import Data.Map( Map )

--------------------------------------------------------------------------------

class Struct s where
  var   :: a -> s a
  isVar :: s a -> Maybe a
  unify :: s a -> s a -> Maybe (

type Heap s = Map Ident (s Ident)



