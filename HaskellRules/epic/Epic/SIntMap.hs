{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Epic.SIntMap (SIntMap, empty, lookup, member, insert, (!), lookupMax, delete
                    , toList, fromList, map, restrictKeys, keys, elems, null) where
import Prelude hiding (lookup, map, null)
import qualified Prelude
import Control.Arrow (first)
import Data.Coerce
import qualified Data.IntMap as M
import Epic.Print (Pretty (..))
import Epic.SIntSet (SIntSet, toIntSet)
import GHC.Stack (HasCallStack)

newtype SIntMap k v = SIntMap (M.IntMap v)
  deriving (Eq, Ord, Show)

instance forall k v. (Pretty k, Pretty v, Coercible k Int) => Pretty (SIntMap k v) where
  pPrintPrec l p (SIntMap m) = pPrintPrec l p $ Prelude.map (first (coerce :: Int -> k)) $ M.toList m

empty :: forall k v. (Coercible k Int) => SIntMap k v
empty = coerce (M.empty :: M.IntMap v)

lookup :: forall k v. (Coercible k Int) => k -> SIntMap k v -> Maybe v
lookup = coerce (M.lookup :: Int -> M.IntMap v -> Maybe v)

member :: forall k v. (Coercible k Int) => k -> SIntMap k v -> Bool
member = coerce (M.member :: Int -> M.IntMap v -> Bool)

(!) :: forall k v. (HasCallStack, Coercible k Int) => SIntMap k v -> k -> v
(!) = coerce ((M.!) :: M.IntMap v -> Int -> v)

insert :: forall k v. (Coercible k Int) => k -> v -> SIntMap k v -> SIntMap k v
insert = coerce (M.insert :: Int -> v -> M.IntMap v -> M.IntMap v)

lookupMax :: forall k v. (Coercible k Int) => SIntMap k v -> Maybe (k, v)
lookupMax = coerce (M.lookupMax :: M.IntMap v -> Maybe (Int, v))

delete :: forall k v. (Coercible k Int) => k -> SIntMap k v -> SIntMap k v
delete = coerce (M.delete :: Int -> M.IntMap v -> M.IntMap v)

fromList :: forall k v. (Coercible k Int) => [(k, v)] -> SIntMap k v
fromList = coerce (M.fromList :: [(Int, v)] -> M.IntMap v)

toList :: forall k v. (Coercible k Int) => SIntMap k v -> [(k, v)]
toList = coerce (M.toList :: M.IntMap v -> [(Int, v)])

map :: forall k v v'. (Coercible k Int) => (v -> v') -> SIntMap k v -> SIntMap k v'
map = coerce (M.map :: (v -> v') -> M.IntMap v -> M.IntMap v')

restrictKeys :: forall k v. (Coercible k Int) => SIntMap k v -> SIntSet k -> SIntMap k v
restrictKeys (SIntMap m) s = SIntMap $ M.restrictKeys m (toIntSet s)

keys :: forall k v . (Coercible k Int) => SIntMap k v -> [k]
keys (SIntMap m) = coerce (M.keys m)

elems :: forall k v . SIntMap k v -> [v]
elems (SIntMap m) = M.elems m

null :: forall k v . SIntMap k v -> Bool
null (SIntMap m) = M.null m
