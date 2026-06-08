module Base.Dom where

import Data.Map  (Map)
import Data.Set  (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set

restrict :: Ord k => Map k v -> Set k -> Map k v
restrict = Map.restrictKeys

dom :: Map k v -> Set k
dom = Map.keysSet

ran :: Ord v => Map k v -> Set v
ran = Set.fromList . Map.elems
