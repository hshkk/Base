module Base.Control.Dom where

import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set

subst :: Ord k => Map k v -> (k,v) -> Map k v
subst m (k,v) = Map.insert k v m

substs :: Ord k => Map k v -> [(k,v)] -> Map k v
substs r []         = r
substs r ((x,v):ws) = substs (subst r (x,v)) ws

restrict :: Ord k => Map k v -> Set k -> Map k v
restrict = Map.restrictKeys

dom :: Map k v -> Set k
dom = Map.keysSet

ran :: Ord v => Map k v -> Set v
ran = Set.fromList . Map.elems
