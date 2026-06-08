module Base.Dom where

import Base.Val (subst)

import Data.Map  (Map)
import Data.Set  (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set

restrict :: Ord k => Map k v -> Set k -> Map k v
restrict = Map.restrictKeys

dom :: Map k v -> Set k
dom = Map.keysSet

substs :: Ord k => Map k v -> [(k,v)] -> Map k v
substs r []         = r
substs r ((x,v):ws) = substs (subst r (x,v)) ws

ran :: Ord v => Map k v -> Set v
ran = Set.fromList . Map.elems
