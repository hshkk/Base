module Base.Dom where

import Data.Map  (Map)
import Data.Set  (Set)

import qualified Data.Map as Map

restrict :: Ord k => Map k v -> Set k -> Map k v
restrict = Map.restrictKeys

dom :: Map k v -> Set k
dom = Map.keysSet