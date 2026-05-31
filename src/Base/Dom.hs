module Base.Dom where

import Base.Expr

import Data.Map  (Map)
import Data.Set  (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set

restrict :: Ord k => Map k v -> Set k -> Map k v
restrict = Map.restrictKeys

dom :: Map k v -> Set k
dom = Map.keysSet

fv :: Tagged Expr -> Set Var
fv (At (Var x) _) = Set.singleton x
fv (At (Con _) _) = Set.empty
fv (At (Bin _ e1 e2) _) = 
    Set.union (fv e1) (fv e2)
fv (At (Inv f es) _)    = 
    Set.union (Set.singleton f) (Set.unions (map fv es))