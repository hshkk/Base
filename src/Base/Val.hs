module Base.Val where

import Base.Expr

import Data.Map (Map)

import qualified Data.Map as Map (insert, union, fromList)

type Loc = Int

data Val
    = VCon Con
    | VCls Env [Var] Body

type Env = Map Var Loc

subst :: Ord k => Map k v -> (k,v) -> Map k v
subst m (k,v) = Map.insert k v m

substs :: Ord k => Map k v -> [(k,v)] -> Map k v
substs m kvs = Map.union m (Map.fromList kvs)