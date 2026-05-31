module Base.CBNa.Subst where

import Base.Expr    (Var)
import Base.CBNa.Val (Bind(..), Env)

import qualified Data.Map as Map

substs :: Env -> [(Var, Bind)] -> Env
substs r []         = r
substs r ((x,b):ws) = substs (subst r (x,b)) ws

subst :: Env -> (Var, Bind) -> Env
subst r (x, Eq v)    = Map.insert x (Eq v) r
subst r (x, Thunk e) = Map.insert x (Thunk e) r