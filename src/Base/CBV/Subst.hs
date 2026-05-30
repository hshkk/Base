module Base.CBV.Subst where 

import Base.Expr    (Var)
import Base.CBV.Val (Env, Val)

import qualified Data.Map as Map

substs :: Env -> [(Var, Val)] -> Env
substs r []         = r
substs r ((x,v):ws) = substs (subst r (x,v)) ws

subst :: Env -> (Var, Val) -> Env
subst r (x,v) = Map.alter (\_ -> Just v) x r