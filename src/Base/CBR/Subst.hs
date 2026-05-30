module Base.CBR.Subst where 

import Base.Expr    (Var)
import Base.CBR.Val (Bind(..), Env)

import qualified Data.Map as Map

substs :: Env -> [(Var, Bind)] -> Env
substs r []         = r
substs r ((x,b):ws) = substs (subst r (x,b)) ws

subst :: Env -> (Var, Bind) -> Env
subst r (x, Eq v) = 
    case Map.lookup x r of
        Just (Ptr y) -> subst r (y, Eq v)
        _            -> Map.insert x (Eq v) r
subst r (x, Ptr y) = Map.insert x (Ptr y) r
