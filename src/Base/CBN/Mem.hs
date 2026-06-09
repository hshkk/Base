module Base.CBN.Mem where

import Base.Data.Expr (Expr, Tagged)
import Base.Data.Val  (Loc, Val, Env)

import Data.Map (Map)

data Bind 
    = Eq Val
    | Thunk Env (Tagged Expr)

type Mem = Map Loc Bind