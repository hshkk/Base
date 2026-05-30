module Base.Type where

import Base.Expr (Var, Prim)

import Data.Map  (Map)

data Type
    = TPrim Prim
    | TProc [Type] Type
    deriving Eq

type TEnv = Map Var Type