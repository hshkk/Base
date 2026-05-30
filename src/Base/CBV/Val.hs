module Base.CBV.Val where

import Base.Expr (Var, Body, Val0)

import Data.Map  (Map)

data Val
    = Val  Val0
    | VCls Env [Var] Body

type Env = Map Var Val