module Base.CBNa.Val where

import Base.Expr (Var, Body, Val0, Expr)

import Data.Map  (Map)

data Val
    = Val  Val0
    | VCls Env [Var] Body

data Bind 
    = Eq    Val
    | Thunk Expr
type Env = Map Var Bind