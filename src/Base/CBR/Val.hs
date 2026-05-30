module Base.CBR.Val where

import Base.Expr (Var, Body, Val0)

import Data.Map  (Map)

data Val
    = Val  Val0
    | VCls Env [Var] Body

data Bind 
    = Eq  Val
    | Ptr Var
type Env = Map Var Bind