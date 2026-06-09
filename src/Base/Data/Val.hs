module Base.Data.Val where

import Base.Data.Expr

import Data.Map (Map)

type Loc = Int

data Val
    = VCon Con
    | VCls Env [Var] Body

type Env = Map Var Loc
