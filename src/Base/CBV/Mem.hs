module Base.CBV.Mem where

import Base.Val (Loc, Val)

import Data.Map (Map)

type Mem = Map Loc Val