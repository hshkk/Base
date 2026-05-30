module Base.CBR.Eval where

import Base.Bin (bin)
import Base.Dom (restrict, dom)
import Base.Expr
    (Prog, Stmt(..), Expr(..), Body(..), Val0(..))
import Base.CBR.Subst (subst, substs)
import Base.CBR.Val   (Env, Val(..))

import Data.List (foldl')

import qualified Data.Map as Map