{-# LANGUAGE OverloadedStrings #-}
module Base.CBN.Text.Pretty where

import Base.CBN.Mem   (Bind(..), Mem)
import Base.Data.Expr (Tagged(At))
import Base.Data.Val  (Loc)

import Prettyprinter  (Doc)

import qualified Base.Text.Pretty as Pretty (pprVal, pprVal', pprExpr)
import qualified Data.Map as Map

pprLoc :: Mem -> Loc -> Doc ann
pprLoc m l = maybe "?" (pprBind m) (Map.lookup l m)

pprLoc' :: Mem -> Loc -> Doc ann
pprLoc' m l = maybe "?" (pprBind' m) (Map.lookup l m)

pprBind :: Mem -> Bind -> Doc ann
pprBind m (Eq v) = Pretty.pprVal (pprLoc m) v
pprBind _ (Thunk _ (At e _)) = Pretty.pprExpr e

pprBind' :: Mem -> Bind -> Doc ann
pprBind' m (Eq v) = Pretty.pprVal' (pprLoc' m) v
pprBind' _ (Thunk _ (At e _)) = Pretty.pprExpr e