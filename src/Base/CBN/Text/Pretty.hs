{-# LANGUAGE OverloadedStrings #-}
module Base.CBN.Text.Pretty where

import Base.Expr
import Base.Val
import Base.CBN.Mem

import Prettyprinter

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