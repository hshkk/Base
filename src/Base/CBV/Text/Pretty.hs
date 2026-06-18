{-# LANGUAGE OverloadedStrings #-}
module Base.CBV.Text.Pretty where

import Base.CBV.Mem  (Mem)
import Base.Data.Val (Loc)

import Prettyprinter (Doc)

import qualified Base.Text.Pretty as Pretty (pprVal, pprVal')
import qualified Data.Map as Map

pprLoc :: Mem -> Loc -> Doc ann
pprLoc m l = maybe "?" (Pretty.pprVal (pprLoc m)) (Map.lookup l m)

pprLoc' :: Mem -> Loc -> Doc ann
pprLoc' m l = maybe "?" (Pretty.pprVal' (pprLoc' m)) (Map.lookup l m)