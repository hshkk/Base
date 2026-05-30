{-# LANGUAGE OverloadedStrings #-}
module Base.CBV.Text.Pretty where

import Base.Expr
import Base.CBV.Val
import Base.Text.Pretty (unpack, pprBody, pprVal0)

import Prettyprinter

import qualified Data.Map as Map

pprVal :: Val -> Doc ann
pprVal (Val v0)      = pprVal0 v0
pprVal (VCls r xs q) = pprCls r xs q

pprCls :: Env -> [Var] -> Body -> Doc ann
pprCls r xs q = 
    parens (pprEnv r <> comma <+> parens ((hsep . punctuate comma . map unpack) xs) <> pprBody q)

pprEnv :: Env -> Doc ann
pprEnv r
    | Map.null r = brackets ""
    | otherwise  =
        brackets (nest 3 (indent 3 ((vsep . punctuate comma . map pprBind) (Map.toList r))) <> line)
    where pprBind (x,v) = unpack x <+> "=" <+> pprVal v

-- hide contents of closures

pprVal' :: Val -> Doc ann
pprVal' (Val v0) = pprVal0 v0
pprVal' _        = parens ""

pprEnv' :: Env -> Doc ann
pprEnv' r
    | Map.null r = brackets ""
    | otherwise  =
        brackets (nest 3 (indent 3 ((vsep . punctuate comma . map pprBind) (Map.toList r))) <> line)
    where pprBind (x,v) = unpack x <+> "=" <+> pprVal' v