{-# LANGUAGE OverloadedStrings #-}
module Base.Text.Pretty where

import Base.Data.Expr
    (Bin(..),
     Body(..),
     Con(..),
     Expr(..),
     Prim(..),
     Prog,
     Stmt(..),
     Tagged(At),
     Var)
import Base.Data.Type (Type(..))
import Base.Data.Val  (Val(..), Env, Loc)

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text (Text)
import Prettyprinter
    (Doc,
     (<+>),
     defaultLayoutOptions,
     hsep,
     indent,
     layoutPretty,
     line,
     nest,
     punctuate,
     vsep,
     braces,
     brackets,
     colon,
     comma,
     equals,
     parens,
     semi,
     Pretty(pretty))
import Prettyprinter.Render.Text (renderStrict)

import qualified Data.ByteString.Lazy.Char8 as Byte (unpack)
import qualified Data.Map    as Map

unpack :: ByteString -> Doc ann
unpack = pretty . Byte.unpack

render :: Doc ann -> Text
render = renderStrict . layoutPretty defaultLayoutOptions

pprProg :: Prog -> Doc ann
pprProg ss = 
    braces (nest 3 (indent 3 (vsep [pprStmt s | At s _ <- ss])) <> line)

pprStmt :: Stmt -> Doc ann
pprStmt (Decl x p)    = 
    unpack x <+> colon <+> pprPrim p <> semi
pprStmt (x := At e _)      = 
    unpack x <+> equals <+> pprExpr e <> semi
pprStmt (Proc f ps q) = 
    "proc" <+> unpack f <> parens (pprPars ps) <> pprBody q <> semi
pprStmt (Prog p)      = 
    pprProg p <> semi

pprPars :: [(Var, Prim)] -> Doc ann
pprPars = 
    let pprPar (x,p) = unpack x <+> colon <+> pprPrim p in
    hsep . punctuate comma . map pprPar

pprBody :: Body -> Doc ann
pprBody (Ret ss (At e _)) =
    let pprRet = "return" <+> pprExpr e <> semi in
    braces (nest 4 (line <> vsep ([pprStmt s | At s _ <- ss] ++ [pprRet])) <> line)

pprExpr :: Expr -> Doc ann
pprExpr (Var x)       = unpack x
pprExpr (Con c)       = pprCon c
pprExpr (Bin b (At e1 _) (At e2 _)) = 
    pprExpr e1 <+> pprBin b <+> pprExpr e2
pprExpr (Inv f es)    = 
    unpack f <> parens (hsep (punctuate comma [pprExpr e | At e _ <- es]))

pprCon :: Con -> Doc ann
pprCon (I n) = pretty n
pprCon (B b) = pretty b

pprPrim :: Prim -> Doc ann
pprPrim Int  = "Int"
pprPrim Bool = "Bool"

pprBin :: Bin -> Doc ann
pprBin Add = "+"
pprBin Sub = "-"
pprBin Mul = "*"
pprBin Or  = "||"
pprBin And = "&&"

pprVal :: (Loc -> Doc ann) -> Val -> Doc ann
pprVal _ (VCon c)      = pprCon c
pprVal k (VCls r xs q) = pprCls r k xs q

pprVal' :: (Loc -> Doc ann) -> Val -> Doc ann
pprVal' _ (VCon c) = pprCon c
pprVal' _ _        = parens ""

pprCls :: Env -> (Loc -> Doc ann) -> [Var] -> Body -> Doc ann
pprCls r k xs q =
    parens (pprEnv r k <> comma <+> parens ((hsep . punctuate comma . map unpack) xs) <> pprBody q)

pprEnv :: Env -> (Loc -> Doc ann) -> Doc ann
pprEnv r k
    | Map.null r = brackets ""
    | otherwise  =
        brackets (nest 3 (indent 3 ((vsep . punctuate comma . map pprBind) (Map.toList r))) <> line)
    where pprBind (x, l) = unpack x <+> "=" <+> k l

pprType :: Type -> Doc ann
pprType (TPrim p)    = pprPrim p
pprType (TProc as b) = 
    parens (hsep (punctuate comma (map pprType as))) <+> "->" <+> pprType b
