{-# LANGUAGE OverloadedStrings #-}
module Base.Text.Pretty where

import Base.Expr
import Base.Type

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text (Text)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import qualified Data.ByteString.Lazy.Char8 as Byte (unpack)

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

pprVal0 :: Val0 -> Doc ann
pprVal0 (VCon c) = pprCon c
pprVal0 VUn      = "?"

pprType :: Type -> Doc ann
pprType (TPrim p)    = pprPrim p
pprType (TProc as b) = 
    parens (hsep (punctuate comma (map pprType as))) <+> "->" <+> pprType b
