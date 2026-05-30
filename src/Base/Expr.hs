module Base.Expr where

import Data.ByteString.Lazy.Char8 (ByteString)

type Var = ByteString
data Con 
    = I Int
    | B Bool

data Prim
    = Int
    | Bool

data Bin
    = Add 
    | Sub 
    | Mul
    | Or  
    | And

type Prog = [Stmt]

data Stmt
    = Decl Var Prim
    | Var := Expr
    | Proc Var [(Var, Prim)] Body
    | Prog Prog

data Body = Ret [Stmt] Expr

data Expr
    = Var Var
    | Con Con
    | Bin Bin Expr Expr
    | Neg Expr          -- induced by integer constants...
    | Inv Var [Expr]

data Val0
    = VCon Con
    | VUn