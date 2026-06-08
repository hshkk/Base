module Base.Expr where

import Data.ByteString.Lazy.Char8 (ByteString)

type Var = ByteString
data Con 
    = I Int
    | B Bool

data Prim
    = Int
    | Bool
    deriving (Eq, Show)

data Bin
    = Add 
    | Sub 
    | Mul
    | Or  
    | And
    deriving Eq

data Posn = Posn !Int -- line number
                 !Int -- column number
data Tagged a = At a Posn

posnOf :: Tagged a -> Posn
posnOf (At _ p) = p

type Prog = [Tagged Stmt]

data Stmt
    = Decl Var Prim
    | Var := Tagged Expr
    | Proc Var [(Var, Prim)] Body
    | Prog Prog

data Body = Ret [Tagged Stmt] (Tagged Expr)

data Expr
    = Var Var
    | Con Con
    | Bin Bin (Tagged Expr) (Tagged Expr)
    | Inv Var [Tagged Expr]
