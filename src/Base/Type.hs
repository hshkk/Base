module Base.Type where

import Base.Err (Err)
import Base.Expr

import Control.Monad  (foldM)
import Data.Bifunctor (second)
import Data.Map (Map)

import qualified Data.Map as Map

data Type
    = TPrim Prim
    | TProc [Type] Type
    deriving (Show, Eq)

type TEnv = Map Var Type

checkP :: TEnv -> Prog -> Err TEnv
checkP g0 ss = do
    gn <- foldM checkS g0 ss
    return gn

checkS :: TEnv -> Stmt -> Err TEnv
checkS g (Decl x p) = Right (Map.insert x (TPrim p) g)
checkS g (x := e)
    | Just b <- Map.lookup x g = do
        a <- checkE g e
        if a == b then Right g
        else Left "Encountered a mismatched type in assignment"
    | otherwise = Left "Cannot assign to undeclared variable"
checkS g (Proc f ps q) = do
    a <- checkB (Map.union g (Map.fromList (map (second TPrim) ps))) q
    Right (Map.insert f (TProc (map (TPrim . snd) ps) a) g)
checkS g (Prog p) = do
    d <- checkP g p
    Right g

checkE :: TEnv -> Expr -> Err Type
checkE g (Var x)
    | Just a <- Map.lookup x g = Right a
    | otherwise = Left "Encountered an unbound variable"
checkE g (Con (I _)) = Right (TPrim Int)
checkE g (Con (B _)) = Right (TPrim Bool)
checkE g (Bin Add e1 e2)
    | Right (TPrim Int) == checkE g e1
    , Right (TPrim Int) == checkE g e2 = Right (TPrim Int)
    | otherwise = Left "Encountered a type mismatch when evaluating addition"
checkE g (Bin Sub e1 e2)
    | Right (TPrim Int) == checkE g e1
    , Right (TPrim Int) == checkE g e2 = Right (TPrim Int)
    | otherwise = Left "Encountered a type mismatch when evaluating subtraction"
checkE g (Bin Mul e1 e2)
    | Right (TPrim Int) == checkE g e1
    , Right (TPrim Int) == checkE g e2 = Right (TPrim Int)
    | otherwise = Left "Encountered a type mismatch when evaluating multiplication"
checkE g (Bin Or  e1 e2)
    | Right (TPrim Bool) == checkE g e1
    , Right (TPrim Bool) == checkE g e2 = Right (TPrim Bool)
    | otherwise = Left "Encountered a type mismatch when evaluating disjunction"
checkE g (Bin And e1 e2)
    | Right (TPrim Bool) == checkE g e1
    , Right (TPrim Bool) == checkE g e2 = Right (TPrim Bool)
    | otherwise = Left "Encountered a type mismatch when evaluating conjunction"
checkE g (Inv f es)
    | Right (TProc as b) <- checkE g (Var f) = do
        cs <- mapM (checkE g) es
        if as == cs then Right b
        else Left "Encountered mismatched argument types"
    | otherwise = Left "Cannot invoke undeclared procedure"

checkB :: TEnv -> Body -> Err Type
checkB g0 (Ret ss e) = do
    gn <- foldM checkS g0 ss
    checkE gn e