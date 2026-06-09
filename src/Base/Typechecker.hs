{-# LANGUAGE OverloadedStrings #-}
module Base.Typechecker where

import Base.Expr
import Base.Type
import Base.Eval.Err
import Base.Text.Pretty

import Control.Monad  (foldM)
import Data.Bifunctor (second)
import Data.List (find)

import qualified Data.Map as Map

checkP :: TEnv -> Prog -> Err TEnv
checkP g0 ss = do
    gn <- foldM checkS g0 ss
    return gn

checkS :: TEnv -> Tagged Stmt -> Err TEnv
checkS g (At (Decl x p) _) = Right (Map.insert x (TPrim p) g)
checkS g (At (x := e) (Posn l c))
    | Just b <- Map.lookup x g = do
        a <- checkE g e
        if a == b then Right g
        else Left (buildError (Type (MismatchedAssignment (decode x) b)) l c (render (pprType a)))
    | otherwise = Left (buildError (Type UndeclaredVariable) l c (decode x))
checkS g (At (Proc f ps q) _) = do
    a <- checkB (Map.union g (Map.fromList (map (second TPrim) ps))) q
    Right (Map.insert f (TProc (map (TPrim . snd) ps) a) g)
checkS g (At (Prog p) _) = do
    _ <- checkP g p
    Right g

checkE :: TEnv -> Tagged Expr -> Err Type
checkE g (At (Var x) (Posn l c))
    | Just a <- Map.lookup x g = Right a
    | otherwise = Left (buildError (Type UndeclaredVariable) l c (decode x))
checkE _ (At (Con (I _)) _) = Right (TPrim Int)
checkE _ (At (Con (B _)) _) = Right (TPrim Bool)
checkE g (At (Bin Add e1 e2) _) = do
    a1 <- checkE g e1
    a2 <- checkE g e2
    case (a1, a2) of
        (TPrim Int, TPrim Int) -> Right (TPrim Int)
        (TPrim Int, _) -> 
            let Posn l c = posnOf e2 in
            Left (buildError (Type (MismatchedOperand Add)) l c (render (pprType a2)))
        (_, _) -> 
            let Posn l c = posnOf e1 in
            Left (buildError (Type (MismatchedOperand Add)) l c (render (pprType a1)))
checkE g (At (Bin Sub e1 e2) _) = do
    a1 <- checkE g e1
    a2 <- checkE g e2
    case (a1, a2) of
        (TPrim Int, TPrim Int) -> Right (TPrim Int)
        (TPrim Int, _) -> 
            let Posn l c = posnOf e2 in
            Left (buildError (Type (MismatchedOperand Sub)) l c (render (pprType a2)))
        (_, _) -> 
            let Posn l c = posnOf e1 in
            Left (buildError (Type (MismatchedOperand Sub)) l c (render (pprType a1)))
checkE g (At (Bin Mul e1 e2) _) = do
    a1 <- checkE g e1
    a2 <- checkE g e2
    case (a1, a2) of
        (TPrim Int, TPrim Int) -> Right (TPrim Int)
        (TPrim Int, _) -> 
            let Posn l c = posnOf e2 in
            Left (buildError (Type (MismatchedOperand Mul)) l c (render (pprType a2)))
        (_, _) -> 
            let Posn l c = posnOf e1 in
            Left (buildError (Type (MismatchedOperand Mul)) l c (render (pprType a1)))
checkE g (At (Bin Or e1 e2) _) = do
    a1 <- checkE g e1
    a2 <- checkE g e2
    case (a1, a2) of
        (TPrim Bool, TPrim Bool) -> Right (TPrim Bool)
        (TPrim Bool, _) -> 
            let Posn l c = posnOf e2 in
            Left (buildError (Type (MismatchedOperand Or)) l c (render (pprType a2)))
        (_, _) -> 
            let Posn l c = posnOf e1 in
            Left (buildError (Type (MismatchedOperand Or)) l c (render (pprType a1)))
checkE g (At (Bin And e1 e2) _) = do
    a1 <- checkE g e1
    a2 <- checkE g e2
    case (a1, a2) of
        (TPrim Bool, TPrim Bool) -> Right (TPrim Bool)
        (TPrim Bool, _) -> 
            let Posn l c = posnOf e2 in
            Left (buildError (Type (MismatchedOperand And)) l c (render (pprType a2)))
        (_, _) -> 
            let Posn l c = posnOf e1 in
            Left (buildError (Type (MismatchedOperand And)) l c (render (pprType a1)))
checkE g (At (Inv f es) (Posn l c))
    | Right (TProc as b) <- checkE g (At (Var f) (Posn l c)) = do
        cs <- mapM (checkE g) es
        if as == cs then Right b
        else 
            let fstMatch xs = find (\(x, (y, _, _)) -> x /= y) xs in
            case fstMatch (zip as (zip3 cs es [1..])) of
                Just (a, (d, At _ (Posn li ci), n)) -> 
                    Left (buildError (Type (MismatchedArguments n a (decode f))) li ci (render (pprType d)))
                _ -> undefined
    | otherwise = Left (buildError (Type UndeclaredProcedure) l c (decode f))

checkB :: TEnv -> Body -> Err Type
checkB g0 (Ret ss e) = do
    gn <- foldM checkS g0 ss
    checkE gn e
