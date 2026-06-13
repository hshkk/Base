module Base.CBV.Eval where

import Base.CBV.Mem (Mem)
import Base.Control.Dom
import Base.Control.Err
import Base.Data.Expr
import Base.Data.Val
import Base.Eval.Bin (bin)
import Base.Eval.Monad

import Control.Monad (foldM)

import qualified Data.Map as Map

type CBV a = Eval Mem a

evalP :: (Env, Mem) -> Prog -> CBV (Env, Mem)
evalP (r,m) = foldM evalS (r,m)

evalS :: (Env, Mem) -> Tagged Stmt -> CBV (Env, Mem)
evalS (r,m) (At (Decl x _) _) = do
    k <- fresh
    return (subst r (x,k), m)
evalS (r,m) (At (x := e) _)   
    | Just l <- Map.lookup x r = do
        (v,n) <- evalE (r,m) e
        return (r,subst n (l,v))
    | otherwise = undefined
evalS (r,m) (At (Proc f ps q) _) = do
    let xs = map fst ps
    k <- fresh
    return (subst r (f,k), subst m (k, VCls r xs q))
evalS (r,m) (At (Prog p) _) = do
    (_,n) <- evalP (r,m) p
    return (r, restrict n (ran r))

evalE :: (Env, Mem) -> Tagged Expr -> CBV (Val, Mem)
evalE (r,m) (At (Var x) (Posn l c))
    | Just k <- Map.lookup x r 
    , Just v <- Map.lookup k m = return (v,m)
    | otherwise = throw (buildError (Eval UnassignedVariable) l c (decode x))
evalE (_,m) (At (Con c) _) = return (VCon c, m)
evalE (r,m0) (At (Bin op e1 e2) _) = do
    (v1,m1) <- evalE (r,m0) e1
    (v2,m2) <- evalE (r,m1) e2
    return (bin op v1 v2, m2)
evalE (r,m) (At (Inv f es) _)
    | Just k <- Map.lookup f r 
    , Just (VCls s xs q) <- Map.lookup k m = do
        vs    <- mapM (evalE (r,m)) es
        ks    <- freshK (length es)
        (v,n) <- evalB (substs s (zip xs ks), substs m (zip ks (map fst vs))) q
        return (v, restrict n (dom m))
    | otherwise = undefined

evalB :: (Env, Mem) -> Body -> CBV (Val, Mem)
evalB (r0,m0) (Ret ss e) = do
    (rn,mn) <- foldM evalS (r0,m0) ss
    evalE (rn,mn) e