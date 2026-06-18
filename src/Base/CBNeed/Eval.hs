module Base.CBNeed.Eval where

import Base.CBNeed.Mem  (Mem, Bind(..))
import Base.Control.Dom (dom, ran, restrict, subst, substs)
import Base.Control.Err
    (buildError,
     decode,
     ErrorType(Eval),
     EvalError(UnassignedVariable))
import Base.Data.Expr
    (Body(..), 
     Expr(..), 
     Posn(Posn), 
     Prog, 
     Stmt(..), 
     Tagged(..))
import Base.Data.Val (Env, Val(..))
import Base.Eval.Bin (bin)
import Base.Eval.Monad (fresh, freshK, throw, Eval)

import Control.Monad (foldM)

import qualified Data.Map as Map

type CBNeed a = Eval Mem a

evalP :: (Env, Mem) -> Prog -> CBNeed (Env, Mem)
evalP (r,m) = foldM evalS (r,m)

evalS :: (Env, Mem) -> Tagged Stmt -> CBNeed (Env, Mem)
evalS (r,m) (At (Decl x _) _) = do
    k <- fresh
    return (subst r (x,k), m)
evalS (r,m) (At (x := e) _)   
    | Just l <- Map.lookup x r = do
        (v,n) <- evalE (r,m) e
        return (r,subst n (l, Eq v))
    | otherwise = undefined
evalS (r,m) (At (Proc f ps q) _) = do
    let xs = map fst ps
    k <- fresh
    return (subst r (f,k), subst m (k, Eq (VCls r xs q)))
evalS (r,m) (At (Prog p) _) = do
    (_,n) <- evalP (r,m) p
    return (r, restrict n (ran r))

evalE :: (Env, Mem) -> Tagged Expr -> CBNeed (Val, Mem)
evalE (r,m) (At (Var x) (Posn l c))
    | Just k <- Map.lookup x r 
    , Just b <- Map.lookup k m = case b of
        Eq v -> return (v,m)
        Thunk s e -> do
            (w,_) <- evalE (s,m) e
            return (w, subst m (k, Eq w))
    | otherwise = throw (buildError (Eval UnassignedVariable) l c (decode x))
evalE (_,m) (At (Con c) _) = return (VCon c, m)
evalE (r,m0) (At (Bin op e1 e2) _) = do
    (v1,m1) <- evalE (r,m0) e1
    (v2,m2) <- evalE (r,m1) e2
    return (bin op v1 v2, m2)
evalE (r,m) (At (Inv f es) _)
    | Just k <- Map.lookup f r 
    , Just (Eq (VCls s xs q)) <- Map.lookup k m = do
        ks    <- freshK (length es)
        (v,n) <- evalB (substs s (zip xs ks), substs m (zip ks (map (Thunk r) es))) q
        return (v, restrict n (dom m))
    | otherwise = undefined

evalB :: (Env, Mem) -> Body -> CBNeed (Val, Mem)
evalB (r0,m0) (Ret ss e) = do
    (rn,mn) <- foldM evalS (r0,m0) ss
    evalE (rn,mn) e