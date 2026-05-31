module Base.CBV.Eval where

import Base.Bin (bin)
import Base.Dom (restrict, dom)
import Base.Err
import Base.Expr
import Base.CBV.Subst (subst, substs)
import Base.CBV.Val   (Env, Val(..))

import Control.Monad (mapM, foldM)

import qualified Data.Map as Map

evalP :: Env -> Prog -> Err Env
evalP r ss = foldM evalS r ss

evalS :: Env -> Tagged Stmt -> Err Env
evalS r (At (Decl x p) _) = Right (subst r (x, Val VUn))
evalS r (At (x := e) _)   = do
    (v, s) <- evalE r e
    Right (subst s (x, v))
evalS r (At (Proc f ps q) _) =
    let xs = map fst ps in Right (subst r (f, VCls r xs q))
evalS r (At (Prog p) _) = do
    s <- evalP r p
    Right (restrict s (dom r))

evalE :: Env -> Tagged Expr -> Err (Val, Env)
evalE r (At (Var x) (Posn l c))
    | Just v <- Map.lookup x r =
        case v of
            Val VUn -> Left (buildError (Eval UnassignedVariable) l c (decode x))
            _       -> Right (v, r)
    | otherwise = undefined
evalE r (At (Con c) _) = Right (Val (VCon c), r)
evalE r (At (Bin op e1 e2) _) = do
    (w1,r1) <- evalE r e1
    (w2,r2) <- evalE r1 e2
    case (w1,w2) of
        (Val v1, Val v2) -> Right (Val (bin op v1 v2), r2)
        _ -> undefined
evalE r (At (Inv f es) _)
    | Just (VCls s xs q) <- Map.lookup f r = do
        vs    <- mapM (evalE r) es
        (v,z) <- evalB (substs s (zip xs (map fst vs))) q
        Right (v, substs r (Map.toList (restrict z (dom r))))
    | otherwise = undefined

evalB :: Env -> Body -> Err (Val, Env)
evalB r0 (Ret ss e) = do
    rn <- foldM evalS r0 ss
    evalE rn e