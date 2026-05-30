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
    v <- evalE r e
    Right (subst r (x, v))
evalS r (At (Proc f ps q) _) =
    let xs = map fst ps in Right (subst r (f, VCls r xs q))
evalS r (At (Prog p) _) = do
    s <- evalP r p
    Right (restrict s (dom r))

evalE :: Env -> Tagged Expr -> Err Val
evalE r (At (Var x) (Posn l c))
    | Just v <- Map.lookup x r =
        case v of
            Val VUn -> Left (buildError (Eval UnassignedVariable) l c (decode x))
            _       -> Right v
    | otherwise = undefined
evalE r (At (Con c) _) = Right (Val (VCon c))
evalE r (At (Bin op e1 e2) _)
    | Right (Val v1) <- evalE r e1
    , Right (Val v2) <- evalE r e2 = Right (Val (bin op v1 v2))
    | otherwise = undefined
evalE r (At (Inv f es) _)
    | Just (VCls s xs q) <- Map.lookup f r = do
        vs <- mapM (evalE r) es
        evalB (substs s (zip xs vs)) q
    | otherwise = undefined

evalB :: Env -> Body -> Err Val
evalB r0 (Ret ss e) = do
    rn <- foldM evalS r0 ss
    evalE rn e