module Base.CBR.Eval where

import Base.Bin (bin)
import Base.Dom (restrict, dom)
import Base.Err
import Base.Expr
import Base.CBR.Subst (subst, substs)
import Base.CBR.Val

import Control.Monad (mapM, foldM)

import qualified Data.Map as Map
import Base.CBR.Text.Pretty (pprEnv')
import Prettyprinter.Render.String (renderString)
import Prettyprinter (layoutPretty, defaultLayoutOptions)

evalP :: Env -> Prog -> Err Env
evalP r ss = foldM evalS r ss

evalS :: Env -> Tagged Stmt -> Err Env
evalS r (At (Decl x p) _) = Right (subst r (x, Eq (Val VUn)))
evalS r (At (x := e) _)   = do
    (v, s) <- evalE r e
    Right (subst s (x, Eq v))
evalS r (At (Proc f ps q) _) =
    let xs = map fst ps in Right (subst r (f, Eq (VCls r xs q)))
evalS r (At (Prog p) _) = do
    s <- evalP r p
    Right (restrict s (dom r))

evalE :: Env -> Tagged Expr -> Err (Val, Env)
evalE r (At (Var x) (Posn l c))
    | Just b <- Map.lookup x r =
        case b of
            Eq (Val VUn) -> Left (buildError (Eval UnassignedVariable) l c (decode x))
            Eq  v        -> Right (v, r)
            Ptr y        -> evalE r (At (Var y) (Posn l c))
    | otherwise = undefined
evalE r (At (Con c) _) = Right (Val (VCon c), r)
evalE r (At (Bin op e1 e2) _)
    | Right (Val v1, r1) <- evalE r e1
    , Right (Val v2, r2) <- evalE r1 e2 = Right (Val (bin op v1 v2), r2)
    | otherwise = undefined
evalE r (At (Inv f es) (Posn l c))
    | Just (Eq (VCls s xs q)) <- Map.lookup f r = 
        case mapM (\(At e _) -> case e of {Var y -> Just y; _ -> Nothing}) es of
            Just ys -> do
                vs    <- fmap (map (Eq . fst)) (mapM (evalE r) es)
                (v,z) <- evalB (substs s (zip xs (map Ptr ys) ++ zip ys vs)) q
                Right (v, substs r (Map.toList (restrict z (dom r))))
            Nothing -> Left (buildError (Eval NonvariableArguments) l c (decode f))
    | otherwise = undefined

evalB :: Env -> Body -> Err (Val, Env)
evalB r0 (Ret ss e) = do
    rn <- foldM evalS r0 ss
    evalE rn e