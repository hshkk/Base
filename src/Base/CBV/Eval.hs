module Base.CBV.Eval where

import Base.Bin (bin)
import Base.Dom (restrict, dom)
import Base.Expr
import Base.CBV.Subst (subst, substs)
import Base.CBV.Val   (Env, Val(..))

import Data.List (foldl')

import qualified Data.Map as Map

evalP :: Env -> Prog -> Env
evalP r ss = foldl' evalS r [s | At s _ <- ss]

evalS :: Env -> Stmt -> Env
evalS r (Decl x p)      = subst r (x, Val VUn)
evalS r (x := (At e _)) = subst r (x, evalE r e)
evalS r (Proc f ps q) =
    let xs = map fst ps in subst r (f, VCls r xs q)
evalS r (Prog p) = restrict (evalP r p) (dom r)

-- the typechecker (Base.Type) should catch all errors that might happen in evaluation!
evalE :: Env -> Expr -> Val
evalE r (Var x)
    | Just v <- Map.lookup x r =
        case v of
            Val VUn -> error "Encountered a variable without an assignment"
            _       -> v
    | otherwise = error "Encountered an unbound variable"
evalE r (Con c) = Val (VCon c)
evalE r (Bin op (At e1 _) (At e2 _))
    | Val v1 <- evalE r e1
    , Val v2 <- evalE r e2 = Val (bin op v1 v2)
    | otherwise = error "Encountered a type mismatch when evaluating a binary operation"
evalE r (Inv f es)
    | Just (VCls s xs q) <- Map.lookup f r =
        let vs = [evalE r e | At e _ <- es] in evalB (substs s (zip xs vs)) q
    | otherwise = error "Encountered an invocation of a non-procedure"

evalB :: Env -> Body -> Val
evalB r0 (Ret ss (At e _)) =
    let rn = foldl' evalS r0 [s | At s _ <- ss] in evalE rn e