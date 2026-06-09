module Base.Eval.Monad where

import Base.Val (Loc)
import Base.Eval.Err (Error, Err)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State  (StateT (runStateT), get, put)

type Eval mem a = StateT Loc (Either Error) a

execute :: Eval mem a -> Err a
execute c = fmap fst (runStateT c 0)

fresh :: Eval mem Loc
fresh = do
    k <- get
    put (succ k)
    return (succ k)

freshK :: Int -> Eval mem [Loc]
freshK n = do
    k <- get
    put (succ (k+n))
    return (map succ [k..k+n])

throw :: Error -> Eval mem a
throw = lift . Left