module Base.Bin where

import Base.Expr (Bin(..), Con(..))
import Base.Val  (Val(..))

bin :: Bin -> Val -> Val -> Val
bin Add (VCon (I n)) (VCon (I m)) = VCon (I (n + m))
bin Sub (VCon (I n)) (VCon (I m)) = VCon (I (n - m))
bin Mul (VCon (I n)) (VCon (I m)) = VCon (I (n * m))
bin Or  (VCon (B b)) (VCon (B c)) = VCon (B (b || c))
bin And (VCon (B b)) (VCon (B c)) = VCon (B (b && c))
bin _ _ _ = error "Encountered a type mismatch when evaluating a binary operation"