{-# LANGUAGE OverloadedStrings #-}
module Base.Err where

import Base.Expr
import Base.Type (Type)
import Base.Text.Pretty

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import Errata (Errata, blockSimple, errataSimple, prettyErrors)
import Errata.Styles (fancyRedPointer, basicStyle)

import qualified Data.ByteString.Lazy.Char8 as Byte (toStrict)
import qualified Data.Text as Text 
import qualified Data.Text.Lazy.IO as Text (putStrLn)

type Err a = Either Error a

data ErrorType
    = Lexical
    | Syntax
    | Type TypeError
    deriving Eq

data TypeError
    = UndeclaredVariable
    | UndeclaredProcedure
    | MismatchedAssignment Text Type -- x : A
    | MismatchedOperand    Bin       -- ⊕ : Π × Π -> Π
    | MismatchedArguments  !Int Type Text -- nth argument : A of f
    deriving Eq

data Error = Error { 
    errorType  :: ErrorType,
    line       :: Int, 
    col        :: Int, 
    unexpected :: Text
    }
    deriving Eq

buildError :: ErrorType -> Int -> Int -> Text -> Error
buildError t l c u = Error {
    errorType = t,
    line = l, 
    col = c, 
    unexpected = u
    }

packageError :: FilePath -> Error -> Errata
packageError f (Error t l c u) = 
    errataSimple
        Nothing
        (blockSimple basicStyle fancyRedPointer f
            (Just ("error: " <> packageErrorType t))
            (l, c - Text.length u, c, Nothing)
            (packageErrorMsg t))
        Nothing
    where 
        packageErrorType Lexical  = "Lexical error"
        packageErrorType Syntax   = "Invalid syntax"
        packageErrorType (Type _) = "Mismatched types"
        packageErrorMsg  Lexical  = Nothing
        packageErrorMsg  Syntax   = Nothing
        packageErrorMsg  (Type UndeclaredVariable)
            = Just ("'" <> u <> "' hasn't been declared as a variable.")
        packageErrorMsg  (Type UndeclaredProcedure)
            = Just ("'" <> u <> "' hasn't been declared as a procedure.")
        packageErrorMsg  (Type (MismatchedAssignment x a))
            = Just ("'" <> x <> "' has type '" <> render (pprType a) 
                        <> "', so I can't assign it an expression of type '" <> u <> "'.")
        packageErrorMsg  (Type (MismatchedOperand op))
            = Just ("'" <> render (pprBin op) <> "' has type '" <> packageBin op 
                        <> "', but I received an operand of type '" <> u <> "'.")
            where 
                packageBin Add = "(Int, Int) -> Int"
                packageBin Sub = "(Int, Int) -> Int"
                packageBin Mul = "(Int, Int) -> Int"
                packageBin Or  = "(Bool, Bool) -> Bool"
                packageBin And = "(Bool, Bool) -> Bool"
        packageErrorMsg  (Type (MismatchedArguments n a f))
            = Just ("Parameter " <> Text.show n <> "of '" <> f <> "' has type '" <> render (pprType a) 
                                 <> "', but I received an argument of type '" <> u <> "'.")
        
printError :: FilePath -> Text -> Error -> IO ()
printError f src e = Text.putStrLn (prettyErrors src [packageError f e])

decode :: ByteString -> Text
decode = decodeLatin1 . Byte.toStrict