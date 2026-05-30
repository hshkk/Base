module Base.Test where

import Base.Err  hiding (line)
import Base.Expr hiding (Posn)
import Base.Type
import Base.Typechecker

import Base.Text.Lex
import Base.Text.Monad
import Base.Text.Par

import Data.Text (Text)
import Prettyprinter (line)
import Prettyprinter.Render.Text (putDoc)
import System.Exit (exitFailure)

import qualified Base.CBR.Eval as CBR
import qualified Base.CBR.Text.Pretty as CBR
import qualified Base.CBV.Eval as CBV
import qualified Base.CBV.Text.Pretty as CBV

import qualified Data.ByteString.Lazy.Char8 as Byte (readFile)
import qualified Data.Map as Map

abort :: FilePath -> Text -> Error -> IO a
abort f src e = do
    printError f src e
    exitFailure
    return undefined

alexLexTest :: FilePath -> IO [Token]
alexLexTest f = do
    g <- Byte.readFile f
    case runP g scanTokens of
        Left  e   -> abort f (decode g) e
        Right tks -> return tks

happyParseTest :: FilePath -> IO Prog
happyParseTest f = do
    g <- Byte.readFile f
    case runP g parseP of
        Left  e -> abort f (decode g) e
        Right p -> return p

typecheckTest :: FilePath -> IO TEnv
typecheckTest f = do
    g <- Byte.readFile f
    case runP g parseP of
        Left  e -> abort f (decode g) e
        Right p -> case checkP Map.empty p of
            Left  e -> abort f (decode g) e
            Right g -> return g

cbvEvalTest :: FilePath -> IO ()
cbvEvalTest f = do
    g <- Byte.readFile f
    case runP g parseP of
        Left  e -> abort f (decode g) e
        Right p -> case checkP Map.empty p of
            Left  e -> abort f (decode g) e
            Right _ -> case CBV.evalP Map.empty p of
                Left  e -> abort f (decode g) e
                Right r -> putDoc (CBV.pprEnv' r <> line)

cbrEvalTest :: FilePath -> IO ()
cbrEvalTest f = do
    g <- Byte.readFile f
    case runP g parseP of
        Left  e -> abort f (decode g) e
        Right p -> case checkP Map.empty p of
            Left  e -> abort f (decode g) e
            Right _ -> case CBR.evalP Map.empty p of
                Left  e -> abort f (decode g) e
                Right r -> putDoc (CBR.pprEnv' r <> line)