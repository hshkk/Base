module Base.Test where

import Base.Expr
import Base.Type
import Base.Typechecker

import Base.Eval.Err  hiding (line)

import Base.Text.Lex
import Base.Text.Monad hiding (Posn)
import Base.Text.Par
import Base.Text.Pretty (pprEnv)

import Data.Text (Text)
import Prettyprinter (line)
import Prettyprinter.Render.Text (putDoc)
import System.Exit (exitFailure)

import qualified Base.CBN.Eval as CBN
import qualified Base.CBN.Text.Pretty as CBN
import qualified Base.CBNeed.Eval as CBNeed
import qualified Base.CBNeed.Text.Pretty as CBNeed
import qualified Base.CBR.Eval as CBR
import qualified Base.CBR.Text.Pretty  as CBR
import qualified Base.CBV.Eval as CBV
import qualified Base.CBV.Text.Pretty  as CBV

import qualified Data.ByteString.Lazy.Char8 as Byte (readFile)
import qualified Data.Map as Map

abort :: FilePath -> Text -> Error -> IO a
abort f src e = do
    printError f src e
    _ <- exitFailure
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
            Right h -> return h

cbvEvalTest :: FilePath -> IO ()
cbvEvalTest f = do
    g <- Byte.readFile f
    case runP g parseP of
        Left  e -> abort f (decode g) e
        Right p -> case checkP Map.empty p of
            Left  e -> abort f (decode g) e
            Right _ -> case execute (CBV.evalP (Map.empty, Map.empty) p) of
                Left  e -> abort f (decode g) e
                Right (r,m) -> putDoc (pprEnv r (CBV.pprLoc' m) <> line)

cbrEvalTest :: FilePath -> IO ()
cbrEvalTest f = do
    g <- Byte.readFile f
    case runP g parseP of
        Left  e -> abort f (decode g) e
        Right p -> case checkP Map.empty p of
            Left  e -> abort f (decode g) e
            Right _ -> case execute (CBR.evalP (Map.empty, Map.empty) p) of
                Left  e -> abort f (decode g) e
                Right (r,m) -> putDoc (pprEnv r (CBR.pprLoc' m) <> line)

cbnEvalTest :: FilePath -> IO ()
cbnEvalTest f = do
    g <- Byte.readFile f
    case runP g parseP of
        Left  e -> abort f (decode g) e
        Right p -> case checkP Map.empty p of
            Left  e -> abort f (decode g) e
            Right _ -> case execute (CBN.evalP (Map.empty, Map.empty) p) of
                Left  e -> abort f (decode g) e
                Right (r,m) -> putDoc (pprEnv r (CBN.pprLoc' m) <> line)

cbneedEvalTest :: FilePath -> IO ()
cbneedEvalTest f = do
    g <- Byte.readFile f
    case runP g parseP of
        Left  e -> abort f (decode g) e
        Right p -> case checkP Map.empty p of
            Left  e -> abort f (decode g) e
            Right _ -> case execute (CBNeed.evalP (Map.empty, Map.empty) p) of
                Left  e -> abort f (decode g) e
                Right (r,m) -> putDoc (pprEnv r (CBNeed.pprLoc' m) <> line)