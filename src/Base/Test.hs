module Base.Test where

import Base.Control.Err hiding (line)
import Base.Data.Expr
import Base.Data.Type
import Base.Data.Val
import Base.Eval.Monad (Eval, execute)
import Base.Eval.Typechecker (checkP)
import Base.Text.Lex
import Base.Text.Monad hiding (Posn)
import Base.Text.Par
import Base.Text.Pretty (pprEnv)

import Data.Text     (Text)
import Prettyprinter (Doc, line)
import Prettyprinter.Render.Text (putDoc)
import System.Exit   (exitFailure)

import qualified Base.CBN.Eval           as CBN
import qualified Base.CBN.Text.Pretty    as CBN
import qualified Base.CBNeed.Eval        as CBNeed
import qualified Base.CBNeed.Text.Pretty as CBNeed
import qualified Base.CBR.Eval           as CBR
import qualified Base.CBR.Text.Pretty    as CBR
import qualified Base.CBV.Eval           as CBV
import qualified Base.CBV.Text.Pretty    as CBV

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Map (Map)

import qualified Data.ByteString.Lazy.Char8 as Byte (readFile)
import qualified Data.Map as Map

abort :: FilePath -> Text -> Error -> IO a
abort f src e = do
    printError f src e
    _ <- exitFailure
    return undefined

testWith :: (ByteString -> Either Error a) -> FilePath -> IO a
testWith p f = do
    g <- Byte.readFile f
    either (abort f (decode g)) return (p g)

alexLexTest :: FilePath -> IO [Token]
alexLexTest = testWith (\g -> runP g scanTokens)

happyParseTest :: FilePath -> IO Prog
happyParseTest = testWith (\g -> runP g parseP)

typecheckTest :: FilePath -> IO TEnv
typecheckTest = testWith (\g -> runP g parseP >>= checkP Map.empty)

evalTest :: ((Env, Map Loc val) -> Prog -> Eval (Map Loc val) (Env, Map Loc val)) 
                                            -- ^ evaluation
         -> (Map Loc val -> Loc -> Doc ann) -- < prettyprinting locations
         -> FilePath
         -> IO ()
evalTest eval ppr f = do
    (r,m) <- testWith (\g -> do
        p <- runP g parseP
        _ <- checkP Map.empty p
        execute (eval (Map.empty, Map.empty) p)) f
    putDoc (pprEnv r (ppr m) <> line)

cbvEvalTest :: FilePath -> IO ()
cbvEvalTest = evalTest CBV.evalP CBV.pprLoc'

cbrEvalTest :: FilePath -> IO ()
cbrEvalTest = evalTest CBR.evalP CBR.pprLoc'

cbnEvalTest :: FilePath -> IO ()
cbnEvalTest = evalTest CBN.evalP CBN.pprLoc'

cbNeedEvalTest :: FilePath -> IO ()
cbNeedEvalTest = evalTest CBNeed.evalP CBNeed.pprLoc'