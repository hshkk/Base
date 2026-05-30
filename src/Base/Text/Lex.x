{
{-# LANGUAGE OverloadedStrings #-}
module Base.Text.Lex where

import Base.Err
import Base.Expr (Var)
import Base.Text.Monad

import Control.Monad (liftM)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Int  (Int64)
import Data.Map  (Map)
import Data.Text (Text)

import qualified Data.ByteString.Internal   as Byte (w2c)
import qualified Data.ByteString.Lazy       as Byte (uncons)
import qualified Data.ByteString.Lazy.Char8 as Byte (readInt, take)
import qualified Data.Map  as Map
import qualified Data.Text as Text (pack)
}

%encoding "iso-8859-1"

$c    = [A-Z\192-\221] # [\215] -- [c]apital ISO/IEC 8859-1 ∖ {\215 = ×}
$s    = [a-z\222-\255] # [\247] -- [s]mall   ISO/IEC 8859-1 ∖ {\247 = ÷}
$l    = [$c$s]                  -- [l]etter

$d    = [0-9]                   -- [d]igit
$i    = [$l$d\'\_]              -- [i]dentifier set
$u    = [.\n]                   -- [u]niversal set

@r    = ":"|"="|"+"|"-"|"*"|"||"|"&&"

@w    = $white+                 -- [w]hitespace

:-

<0>       @w+              ;

<0>       [\(\)\{\}\,\;]   {matchSpecial}

<0>       $d+              {matchInt}
<0>       "True"|"False"   {matchBool}

<0>       @r               {matchReservedOp}

<0>       $s$i*            {matchVarId}
<0>       $c$i*            {matchPrimId}

{

-- tokens

type Id = Var

data Token
    = TkVarId      Id
    | TkReservedId ReservedId
    | TkReservedOp ReservedOp
    | TkPrimId     PrimId
    | TkConstant   Constant
    | TkSpecial    Special
    | TkUnexpected Id
    | TkEOF

data ReservedId
    = RIProc  
    | RIReturn

data ReservedOp
    = ROColon
    | ROEquals
    | ROPlus
    | ROMinus
    | ROTimes
    | RODoublePipe
    | RODoubleAmpersand

data PrimId
    = PIInt
    | PIBool

data Special
    = SLParen | SRParen
    | SLBrace | SRBrace
    | SComma  
    | SSemicolon

data Constant
    = CInt  Int
    | CBool Bool

-- pretty-printing tokens

pprToken :: Token -> Text
pprToken (TkVarId x) = decode x
pprToken (TkReservedId ri) = case ri of
    RIProc   -> "proc"
    RIReturn -> "return"
pprToken (TkReservedOp ro) = case ro of
    ROColon           -> ":"
    ROEquals          -> "="
    ROPlus            -> "+"
    ROMinus           -> "-"
    ROTimes           -> "*"
    RODoublePipe      -> "||"
    RODoubleAmpersand -> "&&"
pprToken (TkPrimId pi) = case pi of
    PIInt  -> "Int"
    PIBool -> "Bool"
pprToken (TkConstant (CInt  n)) = Text.pack (show n)
pprToken (TkConstant (CBool b)) = Text.pack (show b)
pprToken (TkSpecial s) = case s of
    SLParen    -> "("
    SRParen    -> ")"
    SLBrace    -> "{"
    SRBrace    -> "}"
    SComma     -> ","
    SSemicolon -> ";"
pprToken (TkUnexpected u) = decode u
pprToken TkEOF = "end of file"

-- wrapper

type AlexInput = Input

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (Input _ _ _ v) = v

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte (Input p b i _) = 
    case Byte.uncons i of
        Nothing     -> Nothing
        Just (c, j) ->
            let d = Byte.w2c c
                q = movePosn p d
                a = succ b
            in q `seq` j `seq` a `seq` Just (c, Input q a j d)

alexLexicalError :: P a
alexLexicalError = do
    Input (Posn _ l c) _ _ _ <- getInput
    alexLexicalErrorWith l c 

alexLexicalErrorWith :: Int -> Int -> P a
alexLexicalErrorWith l c = 
    errP (buildError Lexical l c "")

scanToken :: P Token
scanToken = do
    i@(Input _ b _ _) <- getInput
    stcd <- getStcd
    case alexScan i stcd of
        AlexEOF -> return TkEOF
        AlexError (Input (Posn _ l c) _ _ _) -> alexLexicalErrorWith l c
        AlexSkip j _ -> do
            setInput j
            scanToken
        AlexToken j@(Input _ c _ _) _ a -> do
            setInput j
            a i (c - b)

scanTokens :: P [Token]
scanTokens = do
    tk <- scanToken
    case tk of
        TkEOF -> return [TkEOF]
        _     -> liftM (tk:) scanTokens

-- combinators

type AlexAction a = AlexInput -> Int64 -> P a

begin :: Startcode -> AlexInput -> Int64 -> P Token
begin c _ _ = do 
    setStcd c
    scanToken

token :: Token -> AlexAction Token
token = tokenWithString . const

tokenWithString :: (ByteString -> Token) -> AlexAction Token
tokenWithString f = \x l -> return (g x l)
    where g (Input _ _ x _) l = f (Byte.take l x)

-- actions

reservedId :: Map ByteString Token
reservedId = Map.fromList
    [ ("proc"  , TkReservedId RIProc)
    , ("return", TkReservedId RIReturn)
    ]

matchVarId :: AlexAction Token
matchVarId = tokenWithString match
    where 
        match s 
            | Just tk <- Map.lookup s reservedId = tk
            | otherwise = TkVarId s

primId :: Map ByteString Token
primId = Map.fromList
    [ ("Int" , TkPrimId PIInt)
    , ("Bool", TkPrimId PIBool)
    ]

matchPrimId :: AlexAction Token
matchPrimId = tokenWithString match
    where 
        match s 
            | Just tk <- Map.lookup s primId = tk
            | otherwise = TkUnexpected s

reservedOp :: Map ByteString Token
reservedOp = Map.fromList
    [ (":"  , TkReservedOp ROColon)
    , ("="  , TkReservedOp ROEquals)
    , ("+"  , TkReservedOp ROPlus)
    , ("-"  , TkReservedOp ROMinus)
    , ("*"  , TkReservedOp ROTimes)
    , ("||" , TkReservedOp RODoublePipe)
    , ("&&" , TkReservedOp RODoubleAmpersand)
    ]

matchReservedOp :: AlexAction Token
matchReservedOp = tokenWithString match
    where
        match s
            | Just tk <- Map.lookup s reservedOp = tk
            | otherwise = TkUnexpected s

matchSpecial :: AlexAction Token
matchSpecial = tokenWithString match
    where
        match "(" = TkSpecial SLParen
        match ")" = TkSpecial SRParen
        match "{" = TkSpecial SLBrace
        match "}" = TkSpecial SRBrace
        match "," = TkSpecial SComma
        match ";" = TkSpecial SSemicolon
        match s   = TkUnexpected s

matchInt :: AlexAction Token
matchInt (Input _ _ bs _) l =
    case Byte.readInt (Byte.take l bs) of
        Just (n, _) -> return (TkConstant (CInt n))
        Nothing     -> alexLexicalError

matchBool :: AlexAction Token
matchBool (Input _ _ bs _) l =
    case Byte.take l bs of
        "True"  -> return (TkConstant (CBool True))
        "False" -> return (TkConstant (CBool False))
        _       -> alexLexicalError
}
