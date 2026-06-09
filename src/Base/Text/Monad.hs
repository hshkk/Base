module Base.Text.Monad where

import Base.Eval.Err (Error, Err)

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Int  (Int64)
import Data.Word (Word8)


data Posn = Posn !Int -- absolute character offset
                 !Int -- line number
                 !Int -- column number

startPosn :: Posn
startPosn = Posn 0 1 1

movePosn :: Posn -> Char -> Posn
movePosn (Posn a l c) '\t' = 
    let d = c + tabSize - (pred c `mod` tabSize) in
    Posn (succ a) l d
    where tabSize :: Int
          tabSize = 8
movePosn (Posn a l _) '\n' = Posn (succ a) (succ l) 1
movePosn (Posn a l c) _    = Posn (succ a) l (succ c)

type Byte = Word8

data Input = Input {
    inp_posn  :: !Posn,
    inp_bytes :: !Int64,
    inp       :: ByteString,
    inp_prev  :: !Char
    }

type Startcode = Int

data PState = PState {
    st_posn   :: !Posn,      -- the position of the parser
    st_bytes  :: !Int64,     -- the number of bytes consumed so far
    st_input  :: ByteString, -- the current input
    st_prev   :: !Char,      -- the preceding character
    st_stcd   :: !Startcode  -- the current startcode
    }

startPState :: ByteString -> PState
startPState s = PState { 
    st_posn   = startPosn,
    st_bytes  = 0,
    st_input  = s,
    st_prev   = '\n',
    st_stcd   = 0
    }

data PResult a 
    = Pass PState a
    | Fail Error

newtype P a = P { unP :: PState -> PResult a }

instance Functor P where
    fmap f x = P (\st -> 
        case unP x st of 
            Fail g     -> Fail g
            Pass st' y -> Pass st' (f y) 
        )

instance Applicative P where
    pure x  = P (\st -> Pass st x)
    f <*> x = P (\st -> 
        case unP f st of 
            Fail g     -> Fail g
            Pass st' y -> 
                case unP x st' of 
                    Fail g -> Fail g
                    Pass st'' z -> Pass st'' (y z) 
        )

instance Monad P where
    return  = pure
    x >>= f = P (\st ->
        case unP x st of
            Fail g     -> Fail g
            Pass st' y -> unP (f y) st'
        )

runP :: ByteString -> P a -> Err a
runP s (P f) = case f (startPState s) of
    Fail g   -> Left  g
    Pass _ x -> Right x

errP :: Error -> P a
errP = P . const . Fail

getInput :: P Input
getInput = P (\st@(PState {
                 st_posn  = p, 
                 st_bytes = b, 
                 st_input = i, 
                 st_prev  = v}) 
    -> Pass st (Input p b i v)
    )

setInput :: Input -> P ()
setInput (Input p b i v) = P (\st -> 
    let st' = st {
        st_posn  = p, 
        st_bytes = b, 
        st_input = i, 
        st_prev  = v} 
    in Pass st' ()
    )

getStcd :: P Startcode
getStcd = P (\st@(PState { st_stcd = c }) -> Pass st c)

setStcd :: Startcode -> P ()
setStcd c = P (\st -> 
    let st' = st { st_stcd = c } 
    in Pass st' () 
    )
