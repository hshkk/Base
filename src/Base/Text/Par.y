{
module Base.Text.Par where

import Base.Expr
import Base.Text.Lex
import Base.Text.Monad

import qualified Data.ByteString.Lazy.Char8 as Byte (readFile)
}

%expect 0

%name parseP prog
%name parseS stmt
%name parseE expr

%monad {P} {(>>=)} {return}
%lexer {lexer} {TkEOF}
%error {happyParseError}

%tokentype {Token}
%token
    var       {TkVarId $$}

    'proc'    {TkReservedId RIProc}
    'return'  {TkReservedId RIReturn}

    ':'       {TkReservedOp ROColon}
    '='       {TkReservedOp ROEquals}
    '+'       {TkReservedOp ROPlus}
    '-'       {TkReservedOp ROMinus}
    '*'       {TkReservedOp ROTimes}
    '||'      {TkReservedOp RODoublePipe}
    '&&'      {TkReservedOp RODoubleAmpersand}

    'Int'     {TkPrimId PIInt}
    'Bool'    {TkPrimId PIBool}

    int       {TkConstant (CInt  $$)}
    bool      {TkConstant (CBool $$)}

    '('       {TkSpecial SLParen}
    ')'       {TkSpecial SRParen}
    '{'       {TkSpecial SLBrace}
    '}'       {TkSpecial SRBrace}
    ','       {TkSpecial SComma}
    ';'       {TkSpecial SSemicolon}

%right '||'
%right '&&'
%left  '+' '-'
%left  '*'

%%

con :: {Con}
    : int            {I $1}
    | bool           {B $1}

prim :: {Prim}
    : 'Int'          {Int}
    | 'Bool'         {Bool}

prog :: {Prog}
    : '{' stmts '}'  {$2}

stmts :: {[Stmt]}
    : stmts0         {reverse $1}

stmts0 :: {[Stmt]}
    : {- ε -}        {[]}
    | stmts0 stmt ';' {$2:$1}

stmt :: {Stmt}
    : var ':' prim   {Decl $1 $3}
    | var '=' expr   {$1 := $3}
    | 'proc' var '(' pars ')' body 
                     {Proc $2 $4 $6}
    | prog           {Prog $1}

pars :: {[(Var, Prim)]}
    : pars0          {reverse $1}

pars0 :: {[(Var, Prim)]}
    : {- ε -}        {[]}
    | pars0 ',' var ':' prim 
                     {($3,$5):$1}

expr :: {Expr}
    : var            {Var $1}
    | con            {Con $1}
    | expr '+'  expr {Bin Add $1 $3}
    | expr '-'  expr {Bin Sub $1 $3}
    | expr '*'  expr {Bin Mul $1 $3}
    | expr '||' expr {Bin Or  $1 $3}
    | expr '&&' expr {Bin And $1 $3}
    | var '(' args ')'
                     {Inv $1 $3}
    | '(' expr ')'   {$2}

args :: {[Expr]}
    : args0          {reverse $1}

args0 :: {[Expr]}
    : {- ε -}        {[]}
    | args0 ',' expr 
                     {$3:$1}

body :: {Body}
    : '{' stmts 'return' expr ';' '}'
                     {Ret $2 $4}

{
happyParseTest :: FilePath -> IO Prog
happyParseTest f = do
    g <- Byte.readFile f
    case runP g parseP of
        Left  e -> ioError (userError e)
        Right p -> return p

happyParseError :: Token -> P a
happyParseError _ = do
    (Inp (Posn _ l c) _ _ _) <- getInp
    errP (msg l c)
    where 
        msg l c = 
            (showString "parse error at line " .
            shows l                           .
            showString ", column "            .
            shows c) ""

lexer :: (Token -> P a) -> P a
lexer = \f -> scanToken >>= f
}