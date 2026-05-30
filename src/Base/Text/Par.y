{
{-# LANGUAGE OverloadedStrings #-}
module Base.Text.Par where

import Base.Err
import Base.Expr hiding (Posn)
import Base.Text.Lex
import Base.Text.Monad

import qualified Base.Expr as Expr (Posn(..))
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

stmts :: {[Tagged Stmt]}
    : stmts0         {reverse $1}

stmts0 :: {[Tagged Stmt]}
    : {- ε -}        {[]}
    | stmts0 stmt ';' 
                     {$2:$1}
    | stmts0 inst ';' 
                     {$2 ++ $1}

inst :: {[Tagged Stmt]} -- x : Π = e
    : var ':' prim '=' expr 
        {% do
            (Input (Posn _ l c) _ _ _) <- getInput
            let p = Expr.Posn l c
            return [At ($1 := $5) p, At (Decl $1 $3) p]
        }

stmt :: {Tagged Stmt}
    : var ':' prim    {% tag (Decl $1 $3)}
    | var '=' expr    {% tag ($1 := $3)}
    | 'proc' var '(' pars ')' body 
                      {% tag (Proc $2 $4 $6)}
    | prog            {% tag (Prog $1)}

pars :: {[(Var, Prim)]}
    : pars0          {reverse $1}

pars0 :: {[(Var, Prim)]}
    : {- ε -}        {[]}
    | var ':' prim           
                     {[($1,$3)]}
    | pars0 ',' var ':' prim 
                     {($3,$5):$1}

expr :: {Tagged Expr}
    : var            {% tag (Var $1)}
    | con            {% tag (Con $1)}
    | expr '+'  expr {% tag (Bin Add $1 $3)}
    | expr '-'  expr {% tag (Bin Sub $1 $3)}
    | expr '*'  expr {% tag (Bin Mul $1 $3)}
    | expr '||' expr {% tag (Bin Or  $1 $3)}
    | expr '&&' expr {% tag (Bin And $1 $3)}
    | var '(' args ')'
                     {% tag (Inv $1 $3)}
    | '(' expr ')'   {$2}

args :: {[Tagged Expr]}
    : args0          {reverse $1}

args0 :: {[Tagged Expr]}
    : {- ε -}        {[]}
    | expr           {[$1]}
    | args0 ',' expr {$3:$1}

body :: {Body}
    : '{' stmts 'return' expr ';' '}'
                     {Ret $2 $4}

{
tag :: a -> P (Tagged a)
tag a = do
    (Input (Posn _ l c) _ _ _) <- getInput
    return (At a (Expr.Posn l c))

happyParseError :: Token -> P a
happyParseError tk = do
    (Input (Posn _ l c) _ _ _) <- getInput
    errP (msg l c)
    where 
        msg l c = buildError Syntax l c (pprToken tk)

lexer :: (Token -> P a) -> P a
lexer = \f -> scanToken >>= f
}