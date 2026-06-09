{
{-# LANGUAGE OverloadedStrings #-}
module Base.Text.Par where

import Base.Expr hiding (Posn)
import Base.Eval.Err
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
    var       {TkVarId      _ _}

    'proc'    {TkReservedId _ RIProc}
    'return'  {TkReservedId _ RIReturn}

    ':'       {TkReservedOp _ ROColon}
    '='       {TkReservedOp _ ROEquals}
    '+'       {TkReservedOp _ ROPlus}
    '-'       {TkReservedOp _ ROMinus}
    '*'       {TkReservedOp _ ROTimes}
    '||'      {TkReservedOp _ RODoublePipe}
    '&&'      {TkReservedOp _ RODoubleAmpersand}

    'Int'     {TkPrimId     _ PIInt}
    'Bool'    {TkPrimId     _ PIBool}

    int       {TkConstant   _ (CInt  _)}
    bool      {TkConstant   _ (CBool _)}

    '('       {TkSpecial    _ SLParen}
    ')'       {TkSpecial    _ SRParen}
    '{'       {TkSpecial    _ SLBrace}
    '}'       {TkSpecial    _ SRBrace}
    ','       {TkSpecial    _ SComma}
    ';'       {TkSpecial    _ SSemicolon}

%right '||'
%right '&&'
%left  '+' '-'
%left  '*'

%%

con :: {Tagged Con}
    : int             {At (I (tkInt  $1)) (tkPosn $1)}
    | bool            {At (B (tkBool $1)) (tkPosn $1)}

prim :: {Prim}
    : 'Int'           {Int}
    | 'Bool'          {Bool}

prog :: {Prog}
    : '{' stmts '}'   {$2}

stmts :: {[Tagged Stmt]}
    : stmts0          {reverse $1}

stmts0 :: {[Tagged Stmt]}
    : {- ε -}         {[]}
    | stmts0 stmt ';' 
                      {$2:$1}
    | stmts0 inst ';' 
                      {$2 ++ $1}

inst :: {[Tagged Stmt]} -- x : Π = e
    : var ':' prim '=' expr 
                      {let p = tkPosn $1
                       in [At (tkVar $1 := $5) p, At (Decl (tkVar $1) $3) p]}

stmt :: {Tagged Stmt}
    : var ':' prim    {At (Decl (tkVar $1) $3) (tkPosn $1)}
    | var '=' expr    {At (tkVar $1 := $3)     (tkPosn $1)}
    | 'proc' var '(' pars ')' body 
                      {At (Proc (tkVar $2) $4 $6) (tkPosn $1)}
    | tprog           {$1}

tprog :: {Tagged Stmt}
    : '{' stmts '}'   {At (Prog $2) (tkPosn $1)}

pars :: {[(Var, Prim)]}
    : pars0           {reverse $1}

pars0 :: {[(Var, Prim)]}
    : {- ε -}         {[]}
    | var ':' prim           
                      {[(tkVar $1,$3)]}
    | pars0 ',' var ':' prim 
                      {(tkVar $3,$5):$1}

expr :: {Tagged Expr}
    : var             {At (Var (tkVar $1))  (tkPosn $1)}
    | con             {let At c p = $1 in At (Con c) p}
    | expr '+'  expr  {At (Bin Add $1 $3)  (tkPosn $2)}
    | expr '-'  expr  {At (Bin Sub $1 $3)  (tkPosn $2)}
    | expr '*'  expr  {At (Bin Mul $1 $3)  (tkPosn $2)}
    | expr '||' expr  {At (Bin Or  $1 $3)  (tkPosn $2)}
    | expr '&&' expr  {At (Bin And $1 $3)  (tkPosn $2)}
    | var '(' args ')'
                      {At (Inv (tkVar $1) $3) (tkPosn $1)}
    | '(' expr ')'    {$2}

args :: {[Tagged Expr]}
    : args0           {reverse $1}

args0 :: {[Tagged Expr]}
    : {- ε -}         {[]}
    | expr            {[$1]}
    | args0 ',' expr  {$3:$1}

body :: {Body}
    : '{' stmts 'return' expr ';' '}'
                      {Ret $2 $4}

{
-- token extractors

tkPosn :: Token -> Expr.Posn
tkPosn (TkVarId      (Posn _ l c) _) = Expr.Posn l c
tkPosn (TkReservedId (Posn _ l c) _) = Expr.Posn l c
tkPosn (TkReservedOp (Posn _ l c) _) = Expr.Posn l c
tkPosn (TkPrimId     (Posn _ l c) _) = Expr.Posn l c
tkPosn (TkConstant   (Posn _ l c) _) = Expr.Posn l c
tkPosn (TkSpecial    (Posn _ l c) _) = Expr.Posn l c
tkPosn (TkUnexpected (Posn _ l c) _) = Expr.Posn l c
tkPosn TkEOF = Expr.Posn 0 0

tkVar :: Token -> Var
tkVar (TkVarId _ x) = x
tkVar _ = undefined

tkInt :: Token -> Int
tkInt (TkConstant _ (CInt n)) = n
tkInt _ = undefined

tkBool :: Token -> Bool
tkBool (TkConstant _ (CBool b)) = b
tkBool _ = undefined

-- happy

happyParseError :: Token -> P a
happyParseError TkEOF = do
    (Input (Posn _ l c) _ _ _) <- getInput
    errP (buildError Syntax l c (pprToken TkEOF))
happyParseError tk = do
    let Expr.Posn l c = tkPosn tk
    errP (buildError Syntax l c (pprToken tk))

lexer :: (Token -> P a) -> P a
lexer = \f -> scanToken >>= f
}