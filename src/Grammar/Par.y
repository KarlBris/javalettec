-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Grammar.Par where
import Grammar.Abs
import Grammar.Lex
import Grammar.ErrM

}

%name pProgram Program

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype { Token }

%token 
 '!' { PT _ (TS _ 1) }
 '!=' { PT _ (TS _ 2) }
 '%' { PT _ (TS _ 3) }
 '&&' { PT _ (TS _ 4) }
 '(' { PT _ (TS _ 5) }
 ')' { PT _ (TS _ 6) }
 '*' { PT _ (TS _ 7) }
 '+' { PT _ (TS _ 8) }
 '++' { PT _ (TS _ 9) }
 ',' { PT _ (TS _ 10) }
 '-' { PT _ (TS _ 11) }
 '--' { PT _ (TS _ 12) }
 '/' { PT _ (TS _ 13) }
 ':' { PT _ (TS _ 14) }
 ';' { PT _ (TS _ 15) }
 '<' { PT _ (TS _ 16) }
 '<=' { PT _ (TS _ 17) }
 '=' { PT _ (TS _ 18) }
 '==' { PT _ (TS _ 19) }
 '>' { PT _ (TS _ 20) }
 '>=' { PT _ (TS _ 21) }
 '[' { PT _ (TS _ 22) }
 ']' { PT _ (TS _ 23) }
 'boolean' { PT _ (TS _ 24) }
 'double' { PT _ (TS _ 25) }
 'else' { PT _ (TS _ 26) }
 'false' { PT _ (TS _ 27) }
 'if' { PT _ (TS _ 28) }
 'int' { PT _ (TS _ 29) }
 'return' { PT _ (TS _ 30) }
 'string' { PT _ (TS _ 31) }
 'true' { PT _ (TS _ 32) }
 'void' { PT _ (TS _ 33) }
 'while' { PT _ (TS _ 34) }
 '{' { PT _ (TS _ 35) }
 '||' { PT _ (TS _ 36) }
 '}' { PT _ (TS _ 37) }

L_ident  { PT _ (TV $$) }
L_integ  { PT _ (TI $$) }
L_doubl  { PT _ (TD $$) }
L_quoted { PT _ (TL $$) }
L_err    { _ }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }
Double  :: { Double }  : L_doubl  { (read ( $1)) :: Double }
String  :: { String }  : L_quoted {  $1 }

Program :: { Program }
Program : ListTopDef { Program $1 } 


TopDef :: { TopDef }
TopDef : Type Ident '(' ListArg ')' Block { FnDef $1 $2 $4 $6 } 


ListTopDef :: { [TopDef] }
ListTopDef : TopDef { (:[]) $1 } 
  | TopDef ListTopDef { (:) $1 $2 }


Arg :: { Arg }
Arg : Type Ident { Arg $1 $2 } 


ListArg :: { [Arg] }
ListArg : {- empty -} { [] } 
  | Arg { (:[]) $1 }
  | Arg ',' ListArg { (:) $1 $3 }


Block :: { Block }
Block : '{' ListStmt '}' { Block (reverse $2) } 


ListStmt :: { [Stmt] }
ListStmt : {- empty -} { [] } 
  | ListStmt Stmt { flip (:) $1 $2 }


Stmt :: { Stmt }
Stmt : ';' { Empty } 
  | Block { BStmt $1 }
  | Type ListItem ';' { Decl $1 $2 }
  | Ident '=' Expr ';' { Ass $1 $3 }
  | Ident '++' ';' { Incr $1 }
  | Ident '--' ';' { Decr $1 }
  | 'return' Expr ';' { Ret $2 }
  | 'return' ';' { VRet }
  | 'if' '(' Expr ')' Stmt { Cond $3 $5 }
  | 'if' '(' Expr ')' Stmt 'else' Stmt { CondElse $3 $5 $7 }
  | 'while' '(' Expr ')' Stmt { While $3 $5 }
  | Expr ';' { SExp $1 }


Item :: { Item }
Item : Ident { NoInit $1 } 
  | Ident '=' Expr { Init $1 $3 }


ListItem :: { [Item] }
ListItem : Item { (:[]) $1 } 
  | Item ',' ListItem { (:) $1 $3 }


Type :: { Type }
Type : 'int' { Int } 
  | 'double' { Doub }
  | 'boolean' { Bool }
  | 'void' { Void }


ListType :: { [Type] }
ListType : {- empty -} { [] } 
  | Type { (:[]) $1 }
  | Type ',' ListType { (:) $1 $3 }


Expr6 :: { Expr }
Expr6 : Ident { EVar $1 } 
  | Integer { ELitInt $1 }
  | Double { ELitDoub $1 }
  | 'true' { ELitTrue }
  | 'false' { ELitFalse }
  | Ident '(' ListExpr ')' { EApp $1 $3 }
  | String { EString $1 }
  | '(' Expr ')' { $2 }


Expr5 :: { Expr }
Expr5 : '-' Expr6 { Neg $2 } 
  | '!' Expr6 { Not $2 }
  | Expr6 { $1 }


Expr4 :: { Expr }
Expr4 : Expr4 MulOp Expr5 { EMul $1 $2 $3 } 
  | Expr5 { $1 }


Expr3 :: { Expr }
Expr3 : Expr3 AddOp Expr4 { EAdd $1 $2 $3 } 
  | Expr4 { $1 }


Expr2 :: { Expr }
Expr2 : Expr2 RelOp Expr3 { ERel $1 $2 $3 } 
  | Expr3 { $1 }


Expr1 :: { Expr }
Expr1 : Expr2 '&&' Expr1 { EAnd $1 $3 } 
  | Expr2 { $1 }


Expr :: { Expr }
Expr : Expr1 '||' Expr { EOr $1 $3 } 
  | Expr1 { $1 }


ListExpr :: { [Expr] }
ListExpr : {- empty -} { [] } 
  | Expr { (:[]) $1 }
  | Expr ',' ListExpr { (:) $1 $3 }


AddOp :: { AddOp }
AddOp : '+' { Plus } 
  | '-' { Minus }


MulOp :: { MulOp }
MulOp : '*' { Times } 
  | '/' { Div }
  | '%' { Mod }


RelOp :: { RelOp }
RelOp : '<' { LTH } 
  | '<=' { LE }
  | '>' { GTH }
  | '>=' { GE }
  | '==' { EQU }
  | '!=' { NE }



{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

