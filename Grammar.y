{
module Grammar where
import Tokens
import Data.List
}

%name shive
%tokentype { Token }
%error { shiveShilarny }
%token
    do      { TokenDo p }
    '&'     { TokenAnd p }
    '='     { TokenEquals p }
    for     { TokenFor p }
    in      { TokenIn p }
    file    { TokenFile p $$ }
    ifexist { TokenIfExist p }
    var     { TokenVar p $$ }
    ','     { TokenComma p }
    '('     { TokenLParen p }
    ')'     { TokenRParen p }

%left '&' ','
%%

DExp : Expr do Expr         { Do $1 $3 }
     | Expr                 { $1 }
     
Expr : for Expr in file     { For $2 $4 }
     | ifexist Expr in Expr { IfExist $2 $4 }
     | AExp                 { $1 }

AExp : AExp '=' BExp        { Equals $1 $3 }
     | BExp                 { $1 }

BExp : BExp ',' CExp        { Comma $1 $3 }
     | BExp '&' CExp        { And $1 $3 }
     | CExp                 { $1 }

CExp : var                  { Var $1 }
     | '(' DExp ')'         { $2 }

{
shiveShilarny :: [Token] -> a
shiveShilarny xs = error ("Shive shilarny at " ++ lcn ++ "\n")
    where
    lcn =  case xs of
             [] -> "the files snuff'd it"
             x:_ -> "stroka " ++ show l ++ ", kolonka " ++ show c
                 where
                 AlexPn _ l c = token_posn x

data Expr = Print Expr
          | IfExist Expr Expr
          | For Expr String
          | Equals Expr Expr
          | And Expr Expr
          | Comma Expr Expr
          | Do Expr Expr
          | Var Int
          deriving Show
}

