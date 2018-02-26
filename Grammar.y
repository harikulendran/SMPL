{
module Grammar where
import Tokens
import Data.List
}

%name shive
%tokentype { Token }
%error { shiveShilarny }
%token
    cout    { TokenPrint p }
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

Expr : cout Expr            { Print $2 }
     | ifexist Expr in Expr { IfExist $2 $4 }
     | for Expr in file     { For $2 $4 }
     | Expr '=' Expr        { Equals $1 $3 }
     | Expr '&' Expr        { And $1 $3 }
     | Expr ',' Expr        { Comma $1 $3 }
     | Expr do  Expr        { Do $1 $3 }
     | '(' Expr ')'         { $2 }
     | var                  { Var $1 }

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

