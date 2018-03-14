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
    as      { TokenAs p }
    file    { TokenFile p $$ }
    ifexist { TokenIfExist p }
    var     { TokenVar p $$ }
    ','     { TokenComma p }
    '('     { TokenLParen p }
    ')'     { TokenRParen p }

%left '&' ','
%%

DExp : Expr do CExp              { Do $1 $3 }
     
Expr : for CExp in FExp as FExp  { For $2 $4 $6 }
     | ifexist CExp in Expr      { IfExist $2 $4 }
     | BExp                      { $1 }

BExp : BExp '&' AExp             { And $1 $3 }
     | '(' Expr ')'              { $2 }

AExp : '(' VExp '=' VExp ')'     { Equals $2 $4 }
     | BExp                      { $1 }

CExp : VExp                      { $1 }
     | VExp ',' CExp             { Comma $1 $3 }

VExp : var                       { Var $1 }
FExp : file                      { $1 }


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
          | For Expr String String
          | Equals Expr Expr
          | And Expr Expr
          | Comma Expr Expr
          | Do Expr Expr
          | Var Int
          deriving Show
}

