{
module Tokens where
}

%wrapper "posn"
$digit = 0-9
-- digits
$alpha = [a-zA-Z]
-- alphabetic characters

tokens :-
$white+        ;
  "--".*       ;
  do           { tok (\p s -> TokenDo p) } 
  \&           { tok (\p s -> TokenAnd p) } 
  \=           { tok (\p s -> TokenEquals p) } 
  for          { tok (\p s -> TokenFor p) } 
  in           { tok (\p s -> TokenIn p) } 
  as           { tok (\p s -> TokenAs p) }
  ifexist      { tok (\p s -> TokenIfExist p) } 
  $digit+      { tok (\p s -> TokenVar p (read s)) } 
  \,           { tok (\p s -> TokenComma p) } 
  \(           { tok (\p s -> TokenLParen p) } 
  \)           { tok (\p s -> TokenRParen p) } 
  $alpha [$alpha $digit \.\_\/]*   { tok (\p s -> TokenFile p s) } 

{
tok f p s = f p s

data Token = 
  TokenDo AlexPosn           |
  TokenAnd AlexPosn          |
  TokenEquals AlexPosn       |
  TokenFor AlexPosn          |
  TokenIn AlexPosn           |
  TokenAs AlexPosn           |
  TokenFile AlexPosn String  |
  TokenIfExist AlexPosn      |
  TokenVar AlexPosn Int      |
  TokenComma AlexPosn        |
  TokenLParen AlexPosn       |
  TokenRParen AlexPosn
  deriving (Eq,Show)

-- code to extract position
token_posn (TokenDo p) = p
token_posn (TokenAnd p) = p
token_posn (TokenEquals p) = p
token_posn (TokenFor p) = p
token_posn (TokenIn p) = p
token_posn (TokenAs p) = p
token_posn (TokenFile p a) = p
token_posn (TokenIfExist p) = p
token_posn (TokenVar p a) = p
token_posn (TokenComma p) = p
token_posn (TokenLParen p) = p
token_posn (TokenRParen p) = p

-- adapted from: https://github.com/simonmar/alex/blob/master/examples/Tokens_posn.x
}
