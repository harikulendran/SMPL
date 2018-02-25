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
  do           { tok (\p s -> Do p) } 
  \&           { tok (\p s -> And p) } 
  \=           { tok (\p s -> Equals p) } 
  for          { tok (\p s -> For p) } 
  in           { tok (\p s -> In p) } 
  ifexist      { tok (\p s -> IfExist p) } 
  $digit+      { tok (\p s -> Var p (read s)) } 
  \,           { tok (\p s -> Comma p) } 
  \(           { tok (\p s -> LParen p) } 
  \)           { tok (\p s -> RParen p) } 
  $alpha [$alpha $digit \.]*   { tok (\p s -> File p s) } 

{
tok f p s = f p s

data Token = 
  Do AlexPosn           |
  And AlexPosn          |
  Equals AlexPosn       |
  For AlexPosn          |
  In AlexPosn           |
  File AlexPosn String  |
  IfExist AlexPosn      |
  Var AlexPosn Int      |
  Comma AlexPosn        |
  LParen AlexPosn       |
  RParen AlexPosn
  deriving (Eq,Show)

token_posn (Do p) = p
token_posn (And p) = p
token_posn (Equals p) = p
token_posn (For p) = p
token_posn (In p) = p
token_posn (File p a) = p
token_posn (IfExists p) = p
token_posn (Var p a) = p
token_posn (Comma p) = p
token_posn (LParen p) = p
token_posn (RParen p) = p

}
