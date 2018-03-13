module Evaluator where

import Tokens
import Grammar
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

type Scope = [(Int,String)]




--Helpers
contains :: Scope -> Int -> Bool
contains [] i = False
contains (s:ss) i | fst s == i = True
                  | otherwise = contains ss i

find :: Int -> Scope -> String
find n [] = []
find n (s:ss) | fst s == n = snd s
              | otherwise = find n ss

update :: Scope -> (Int,String) -> Scope
update scope val =
    let update' (s:ss) v m | fst s == fst v = m ++ [v] ++ ss
                           | otherwise = update' ss v (m++[s])
    in update' scope val []

{-- 
 - MIGHT NOT NEED THESE
(#) :: Scope -> (Int,String) -> Scope
scope # val | scope `contains` (fst val) = update scope val
            | otherwise = scope ++ [val]

(##) :: Scope -> Scope -> Scope
s1 ## [] = s1
s1 ## (s:s2) = (s1 # s) ## s2
--}



--Operator Implementations
filterScope :: Scope -> Scope -> Scope
filterScope _ [] = []
filterScope s1 (s:s2) | s1 `contains` (fst s) = (fst s,(find (fst s) s1)):(filterScope s1 s2)
                      | otherwise = filterScope s1 s2

    
eval' :: Scope -> Expr -> Scope
eval' scope expr = case expr of
    Var i       -> scope ++ [(i,"null")]
    Comma e1 e2 -> (eval' scope e1) ++ (eval' scope e2)
    For e s     -> (0,s):(eval' scope e)
    Do e1 e2    -> filterScope (eval' scope e1) ((0,"file"):(eval' scope e2))




-- Reading Files
commaSplit :: Text.Text -> Scope
commaSplit line =
    let split [] ys zs n = ys++[(n,zs)]
        split (x:xs) ys zs n | x == ',' = split xs (ys++[(n,zs)]) [] (n+1)
                             | otherwise = split xs ys (zs++[x]) n
    in split (Text.unpack line) [] [] 1


updateLine :: [Scope] -> [Text.Text] -> [Scope]
updateLine scopes [] = scopes
updateLine scopes (line:lines) = ((commaSplit line):(updateLine scopes lines))

readCSV scopes filename = do
    file <- Text.lines <$> (Text.readFile filename)
    return (updateLine scopes file)

eval (s:scope) = do
    fdata <- readCSV [] (snd s)
    let filt [] _ _ outA _ = outA
        filt (d:ds) [] outl outA mem = filt ds mem [] (outA++[outl]) []
        filt (d:ds) (v:vs) outl outA mem | d `contains` (fst v) = filt (d:ds) vs (outl++(find (fst v) d)++", ") outA (mem++[v])
                                         | otherwise = filt (d:ds) vs outl outA (mem++[v])
    return (unlines (filt fdata scope [] [] []))




--Interpret
interpret str = eval (eval' [] (shive $ alexScanTokens str))
