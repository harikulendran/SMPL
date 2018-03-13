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
find n     []              = []
find n (s:ss) | fst s == n = snd s
              | otherwise  = find n ss

findAll :: Scope -> Int -> [String]
findAll scope n = 
    let findAll' [] n out                  = out
        findAll' (s:ss) n out | fst s == n = findAll' ss n (out++[snd s])
                              | otherwise  = findAll' ss n out
    in findAll' scope n []

removeAll :: Scope -> Int -> Scope
removeAll s n = 
    let removeAll' [] n mem                  = mem
        removeAll' (s:ss) n mem | fst s == n = removeAll' ss n mem
                                | otherwise  = removeAll' ss n (mem++[s])
    in removeAll' s n []

update :: Scope -> (Int,String) -> Scope
update scope val =
    let update' (s:ss) v m | fst s == fst v = m ++ [v] ++ ss
                           | otherwise      = update' ss v (m++[s])
    in update' scope val []




--Operator Implementations
filterScope :: Scope -> Scope -> Scope -> (Scope,Scope)
filterScope s1 [] out = (s1,out)
filterScope s1 (s:s2) out | s1 `contains` (fst s) = filterScope s1 s2 (out++[(fst s,(find (fst s) s1))])
                          | otherwise = filterScope s1 s2 out


    
eval' :: Scope -> Expr -> String  -> (Scope,Scope)
eval' scope expr str  = case expr of
    Var i             -> (scope ++ [(i,str)],[])
    Comma e1 e2       -> ((fst(eval' scope e1 str)) ++ (fst(eval' scope e2 str)),[])
    For e s           -> ((0,s):(fst(eval' scope e s)),[])
    And e1 e2         -> ((fst $ eval' scope e1 str) ++ (fst $ eval' scope e2 str),[])
    Do e1 e2          -> filterScope (fst(eval' scope e1 str)) ((fst(eval' scope e2 str))) []




-- Reading Files
commaSplit :: Text.Text -> String -> Scope -> Scope
commaSplit iline name scope =
    let checkVar []                       = -1
        checkVar (v:vars) | snd v == name = fst v
                          | otherwise     = checkVar vars

        getWord []       cWord out = out++[cWord]
        getWord (l:line) cWord out | l == ','  = getWord line [] (out++[cWord])
                                   | otherwise = getWord line (cWord++[l]) out

        prune []        vars out = out
        prune (w:words) vars out | checkVar vars == -1 = prune words vars out
                                 | otherwise           = prune words (vars `removeAll` (checkVar vars)) (out++[(checkVar vars,w)])

   in prune ( getWord ( Text.unpack iline ) [] [] ) scope []

updateLine :: Scope -> [Text.Text] -> String -> [Scope]
updateLine scopes [] _ = []
updateLine scopes (line:lines) name = ((commaSplit line name scopes):(updateLine scopes lines name))

readCSV scopes filename = do
    file <- Text.lines <$> (Text.readFile filename)
    return (updateLine scopes file filename)

loadFiles scopes vars = do
    let names = scopes `findAll` 0
    let sRead a = readCSV (vars `removeAll` 0) a

    let crossP [] ys = ys
        crossP xs ys = [ x++y | x<-xs, y<-ys ]

    let crossPA [] o = o
        crossPA (x:xs) o = crossPA xs (crossP o x) 

    allData <- sequence $ map sRead names
    return (crossPA allData [])

eval (scope,vars) = do
    fdata <- loadFiles scope vars
    let orderLine' :: Scope -> Scope -> String
        orderLine'        [] line = []
        orderLine' (v:oVars) line = (find (fst v) line)++", "++(orderLine' oVars line)

        orderLine line = orderLine' vars line
       
        filt [] _ _ outA _ = outA
        filt (d:ds) [] outl outA mem = filt ds mem [] (outA++[outl]) []
        filt (d:ds) (v:vs) outl outA mem | d `contains` (fst v) = filt (d:ds) vs (outl++(find (fst v) d)++", ") outA (mem++[v])
                                         | otherwise = filt (d:ds) vs outl outA (mem++[v])
    mapM_ putStrLn (map orderLine fdata)




--Interpret
interpret str = eval (eval' [] (shive $ alexScanTokens str) "null")
