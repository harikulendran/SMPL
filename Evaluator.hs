module Main where

import Data.Functor
import System.Environment
import Tokens
import Grammar
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.List (sort, group, reverse)
import Data.List.Split(splitOn)
import Data.Char (isAlpha, isNumber)
import Helpers


--Operator Implementations
filterScope :: Scope -> Scope -> Scope -> (Scope,Scope)
filterScope s1 [] out = (s1,out)
filterScope s1 (s:s2) out | s1 `contains` (fst s) = filterScope s1 s2 (out++[(fst s,(find (fst s) s1))])
                          | otherwise = filterScope s1 s2 out

filterByExist :: Scope -> Scope -> (Scope,Scope)
filterByExist   []     _ = ([],[])
filterByExist vars scope | (and $ map (\n -> scope `contains` (fst n)) vars)  = (scope,[])
                         | otherwise                                          = ([],[])

filterByEqual :: Scope -> (Int,String) -> (Int,String) -> (Scope,Scope)
filterByEqual [] _ _      = ([],[])
filterByEqual scope v1 v2 = ((scope `updateAll` (fst v1,(find (fst v1) scope)++"="++(show $ (fst v2)))) `updateAll` (fst v2,(find (fst v2) scope)++"="++(show $ (fst v1))),[])


--Evaluate the parse tree   
eval' :: Scope -> Expr -> String  -> (Scope,Scope)
eval' scope expr str  = case expr of
    Var i                -> (scope ++ [(i,str)],[])
    Comma e1 e2          -> ((fst(eval' scope e1 str)) ++ (fst(eval' scope e2 str)),[])
    For e s1 s2          -> ((0,s2++"|"++s1):(fst(eval' scope e (s2++"|"++s1))),[])
    IfExist e1 e2        -> filterByExist (fst $ eval' scope e1 str) (fst $ eval' scope e2 str)
    And e (Equals v1 v2) -> filterByEqual (fst $ eval' scope e str) (head $ fst $ eval' scope v1 str) (head $ fst $ eval' scope v2 str)
    And e1 e2            -> ((fst $ eval' scope e1 str) ++ (fst $ eval' scope e2 str),[])
    Do e1 e2             -> filterScope (fst(eval' scope e1 str)) ((fst(eval' scope e2 str))) []




-- Reading Files
commaSplit :: Text.Text -> String -> Scope -> Scope
commaSplit iline name scope =
    let checkVar []                        = -1
        checkVar (v:vars) | snd v === name = fst v
                          | otherwise      = checkVar vars

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
    file <- Text.lines <$> (Text.readFile (split filename '|'))
    return (updateLine scopes file filename)

loadFiles scopes vars = do
    let names = scopes `findAll` 0
    let sRead a = readCSV (scopes `removeAll` 0) a

    let crossP [] ys = ys
        crossP xs ys = [ x++y | x<-xs, y<-ys ]

    let crossPA [] o = o
        crossPA (x:xs) o | and $ map (\n -> n/=[]) (x:xs) =  crossPA xs (crossP o x) 
                         | otherwise = []

    allData <- sequence $ map sRead names
    return (crossPA allData [])


-- execute the evaluated parsetree output on the csv files
eval (scope,vars) = do
    fdata <- loadFiles scope vars
    let orderLine' :: Scope -> Scope -> String
        orderLine'        [] line = []
        orderLine' (v:oVars) line | varsEquiv line = (find (fst v) line)++","++(orderLine' oVars line)
                                  | otherwise      = []

        varsEquiv scp = and $ map (\var -> (equiv $ scp `findAll` (fst var))) scp
        
        --Get substitution rules
        subRules scope = lzip (map head (group $ sort $ map (\n -> sort [fst n, read (filter isNumber (split (snd n) '='))::Int]) (filter (\n -> '=' `elem` (snd n)) scope))) [1..]

        --Get next free variable
        getBndVars scp = map head (group $ sort $ map fst scp)
        getFreeVar xs = (last xs) + 1
        free  = getFreeVar $ getBndVars (scope++vars)

        --Rename variable to next free var
        rename' scp x y = renameV scp (x,"") (y,find x scp)
        rename scp x y m = rename' (rename' scp x ((free ) + m)) y ((free ) + m)

        --Apply substitutions
        cleanLine s [] = s
        cleanLine s (([a,b,c]):rs) = cleanLine (rename s a b c) rs
        cleanData fdata scope = map (\n -> cleanLine (fst n) (subRules (snd n))) (zip fdata (repeat scope))

        orderLine'' line = orderLine' (head $ cleanData [vars] scope) line
        orderLine line | orderLine'' line == [] = []
                       | otherwise = init $ orderLine'' line

        -- Properly order output
        out = filter (\n -> n /= []) (map orderLine (cleanData fdata scope))
        outA = map (\n -> splitOn "," n) out
        ordA = sort outA
        sinA = map (\p -> concatMap (\n -> n++",") p) ordA

    return $ unlines $ map init sinA




--Interpret
interpret str = eval (eval' [] (parse $ alexScanTokens str) "null") >>= putStr


--For Testing (unit test files not included in submission)
testInterpret str = eval (eval' [] (parse $ alexScanTokens str) "null")


execute filename = do
    command <- Text.lines <$> (Text.readFile filename)
    let string = Text.unpack $ head $ command
    interpret string

main = do 
    args <- getArgs
    execute $ head $ args
