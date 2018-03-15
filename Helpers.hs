module Helpers where

import Data.Char (isAlpha, isNumber)

type Scope = [(Int,String)]

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

remove :: Scope -> Int -> Scope
remove     [] n              = []
remove (s:ss) n | fst s == n = ss
                | otherwise  = s:(remove ss n)

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

updateAll :: Scope -> (Int,String) -> Scope
updateAll scope val = 
    let update'     [] _ m                  = m
        update' (s:ss) v m | fst s == fst v = m ++ [v] ++ (update' ss v [])
                           | otherwise      = update' ss v (m++[s])
    in update' scope val []

renameV scope val val2 = 
    let update'     [] _  _ m                  = m
        update' (s:ss) v v2 m | fst s == fst v = m ++ [v2] ++ (update' ss v v2 [])
                              | otherwise      = update' ss v v2 (m++[s])
    in update' scope val val2 []

equiv :: [String] -> Bool
equiv xs = (filter (\n -> n /= (head xs)) xs) == []

(===) :: String -> String -> Bool
(===) s1 s2 =
    let testString s = filter (\n -> n=='.' || isAlpha n || isNumber n) s
    in (testString s1) == (testString s2)

lzip (x:xs) (y:ys) = (x++[y]):lzip xs ys
lzip      _      _ = []

split      [] delim = []
split (s:str) delim | s == delim = str
                    | otherwise  = split str delim



