{-# LANGUAGE MultiWayIf #-}

module Wordle
( Correctness
, Wordle
, wordleFromStr
, correct
) where

import qualified Data.List as List

data Correctness = Correct | Misplaced | Wrong deriving (Show, Eq)

type Wordle = [(Char,Int)]

-- flip the items of the given tuple.
flip :: (a,b) -> (b,a)
flip (a,b) = (b,a)

-- unzip a zipped list
unzip' :: [(a,b)] -> ([a],[b])
unzip' lst = uzip lst ([],[])
    where uzip []         res   = res
          uzip ((e,i):ll) (a,b) = uzip ll ((e:a),(i:b))

-- remove an element from a list
remove :: (Eq a) => a -> [a] -> [a]
remove elem (f:lst)
    | elem == f = lst
    | otherwise = remove elem lst

remove' :: (Eq a) => a -> [a] -> [a]
remove' = undefined

-- Pair every element of the list with its corresponding index.
enumerate :: [a] -> [(a,Int)]
enumerate [] = []
enumerate lst = 
    let cnt = length lst in
    zip lst [1..cnt]

-- create a wordle from a string.
wordleFromStr :: String -> Wordle
wordleFromStr str = enumerate str

-- checks if t is contained in any tuple in the given tuple list,
-- ignoring the index.
elemOf :: (Eq a) => (a,Int) -> [(a,Int)] -> Bool
(t,_) `elemOf` lst = 
    foldr (\(c,_) _ -> if t == c then True else False) False lst

-- compute the correctness of a given wordle.
correct :: Wordle -> Wordle -> [Correctness]
correct guess solution = correct' guess solution []

-- compute the correctness of a given wordle.
correct' :: Wordle -> Wordle -> [Correctness] -> [Correctness]
correct' []     _    acc = acc
correct' (g:gs) sltn acc
    | g `elem` sltn   = correct' gs (remove g sltn)  (Correct : acc)
    | g `elemOf` sltn = correct' gs (remove' g sltn) (Misplaced : acc)
    | otherwise       = correct' gs sltn             (Wrong : acc)
