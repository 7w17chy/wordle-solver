module Wordle
( Correctness
, Wordle
, wordleFromStr
, correct
) where

import qualified Data.List as List
import Util

data Correctness = Correct | Misplaced | Wrong deriving (Show, Eq)

type Wordle = [(Char,Int)]

-- create a wordle from a string.
wordleFromStr :: String -> Wordle
wordleFromStr str = enumerate str

-- compute the correctness of a given wordle.
correct :: Wordle -> Wordle -> [Correctness]
correct guess solution = correct' guess solution []

-- compute the correctness of a given wordle.
correct' :: Wordle -> Wordle -> [Correctness] -> [Correctness]
correct' []     _    acc = acc
correct' (g:gs) s@(f:sltn) acc
    | g == f          = correct' gs sltn          (Correct : acc)
    | g `elemOf` s    = correct' gs (remove' g s) (Misplaced : acc) -- TODO: we cant just remove any equal letter!
    | otherwise       = correct' gs s             (Wrong : acc)
