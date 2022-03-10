module Wordle
( Correctness
, Wordle
, wordleFromStr
, correct
, check
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
correct guess solution = correct' checked solution []
    where checked = check guess solution []

data Check = MisplacedOrBad (Char,Int) | Good deriving (Show, Eq)

check :: Wordle -> Wordle -> [Check] -> [Check]
check []     _          acc = acc
check (g:gs) s@(f:sltn) acc
    | g == f          = check gs sltn          (acc ++ [Good])
    | otherwise       = check gs s             (acc ++ [MisplacedOrBad g])

correct' :: [Check] -> Wordle -> [Correctness] -> [Correctness]
correct' []      _    acc = acc
correct' (c:chk) sltn acc = 
    case c of Good                  -> correct' chk sltn (acc ++ [Correct])
              MisplacedOrBad mob
                | mob `elemOf` sltn -> correct' chk (remove' mob sltn) (acc ++ [Misplaced])
                | otherwise         -> correct' chk sltn               (acc ++ [Wrong])
