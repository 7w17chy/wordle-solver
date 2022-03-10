module Util
( flip
, unzip'
, remove
, remove'
, enumerate
, elemOf
) where

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

-- checks if t is contained in any tuple in the given tuple list,
-- ignoring the index.
elemOf :: (Eq a) => (a,Int) -> [(a,Int)] -> Bool
(t,_) `elemOf` lst = 
    foldr (\(c,_) _ -> if t == c then True else False) False lst
