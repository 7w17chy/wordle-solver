module Test
( unzip'
) where

unzip' :: [(a,b)] -> ([a],[b])
unzip' lst = unzip' lst ([],[])
    where unzip' []         res   = res
          unzip' ((e,i):ll) (a,b) = unzip' ll ((e:a),(i:b))
