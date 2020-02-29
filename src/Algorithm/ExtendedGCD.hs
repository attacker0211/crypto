module Algorithm.ExtendedGCD
  ( egcd
  , egcdL
  , divList
  , tableF
  , tableS
  , table'
  )
where

-- a, b positive. Find u, v such that au + bv = gcd(a, b)
egcd :: Int -> Int -> (Int, Int)
egcd a b = let li = divList a b in (last (tableF li), last (tableS li))

egcdL :: [Int] -> [Int] -> (Int, Int)
egcdL fi se = (last fi, last se)

divList :: Int -> Int -> [Int]
divList _ 0 = []
divList a b = [a `div` b] ++ divList b (a `rem` b)

tableF :: [Int] -> [Int]
tableF li = table' 0 1 li

tableS :: [Int] -> [Int]
tableS li = table' 1 0 li

table' :: Int -> Int -> [Int] -> [Int]
table' u v []       = [u, v]
table' u v (x : xs) = let t = x * v + u in [t] ++ table' v t xs
