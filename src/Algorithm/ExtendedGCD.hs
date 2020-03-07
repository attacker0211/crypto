module Algorithm.ExtendedGCD
  ( divList
  , egcd
  , egcdL
  , tableF
  , tableS
  , table'
  )
where

-- a, b positive. Find u, v such that au + bv = gcd(a, b)
-- egcd a b = (g, u, v)
egcd :: Int -> Int -> (Int, Int, Int)
egcd a 0 = (a, )

-- multiplicative inverse
inv :: Int -> Int -> Int
inv a b =
  let
    li = divList a b
    fi = tableF li
    se = tableS li
    mul =
      (se !! (length se - 2)) * (last fi) - (fi !! (length fi - 2)) * (last se)
  in
    (mul * (se !! (length se - 2)) + b) `mod` b

-- egcdL tableF tableS -> (u, v)
egcdL :: [Int] -> [Int] -> (Int, Int, Int)
egcdL fi se =
  let
    mul =
      (se !! (length se - 2)) * (last fi) - (fi !! (length fi - 2)) * (last se)
  in  ( abs mul
      , ((signum mul) * (se !! (length se - 2)) + (last se)) `mod` (last se)
      , (signum mul) * (-1) * (fi !! (length fi - 2))
      )

-- divisors list while doing gcd algorithm
divList :: Int -> Int -> [Int]
divList _ 0 = []
divList a b = [a `div` b] ++ divList b (a `rem` b)

-- generate the first row of the bezout table
tableF :: [Int] -> [Int]
tableF li = table' 0 1 li

-- generate the second row of the bezout table
tableS :: [Int] -> [Int]
tableS li = table' 1 0 li

table' :: Int -> Int -> [Int] -> [Int]
table' _ _ []       = []
table' u v (x : xs) = let t = x * v + u in [t] ++ table' v t xs
