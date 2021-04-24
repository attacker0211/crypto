module Algorithm.ExtendedGCD
  ( divList
  , egcd
  , egcdL
  , inv
  , tableF
  , tableS
  , table'
  ) where

-- a, b positive. Find u, v such that au + bv = gcd(a, b)
-- egcd a b = (g, u, v)
-- g = bu + (a `mod` b)v = bu + [a - (a `div` b)b]v = av + b(u - (a `div` b)v
egcd :: Int -> Int -> (Int, Int, Int)
egcd a 0 = (abs a, signum a, 0)
egcd a b = let (g, u, v) = egcd b (a `mod` b) in (g, v, u - (a `div` b) * v)

-- multiplicative inverse
-- find x such that ax = 1 mod p
inv :: Int -> Int -> Maybe Int
inv a p = let (g, u, _) = egcd a p in inv' g u p where
  inv' 1 u p = Just ((u + p) `mod` p)
  inv' _ _ _ = Nothing

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

-- generate the first row of the gcd table
tableF :: [Int] -> [Int]
tableF li = table' 0 1 li

-- generate the second row of the gcd table
tableS :: [Int] -> [Int]
tableS li = table' 1 0 li

-- helper to generate table's row
table' :: Int -> Int -> [Int] -> [Int]
table' _ _ []       = []
table' u v (x : xs) = let t = x * v + u in [t] ++ table' v t xs
