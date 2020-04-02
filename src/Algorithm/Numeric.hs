module Algorithm.Numeric
  ( multipleF
  , log2
  )
where
-- multiple in a field F_p
multipleF :: Integer -> Integer -> Integer -> Integer
multipleF p a b = (a * b) `mod` p

-- log2
log2 :: Integer -> Integer
log2 x = floor (logBase 2 (fromIntegral x))
