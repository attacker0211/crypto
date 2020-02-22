module Algorithm.Numeric
  ( multipleF
  , log2
  )
where
-- multiple in a field F_p
multipleF :: Int -> Int -> Int -> Int
multipleF p a b = (a * b) `mod` p

-- log2
log2 :: Int -> Int
log2 x = floor (logBase (fromIntegral 2) (fromIntegral x))
