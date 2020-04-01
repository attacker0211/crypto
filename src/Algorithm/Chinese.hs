module Algorithm.Chinese
  ( chineseMod
  , chineseEq
  , solveChineseEq
  , chineseRT
  )
where
import           Algorithm.ExtendedGCD          ( inv )
import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )
chineseMod :: [(Int, Int)] -> Int
chineseMod congruences = product (snd <$> congruences)

-- return tuples (M_k, m_k)
chineseEq :: Int -> [(Int, Int)] -> [(Int, Int)]
chineseEq chiMod congruences =
  (\(_, m) -> (chiMod `quot` m, m)) <$> congruences

solveChineseEq :: [(Int, Int)] -> [Maybe Int]
solveChineseEq eqs = (\(r, m) -> inv r m) <$> eqs

-- (b_k, m_k) -> solution to the system
chineseRT :: [(Int, Int)] -> [Maybe Int] -> Maybe Int
chineseRT congruences invs = prod' $ (zipWith combine' congruences invs)
 where
  combine' _  Nothing  = Nothing
  combine' tp (Just r) = Just $ (fst tp * snd tp * r)

  prod' [] = Nothing
  prod' (x : xs) =
    if isNothing x then Nothing else Just $ (fromJust x * (fromJust $ prod' xs))
