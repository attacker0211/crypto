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
chineseRT :: [(Int, Int)] -> [(Int, Int)] -> [Maybe Int] -> Int -> Maybe Int
chineseRT congruences eq invs chiMod = sum'
  chiMod
  (zipWith combine' (zip (fst <$> congruences) (fst <$> eq)) invs)
 where
  combine' _  Nothing  = Nothing
  combine' tp (Just r) = Just $ (fst tp * snd tp * r `rem` chiMod)

  sum' _      []       = Just 0
  sum' chiMod (x : xs) = if isNothing x
    then Nothing
    else Just $ ((fromJust x + (fromJust (sum' chiMod xs))) `rem` chiMod)
