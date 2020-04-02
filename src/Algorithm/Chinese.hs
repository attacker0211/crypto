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
chineseMod :: [(Integer, Integer)] -> Integer
chineseMod congruences = product (snd <$> congruences)

-- return tuples (M_k, m_k)
chineseEq :: Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
chineseEq chiMod congruences =
  (\(_, m) -> (chiMod `quot` m, m)) <$> congruences

solveChineseEq :: [(Integer, Integer)] -> [Maybe Integer]
solveChineseEq eqs = (\(r, m) -> inv r m) <$> eqs

-- (b_k, m_k) -> solution to the system
chineseRT
  :: [(Integer, Integer)]
  -> [(Integer, Integer)]
  -> [Maybe Integer]
  -> Integer
  -> Maybe Integer
chineseRT congruences eq invs chiMod = sum'
  chiMod
  (zipWith combine' (zip (fst <$> congruences) (fst <$> eq)) invs)
 where
  combine' _  Nothing  = Nothing
  combine' tp (Just r) = Just $ (fst tp * snd tp * r `mod` chiMod)

  sum' _      []       = Just 0
  sum' chiMod (x : xs) = if isNothing x
    then Nothing
    else Just $ ((fromJust x + (fromJust (sum' chiMod xs))) `mod` chiMod)
