module Algorithm.SuccessiveSquaring
  ( toLogTwo
  , successiveList
  , powerList
  , fastPowering
  , fastPoweringL
  )
where
import           Algorithm.Numeric
import           Data.List                      ( (++) )
import           Data.Bits                      ( shiftL )

toLogTwo :: Int -> [Int]
toLogTwo 0 = []
toLogTwo x = let lo = log2 x in toLogTwo (x - (shiftL 1 lo)) ++ [lo]

-- Base (g) -> Max exponent -> mod p -> List 
successiveList :: Int -> Int -> Int -> [Int]
successiveList g 0 __ = [g]
successiveList g a p =
  let l = successiveList g (a - 1) p
  in  l ++ [((`mod` p) . (\x -> x * x) . last) $ l]

-- base g -> exponent h -> mod p -> g^h mod p
powerList :: Int -> Int -> Int -> [Int]
powerList g h p =
  let lt = toLogTwo h
      sl = successiveList g (log2 h) p
  in  (sl !!) <$> lt

fastPowering :: Int -> Int -> Int -> Int
fastPowering g h p = foldr (multipleF p) 1 (powerList g h p)

fastPoweringL :: Int -> [Int] -> Int
fastPoweringL p li = foldr (multipleF p) 1 li
