module Algorithm.SuccessiveSquaring
  ( toLogTwo
  , successiveList
  , powerList
  , fastPowering
  , fastPoweringL
  )
where
import           Algorithm.Numeric
import           Data.Bits                      ( shiftL )
import           Data.List                      ( (++) )

toLogTwo :: Integer -> [Integer]
toLogTwo 0 = []
toLogTwo x =
  let lo = log2 x
  in  toLogTwo (x - toInteger (shiftL (1 :: Integer) (fromInteger lo))) ++ [lo]

-- Base (g) -> Max exponent -> mod p -> List 
successiveList :: Integer -> Integer -> Integer -> [Integer]
successiveList g 0 __ = [g]
successiveList g a p =
  let l = successiveList g (a - 1) p
  in  l ++ [((`mod` p) . (\x -> x * x) . last) $ l]

 --base g -> exponent h -> mod p -> g^h mod p
powerList :: Integer -> Integer -> Integer -> [Integer]
powerList g h p =
  let lt = toLogTwo h
      sl = successiveList g (log2 h) p
  in  (sl !!) <$> (fromInteger <$> lt)

fastPowering :: Integer -> Integer -> Integer -> Integer
fastPowering g h p = foldr (multipleF p) 1 (powerList g h p)

fastPoweringL :: Integer -> [Integer] -> Integer
fastPoweringL p li = foldr (multipleF p) 1 li
