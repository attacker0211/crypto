module Algorithm.MillerRabin
  ( elimTwo
  , millerList
  , checkPrimeMiller
  , millerTest
  )
where
import           Algorithm.SuccessiveSquaring
import           Algorithm.Numeric

elimTwo :: Integer -> Integer -> (Integer, Integer)
elimTwo t x = if t `mod` 2 == 0 then elimTwo (t `div` 2) (x + 1) else (t, x)

-- a = a^q -> k -> n -> List of k elements
millerList :: Integer -> Integer -> Integer -> [Integer]
millerList _  0 _ = []
millerList aq k n = [aq] ++ millerList (multipleF n aq aq) (k - 1) n

checkPrimeMiller :: Integer -> Integer -> Integer -> Integer -> Bool
checkPrimeMiller a q k n =
  let ml = millerList (fastPowering a q n) k n in check' ml 0
 where
  check' []       _ = True
  check' (x : xs) 0 = if x == 1 then False else check' xs 1
  check' (x : xs) k = if x == n - 1 then False else check' xs (k + 1)

millerTest :: Integer -> Integer -> Bool
millerTest a n =
  let (q, k) = elimTwo (n - 1) 0
      a'     = fastPowering a q n
  in  miller a' k k n
 where
  miller _  0 _ _ = True
  miller a' p k n = if p == k
    then if a' `mod` n == 1
      then False
      else miller (multipleF n a' a') (k - 1) k n
    else if a' `mod` n == -1
      then False
      else miller (multipleF n a' a') (p - 1) k n
