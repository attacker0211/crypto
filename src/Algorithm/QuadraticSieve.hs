module Algorithm.QuadraticSieve
  ()
where
import           Data.Array.Unboxed
import           Control.Lens

bound :: Int -> Int
bound n = floor (sqrt n) + 1

initList :: Int -> [Int]
initList bound = (\x -> x * x - n) <$> [bound .. 2 * bound]

factorBase :: [Int]
factorBase = [2, 3, 5, 7]

initSieve :: Int -> Int -> UArray (Int, Int) Int
initSieve bound factorBound =
  listArray ((0, 0), (bound, factorBound)) replicate (bound * factorBound) 0

-- number needs to be factored n -> factorBase -> processed sieve
process :: Int -> [Int] -> UArray (Int, Int) Int
process n =
  let b     = bound n
      init  = initList b
      sieve = initSieve b (length factorBase)
  in  process' init sieve


