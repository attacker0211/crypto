module Algorithm.QuadraticSieve
  ()
where
import           Data.Array.Unboxed
import           Control.Lens
import           Data.Maybe
import           Control.Monad

bound :: Int -> Int
bound n = fromIntegral (floor (sqrt (fromIntegral n))) + 1

initList :: Int -> Int -> [Int]
initList n bound = (\x -> x * x - n) <$> [bound .. 2 * bound]

factorBase :: [Int]
factorBase = [2, 3, 5, 7]

initSieve :: Int -> Int -> UArray (Int, Int) Int
initSieve bound factorBound =
  --[ [ 0 | j <- [1 .. factorBound] ] | i <- [1 .. bound] ]
  listArray ((0, 0), (bound, factorBound)) (replicate (bound * factorBound) 0)

-- number needs to be factored n -> factorBase -> processed sieve
convert :: Int -> UArray (Int, Int) Int
convert n =
  let b     = bound n
      initL = initList n b
      sieve = initSieve (length initL) (length factorBase)
  in  foldl
        (\sieve xy -> process sieve
                              (fromJust (initL ^? ix (snd xy)))
                              (fromJust (factorBase ^? ix (fst xy)))
                              xy
        )
        sieve
        [ (x, y)
        | x <- [0 .. (length factorBase - 1)]
        , y <- [0 .. (length initL - 1)]
        ]

process
  :: UArray (Int, Int) Int -> Int -> Int -> (Int, Int) -> UArray (Int, Int) Int
process sieve num fac xy = if num `rem` fac == 0
  then
    let t = fromJust (sieve ^? ix xy) + 1
    in  process (sieve & ix xy .~ (t `rem` 2)) (num `div` fac) fac xy
  else sieve
