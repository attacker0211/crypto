module Algorithm.QuadraticSieve
  ( initList
  , convert
  , convertL
  , extractSieve
  )
where
import           Data.Array.Unboxed
import           Control.Lens

bound :: Int -> Int
bound n = (floor (sqrt (fromIntegral n))) + 1

initList :: Int -> [((Int, Int), Int)]
initList n =
  let b = bound n in (\x -> ((x, x * x - n), x * x - n)) <$> [b .. 2 * b]

factorBase :: [Int]
factorBase = [2, 3, 5, 7]

initSieve :: Int -> Int -> UArray (Int, Int) Int
initSieve bound factorBound =
  listArray ((0, 0), (bound, factorBound)) (replicate (bound * factorBound) 0)

-- number needs to be factored n -> factorBase -> processed sieve
convert :: Int -> (UArray (Int, Int) Int, [((Int, Int), Int)])
convert n = let initL = initList n in convertL initL

convertL :: [((Int, Int), Int)] -> (UArray (Int, Int) Int, [((Int, Int), Int)])
convertL initL =
  let sieve = initSieve (length initL - 1) (length factorBase - 1)
  in  foldl
        (\(sieve, initL) xy ->
          process (sieve, initL) (factorBase !! (snd xy)) xy
        )
        (sieve, initL)
        [ (x, y)
        | x <- [0 .. (length initL - 1)]
        , y <- [0 .. (length factorBase - 1)]
        ]

process
  :: (UArray (Int, Int) Int, [((Int, Int), Int)])
  -> Int
  -> (Int, Int)
  -> (UArray (Int, Int) Int, [((Int, Int), Int)])
process sl fac xy =
  let sieve = fst sl
      initL = snd sl
      num   = initL !! (fst xy)
      k     = fst $ num
      v     = snd $ num
  in  if v `rem` fac == 0
        then
          let t = sieve ! xy + 1
          in  process
                ( (sieve & ix xy .~ (t `rem` 2))
                , (initL & ix (fst xy) .~ (k, v `div` fac))
                )
                fac
                xy
        else sl

extractSieve
  :: (UArray (Int, Int) Int, [((Int, Int), Int)]) -> [([Int], (Int, Int))]
extractSieve sl =
  let sieve = fst sl
      li    = snd sl
  in  extract' 0 li sieve where
  extract' i (x : xs) sieve = if snd x == 1
    then
      [([ sieve ! (i, j) | j <- [0 .. (length factorBase - 1)] ], fst x)]
        ++ extract' (i + 1) xs sieve
    else extract' (i + 1) xs sieve
  extract' _ [] _ = []
