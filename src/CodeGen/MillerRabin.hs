module CodeGen.MillerRabin
  ( genMiller
  , genMillerL
  )
where
import           Algorithm.MillerRabin
import           Algorithm.SuccessiveSquaring
import           CodeGen.Utils
import           Data.Text.Prettyprint.Doc      ( Pretty(..)
                                                , Doc
                                                , (<+>)
                                                )
import qualified Data.Text.Prettyprint.Doc     as Pretty

genMiller :: Integer -> Integer -> Doc ann
genMiller n a =
  let (q, k) = elimTwo (n - 1) 0
  in  ddollar (genFac k q) <> Pretty.hardline <> align (genList q n k a)

genMillerL :: Integer -> [Integer] -> Doc ann
genMillerL n a =
  let (q, k) = elimTwo (n - 1) 0
  in  ddollar (genFac k q) <> Pretty.hardline <> Pretty.vsep
        ((genList q n k) <$> a)

genFac :: Integer -> Integer -> Doc a
genFac k q =
  "n - 1 =" <+> "2^" <> Pretty.braces (pretty k) <+> cdot <+> pretty q

-- a -> q -> n -> k
genList :: Integer -> Integer -> Integer -> Integer -> Doc ann
genList q n k a =
  let ml = millerList (fastPowering a q n) k n in align (genL ml 1)
 where
  genL :: [Integer] -> Integer -> Doc a
  genL [] _ = ""
  genL [x] i =
    pretty a
      <>  "^"
      <>  Pretty.braces (pretty i <> "." <> pretty q)
      <+> "&"
      <>  eqv
      <+> pretty x
      <+> "&"
      <>  pmo n
      <>  Pretty.hardline
  genL (x : xs) i =
    pretty a
      <>  "^"
      <>  Pretty.braces (pretty i <> "." <> pretty q)
      <+> "&"
      <>  eqv
      <+> pretty x
      <+> "&"
      <>  pmo n
      <>  newl
      <>  Pretty.hardline
      <>  genL xs (2 * i)
