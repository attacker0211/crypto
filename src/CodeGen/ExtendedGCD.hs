module CodeGen.ExtendedGCD
  ( genEucl
  , genEGCDT
  )
where
import           Algorithm.ExtendedGCD
import           CodeGen.Utils
import           Data.Text.Prettyprint.Doc      ( Pretty(..)
                                                , Doc
                                                , (<+>)
                                                )
import qualified Data.Text.Prettyprint.Doc     as Pretty

-- gen steps for doing Euclid algorithm
genEucl :: Integer -> Integer -> Doc a
genEucl a b = align (genEucl' a b) where
  genEucl' _ 0 = Pretty.hardline
  genEucl' a b =
    pretty a
      <+> "&="
      <+> pretty b
      <+> cdot
      <+> pretty (a `div` b)
      <+> "+"
      <+> pretty (a `rem` b)
      <+> (if (a `rem` b) == 0 then "" else newl)
      <+> genEucl' b (a `rem` b)

-- gen table to find u v such that ub - va = (-1)^{t}
genEGCDT :: Integer -> Integer -> Doc a
genEGCDT a b =
  let dl = divList a b
  in  ctabl
        (pretty ("cc|" ++ replicate (length dl) 'c'))
        (   "&&"
        <+> genRow dl
        <+> newl
        <>  Pretty.hardline
        <>  "\\hline"
        <>  Pretty.hardline
        <>  dollar "0"
        <+> "&"
        <+> dollar "1"
        <+> "&"
        <+> genRow (tableF dl)
        <+> newl
        <>  Pretty.hardline
        <>  dollar "1"
        <+> "&"
        <+> dollar "0"
        <+> "&"
        <+> genRow (tableS dl)
        <>  Pretty.hardline
        )
