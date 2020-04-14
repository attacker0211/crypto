module CodeGen.SuccessiveSquaring
  ( genSuccessive
  , genBase
  , genRes
  )
where
import           Algorithm.SuccessiveSquaring
import           Algorithm.Numeric
import           CodeGen.Utils
import           Data.Text.Prettyprint.Doc      ( Pretty(..)
                                                , Doc
                                                , (<+>)
                                                )
import qualified Data.Text.Prettyprint.Doc     as Pretty

genOneSL :: Int -> Int -> Int -> Int -> Doc a
genOneSL g h p x =
  pretty g
    <>  "^"
    <>  Pretty.braces ("2^" <> Pretty.braces (pretty h))
    <+> eqv
    <+> pretty x
    <+> mo
    <>  Pretty.braces (pretty p)

genSL :: Int -> Int -> Int -> [Int] -> Doc a
genSL _ _ _ []  = ""
genSL g h p [x] = genOneSL g h p x
genSL g h p (fi : se) =
  (genOneSL g h p fi)
    <+> "&"
    <+> qqd
    <+> (genOneSL g (h + 1) p (head se))
    <+> (if (drop 1 se) == [] then "" else newl)
    <>  Pretty.hardline
    <>  genSL g (h + 2) p (drop 1 se)

genLB :: [Int] -> Doc a
genLB []       = ""
genLB [e     ] = "2^" <> Pretty.braces (pretty e)
genLB (e : es) = "2^" <> Pretty.braces (pretty e) <+> "+" <+> genLB es

genPL :: [Int] -> Doc a
genPL []       = ""
genPL [e     ] = pretty e
genPL (e : es) = pretty e <+> cdot <+> genPL es

genBase :: Int -> Doc a
genBase x = dollar (pretty x <+> "=" <+> genLB (toLogTwo x))

genSuccessive :: Int -> Int -> Int -> Doc a
genSuccessive g h p =
  let sl = successiveList g (log2 h) p in align (genSL g 0 p sl)

genRes :: Int -> Int -> Int -> Doc a
genRes g h p =
  let pl = powerList g h p
  in  dollar
        (   pretty g
        <>  "^"
        <>  Pretty.braces (pretty h)
        <+> eqv
        <+> genPL pl
        <+> eqv
        <+> pretty (fastPoweringL p pl)
        <+> mo
        <>  Pretty.braces (pretty p)
        )
