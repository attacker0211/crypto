module CodeGen.Chinese
  ( genChinese
  )
where
import           Algorithm.Chinese
import           CodeGen.Utils
import           Data.List                      ( intersperse )
import           Data.Maybe                     ( isNothing
                                                , fromJust
                                                )
import           Data.Text.Prettyprint.Doc      ( Pretty(..)
                                                , Doc
                                                , (<+>)
                                                )
import qualified Data.Text.Prettyprint.Doc     as Pretty

genChinese :: [(Int, Int)] -> Doc a
genChinese congruences =
  let
    n    = chineseMod congruences
    eq   = chineseEq n congruences
    invs = solveChineseEq eq
    res  = chineseRT congruences eq invs n
  in
    align (genCons congruences)
    <> Pretty.hardline
    <> genMod n congruences eq
    <> Pretty.hardline
    <> align (genEq eq 1)
    <> Pretty.hardline
    <> genInvs invs 1
    <> Pretty.hardline
    <> genRes (zip3 (fst <$> congruences) (fst <$> eq) (fromJust <$> invs))
              res
              n
 where
  genCons [] = ""
  genCons [x] =
    "x"
      <+> "&"
      <>  eqv
      <+> pretty (fst x)
      <+> mo
      <>  (Pretty.braces . pretty . snd) x
  genCons (x : xs) = genCons [x] <+> newl <> Pretty.hardline <> genCons xs
  genMod n congruences eq =
    ddollar
        (   "N ="
        <+> Pretty.hsep
              (intersperse Pretty.dot (pretty <$> (snd <$> congruences)))
        <+> "="
        <+> pretty n
        )
      <> Pretty.hardline
      <> align (genMod' eq n 1)
   where
    genMod' :: [(Int, Int)] -> Int -> Int -> Doc a
    genMod' [] _ _ = ""
    genMod' [x] n i =
      "N_"
        <>  pretty i
        <+> "&="
        <+> fracD "N" ("n_" <> pretty i)
        <+> "="
        <+> frac n (snd x)
        <+> "="
        <+> pretty (fst x)
    genMod' (x : xs) n i =
      genMod' [x] n i <> newl <> Pretty.hardline <> genMod' xs n (i + 1)

  genEq :: [(Int, Int)] -> Int -> Doc a
  genEq [] _ = ""
  genEq [x] i =
    "N_"
      <>  pretty i
      <>  "x_"
      <>  pretty i
      <+> "&"
      <>  eqv
      <+> pretty (1 :: Int)
      <+> mo
      <>  Pretty.braces (pretty (snd x))
  genEq (x : xs) i =
    genEq [x] i <+> newl <> Pretty.hardline <> genEq xs (i + 1)

  genInvs :: [Maybe Int] -> Int -> Doc a
  genInvs []  _ = ""
  genInvs [x] i = if isNothing x
    then "No solution for" <+> dollar ("x_" <> pretty i) <> newl
    else dollar ("x_" <> pretty i <+> "=" <+> pretty (fromJust x)) <> newl
  genInvs (x : xs) i = genInvs [x] i <> Pretty.hardline <> genInvs xs (i + 1)

  genRes zipEqs res n = if isNothing res
    then "No solution for this system of equation."
    else ddollar
      (   "x ="
      <+> prettyZipEqs zipEqs
      <+> "="
      <+> pretty (fromJust res)
      <+> mo
      <>  Pretty.braces (pretty n)
      )
   where
    prettyZipEqs [] = ""
    prettyZipEqs [(f, s, t)] =
      pretty f <+> cdot <+> pretty s <+> cdot <+> pretty t
    prettyZipEqs (x : xs) = prettyZipEqs [x] <+> "+" <+> prettyZipEqs xs
