module CodeGen.Chinese where
import           CodeGen.Chinese
import           CodeGen.Utils
import           Data.List                      ( intersperse )
import           Data.Maybe                     ( isNothing )
import           Data.Text.Prettyprint.Doc      ( Pretty(..)
                                                , Doc
                                                , (<+>)
                                                )
import qualified Data.Text.Prettyprint.Doc     as Pretty

genChinese :: [(Int, Int)] -> Doc a
genChinese congruences =
  let n    = chineseMod congruences
      eq   = chineseEq n congruences
      invs = solveChineseEq eq
      res  = chineseRT congruences invs
  in  align (genCons congruences)
        <> Pretty.hardline
        <> align (genMod n congruences eq)
        <> Pretty.hardline
        <> genEq eq 1
        <> Pretty.hardline
        <> genInvs invs 1
        <> Pretty.hardline
        <> genRes res n
 where
  genCons [] _ = ""
  genCons [x : xs] =
    "x"
      <+> "&"
      <>  eqv
      <+> pretty (fst x)
      <+> mo
      <>  (Pretty.braces . pretty . snd) x
      <+> newl
      <>  Pretty.hardline
      <>  genCons xs
  genMod n congruences invs =
    ddollar
        (   "N ="
        <+> intersperse Pretty.dot (prettyList congruences)
        <+> "="
        <+> pretty n
        )
      <> Pretty.hardline
      <> align (genMod' invs n 1)
   where
    genMod' [] _ _ = ""
    genMod' (x : xs) n i =
      "N_"
        <>  pretty i
        <+> "&="
        <+> fracD "N" ("n_" <> pretty i)
        <+> "="
        <+> frac n (snd x)
        <+> "="
        <+> fst x
  genEq [] _ = ""
  genEq x i =
    "N_"
      <>  pretty i
      <>  "x_"
      <>  pretty i
      <+> "&"
      <>  eqv
      <+> pretty 1
      <+> mo
      <>  Pretty.braces (pretty (snd x))
  genEq (x : xs) i = genEq x i <+> newl <> Pretty.hardline <> genEq xs (i + 1)

  genInvs []       _ = ""
  genInvs [x : xs] i = if isNothing x
    then "No solution for" <+> dollar ("x_" <> i) <> newl
    else ddollar ("x_" <> pretty i <+> "=" <+> x) <> genInvs xs (i + 1)

  genRes x n = if isNothing x
    then "No solution for x."
    else ddollar ("x =" <+> x <+> mo <> Pretty.braces n)
