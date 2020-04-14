module CodeGen.Utils
  ( align
  , cdot
  , center
  , dollar
  , ddollar
  , eqv
  , frac
  , fracD
  , gcdP
  , genRow
  , mo
  , newl
  , pmo
  , tabl
  , ctabl
  , qd
  , qqd
  )
where
import           Data.Text.Prettyprint.Doc      ( Pretty(..)
                                                , Doc
                                                , (<+>)
                                                )
import           Data.Text.Prettyprint.Doc.Internal
                                                ( enclose )
import qualified Data.Text.Prettyprint.Doc     as Pretty

begin :: Doc a -> Doc a
begin x = "\\begin" <> Pretty.braces x

end :: Doc a -> Doc a
end x = "\\end" <> Pretty.braces x

balign :: Doc a
balign = begin "align*" <> Pretty.hardline

ealign :: Doc a
ealign = end "align*"

-- \begin{align*}
-- x
-- \end{align*}
align :: Doc a -> Doc a
align x = enclose balign ealign x

bcenter :: Doc a
bcenter = begin "center" <> Pretty.hardline

ecenter :: Doc a
ecenter = end "center"

-- \begin{center}
-- x
-- \end{center}
center :: Doc a -> Doc a
center x = enclose bcenter (Pretty.hardline <> ecenter) x

btabl :: Doc a -> Doc a
btabl x = begin "tabular" <> Pretty.braces x <> Pretty.hardline

etabl :: Doc a
etabl = end "tabular"

-- \begin{tabular}{col}
-- x
-- \end{tabular}
tabl :: Doc a -> Doc a -> Doc a
tabl col x = enclose (btabl col) etabl x

-- \begin{center}
-- \begin{tabular}{col}
-- x
-- \end{tabular}
-- \end{center}
ctabl :: Doc a -> Doc a -> Doc a
ctabl col x = center (tabl col x)

cdot :: Doc a
cdot = "\\cdot"

dl :: Doc a
dl = "$"

ddl :: Doc a
ddl = "$$"

dollar :: Doc a -> Doc a
dollar x = enclose dl dl x

ddollar :: Doc a -> Doc a
ddollar x = enclose ddl ddl x

eqv :: Doc a
eqv = "\\equiv"

mo :: Doc a
mo = "\\mod"

pmo :: Int -> Doc a
pmo x = "\\pmod" <> Pretty.braces (pretty x)

qqd :: Doc a
qqd = "\\qquad"

qd :: Doc a
qd = "\\quad"

newl :: Doc a
newl = "\\\\"

genRow :: [Int] -> Doc a
genRow []       = ""
genRow [x     ] = dollar (pretty x)
genRow (x : xs) = dollar (pretty x) <+> "&" <+> genRow xs

frac :: Int -> Int -> Doc a
frac x y = "\\frac" <> Pretty.braces (pretty x) <> Pretty.braces (pretty y)

fracD :: Doc a -> Doc a -> Doc a
fracD d1 d2 = "\\frac" <> Pretty.braces d1 <> Pretty.braces d2

gcdP :: Int -> Int -> Doc a
gcdP i1 i2 =
  "\\gcd" <> Pretty.parens (pretty i1 <+> Pretty.comma <+> pretty i2)
