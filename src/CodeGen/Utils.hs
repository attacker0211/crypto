module CodeGen.Utils
  ( align
  , cdot
  , center
  , dollar
  , eqv
  , mo
  , newl
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
begin x = "\\begin" <> braces x

end :: Doc a -> Doc a
end x = "\\end" <> braces x

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
center x = enclose bcenter ecenter x

btabl :: Doc a -> Doc a
btabl x = begin "tabular" <> braces x <> Pretty.hardline

etabl :: Doc a -> Doc a
etabl x = end "tabular"

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

dollar :: Doc a -> Doc a
dollar x = enclose dl dl x

eqv :: Doc a
eqv = "\\equiv"

mo :: Doc a
mo = "\\mod"

qqd :: Doc a
qqd = "\\qquad"

qd :: Doc a
qd = "\\quad"

newl :: Doc a
newl = "\\\\"

genRow :: [Int] -> Doc a
genRow []       = ""
genRow [a     ] = dollar a
genRow [x : xs] = dollar a <+> "&" <+> genRow xs
