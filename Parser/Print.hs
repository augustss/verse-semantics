module Print(
  module Text.PrettyPrint.HughesPJ,
  module Text.PrettyPrint.HughesPJClass,
  module Print,
  ) where
import Text.PrettyPrint.HughesPJ hiding(first)
import Text.PrettyPrint.HughesPJClass

indent :: Doc -> Doc
indent = nest 2

pPrintL :: (Pretty a) => PrettyLevel -> a -> Doc
pPrintL l x = pPrintPrec l 0 x

commaSep :: Pretty a => PrettyLevel -> Rational -> [a] -> Doc
commaSep l p xs = fsep . punctuate comma . map (pPrintPrec l p) $ xs

pp :: (Pretty a) => a -> IO ()
pp = putStrLn . prettyShow
