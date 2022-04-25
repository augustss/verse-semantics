module Print(
  module Text.PrettyPrint.HughesPJ,
  module Text.PrettyPrint.HughesPJClass,
  module Print,
  ) where
import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJClass

indent :: Doc -> Doc
indent = nest 2

pPrintL :: (Pretty a) => PrettyLevel -> a -> Doc
pPrintL l x = pPrintPrec l 0 x

pp :: (Pretty a) => a -> IO ()
pp = putStrLn . prettyShow
