{-# OPTIONS_GHC -Wno-orphans #-}

module Epic.Print
  ( module Epic.Print,
    module Text.PrettyPrint.HughesPJClass,
  )
where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text
import Text.PrettyPrint.HughesPJClass hiding (Str, first)
import Prelude hiding ((<>))

instance Pretty Data.Text.Text where
  pPrintPrec l p = pPrintPrec l p . Data.Text.unpack

commaSep :: (Pretty a) => PrettyLevel -> [a] -> Doc
commaSep l es = fsep $ punctuate comma (map (pPrintL l) es)

pPrintL :: (Pretty a) => PrettyLevel -> a -> Doc
pPrintL l = pPrintPrec l 0

indent :: Doc -> Doc
indent = nest 2

pp :: (Pretty a) => a -> IO ()
pp = putStrLn . prettyShow

ppl :: (Pretty a) => PrettyLevel -> a -> IO ()
ppl l = putStrLn . render . pPrintL l

ppx :: (Pretty a) => a -> IO ()
ppx = putStrLn . renderStyle s . pPrintL prettyNormal
  where s = style{ lineLength = 150, ribbonsPerLine = 1.2 }

type PPPrec = Rational

data PPFixity = PPInfixL PPPrec | PPInfixR PPPrec | PPInfix PPPrec
  deriving (Eq, Show)

ppInfix :: PPPrec -> PPFixity -> (PPPrec -> Doc) -> Doc -> (PPPrec -> Doc) -> Doc
ppInfix prec fixity left op right =
  maybeParens (prec > opPrec) $ left leftPrec <> op <> right rightPrec
  where
    (opPrec, leftPrec, rightPrec) =
      case fixity of
        PPInfixL p -> (p, p, p + 1)
        PPInfixR p -> (p, p + 1, p)
        PPInfix p -> (p, p + 1, p + 1)

pPrintPrecF :: (Pretty a) => PrettyLevel -> a -> PPPrec -> Doc
pPrintPrecF l a p = pPrintPrec l p a

instance (Pretty v) => Pretty (S.Set v) where
  pPrintPrec l p = pPrintPrec l p . S.toList

instance (Pretty k, Pretty v) => Pretty (M.Map k v) where
  pPrintPrec l p = pPrintPrec l p . M.toList

ppStruct :: String -> [(String, Doc)] -> Doc
ppStruct con fs =
  vcat
    [ text con <+> text "{",
      indent $ vcat $ punctuate comma $ map (\(f, v) -> text f <+> text "=" <+> v) fs,
      text "}"
    ]
