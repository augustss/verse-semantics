module AnsiStyle
  ( errorColor
  , bolded
  ) where

import Prettyprinter
import Prettyprinter.Render.Terminal

errorColor :: AnsiStyle
errorColor = colorDull Red

bolded :: Doc AnsiStyle -> Doc AnsiStyle
bolded = annotate bold
