module ENVDesugar(envDesugar) where
import FrontEnd.Expr

-- Turn some functions into their primitive counterpart.
envDesugar :: SrcEssential -> SrcEssential
envDesugar = desugar

desugar :: SrcEssential -> SrcEssential
-- x := :any        --->  exists x
desugar (DefineE i (Range (Variable (Ident _ "any")))) = DefineV i
-- prim := e1; e2   --->  e2
desugar (Variable (Ident _ s)) | Just p <- lookup s primOps = EPrim p
desugar (Seq (DefineE (Ident _ s) _) e) | Just _ <- lookup s primOps' = desugar e
desugar e = composOp desugar e

primOps :: [(String, PrimOp)]
primOps =
  [ ("any",          IsAny)
  , ("int",          IsInt)
  , ("nat",          IsInt)   -- No negative ints
  , ("operator'..'", DotDot)
  , ("prefix'-'",    Neg)
  , ("operator'+'",  Add)
  , ("operator'-'",  Sub)
  , ("operator'*'",  Mul)
  , ("operator'/'",  Div)
  , ("operator'>'",  Gt)
  , ("operator'<'",  Lt)
  ]

primOps' :: [(String, PrimOp)]
primOps' = primOps ++
  [ ("prefix'[]'",   undefined)
  ]
