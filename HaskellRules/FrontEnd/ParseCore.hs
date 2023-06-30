module FrontEnd.ParseCore(pCore, pCoreFile) where
import Control.Monad(void)
import Data.Maybe
import Epic.Print
import FrontEnd.Desugar(exprToCore)
import FrontEnd.Expr
import FrontEnd.Flags

import Text.Megaparsec(sepBy, sepBy1, many, eof, choice, some, optional, (<|>))
import FrontEnd.Parse(P, pOp, pParens, skip, pLiteral, pIdent, pMacroName, pBraces, try, pKeyword, lexeme, string)
import FrontEnd.Desugar(dsScope)

-- Parse Core
pCoreFile :: P Core
pCoreFile = skip *> pCore <* eof

pCore :: P Core
pCore = exprToCore flg . dsScope flg <$> pSeq
  where flg = defaultFlags{ fSplit = False }

-- XXX pDef, pLam
-- XXX primops

pExists :: P Expr
pExists = exists <$> (pQuant *> some pIdent <* pOp ".") <*> pSeq
  where
    exists :: [Ident] -> Expr -> Expr
    exists is e = Exists is e
    pQuant = pKeyword "exists" <|> pKeyword "exi" <|> pKeyword "ex" <|> pKeyword "E"
      -- <|> void (pOp "∃")

pLam :: P Expr
pLam = lam <$> (pLambda *> some pIdent <* pOp ".") <*> pSeq
  where
    lam :: [Ident] -> Expr -> Expr
    lam is e = foldr Lam e is
    pLambda = pKeyword "lam" <|> pKeyword "lambda" <|> void (pOp "\\")
      -- <|> pKeyword "λ"

pSeq :: P Expr
pSeq = choice [ pLam, pExists, cons <$> pEqu <*> optional (pOp ";" *> pSeq) ]
  where
    cons e Nothing = e
    cons e (Just e') = Seq [e, e']

pEqu :: P Expr
pEqu = try (DefineE <$> (pIdent <* pOp ":=") <*> pChoice)
       <|>
       foldr1 Unify <$> sepBy1 pChoice (pOp "=")

pChoice :: P Expr
pChoice = foldr1 Choice <$> sepBy1 pApply (pOp "|")

pApply :: P Expr
pApply = do
  e1 <- pAtom
  let app f [] = f
      app f (a:as) = app (ApplyD f a) as
      pCall :: P Expr
      pCall = app e1 <$> many pTuple
      pBinOp = do i <- pOper; e2 <- pAtom; pure (ApplyD (Variable i) (Array [e1, e2]))
  pBinOp <|> pCall

pOper :: P Ident
pOper = choice $ map (\ o -> const (Ident noLoc ("in'" ++ o ++ "'")) <$> pOp o)
  [ ">=", "<=", "<>", "+", "-", "*", "/" ]

pTuple :: P Expr
pTuple = try (pParens (pure (Array [])))
         <|>
         pParens pComma

pComma :: P Expr
pComma = try (arr <$> pEqu <*> some (pOp "," *> pEqu))
         <|>
         pSeq
  where arr x xs = Array (x:xs)

pAtom :: P Expr
pAtom = choice [pTuple, pLiteral, pName, pMacro, pArray]

pName :: P Expr
pName = do
  i@(Ident l s) <- pIdent
  let ops = [ ("fail", Fail)
            , ("gt", vi "in'>'")
            , ("lt", vi "in'<'")
            , ("add", vi "in'+'")
            , ("addto", vi "in'+='")
            , ("isInt", vi "isInt$")
            ]
      vi = Variable . Ident l
  pure $ fromMaybe (Variable i) $ lookup s ops

pArray :: P Expr
pArray =
    Array <$> (pKeyword "array" *> pBraces (sepBy pSeq (pOp ",")))
  <|>
    pLT *> (Array <$> sepBy pSeq (pOp ",")) <* pGT
 where pLT = lexeme (string "<")
       pGT = lexeme (string ">")

pMacro :: P Expr
pMacro = mac <$> pMacroName <*> pBraces pSeq
  where
    mac i@(Ident _ s) e | s `elem` macros = Macro1 i [] e
    mac i _ = error $ "Unknown macro " ++ prettyShow i
    macros = ["one", "all", "succeeds", "decides"]
