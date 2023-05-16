{-# LANGUAGE DeriveDataTypeable #-}
module ParseVerse(
  mkRulesParse,
  parseDie, parsesDie,
  ParseTree(..),
  flattenParseTree,
  ) where
--import Control.Monad
import Data.Data (Data)
import Data.Generics.Uniplate.Data
import Data.List
import qualified Data.Map as M
import Data.Maybe
import GHC.Stack
import Text.Read(readMaybe)

import ParseEBNF
import ParserComb

data LexState = LexState
  { nest       :: Bool
  , blockInd   :: String
  , lineInd    :: String
  , linePrefix :: String
  , thisInd    :: String -- just temporary storage
  }
  deriving (Show)

initLexState :: LexState
initLexState = LexState { nest = True, blockInd = "", lineInd = "", linePrefix = "", thisInd = "" }

type P a = Prsr [LexState] a

runP :: (Show a) => P a -> FilePath -> String -> Either String [a]
runP p fn f = either Left (Right . map fst) $ runPrsr [initLexState] p fn f

parseDie :: P ParseTree -> FilePath -> String -> ParseTree
parseDie p fn file =
  case runP p fn file of
    Left err -> error err
    Right xs ->
      case nub (map hackAmbig xs) of
        [a] -> a
        as -> error $ "Ambiguous:\n" ++ unlines (map show as)

parsesDie :: P ParseTree -> FilePath -> String -> [ParseTree]
parsesDie p fn file =
  case runP p fn file of
    Left err -> error err
    Right xs -> xs

---------------------------------------------------------

data ParseTree
  = SSeq [ParseTree]
  | SAlt Int ParseTree
  | SChar Char
  | SStr String
  | SMany [ParseTree]
  | SOpt (Maybe ParseTree)
  | SName String ParseTree
  | SUnit
  deriving (Show, Eq, Data)

flattenParseTree :: ParseTree -> String
flattenParseTree (SSeq ss) = concatMap flattenParseTree ss
flattenParseTree (SAlt _ s) = flattenParseTree s
flattenParseTree (SChar c) = [c]
flattenParseTree (SStr s) = s
flattenParseTree (SMany ss) = concatMap flattenParseTree ss
flattenParseTree (SOpt ms) = maybe "" flattenParseTree ms
flattenParseTree (SName _ s) = flattenParseTree s
flattenParseTree SUnit = ""

mkRulesParse :: String -> [Rule] -> P ParseTree
mkRulesParse t rs =
  let r = M.fromList $  [ (n, SName n <$> (mkElemParse r x <?> n)) | Rule{name=n, rhs=x} <- rs ]
                     ++ [ ("end", SUnit <$ eof) ]
  in  fromMaybe undefined $ M.lookup t r

type RuleEnv = M.Map String (P ParseTree)

mkElemParse :: RuleEnv -> Elem -> P ParseTree
mkElemParse r (Seq xs) = SSeq <$> mapM (mkElemParse r) xs
mkElemParse r (Alt xs) = choice $ zipWith (\ i p -> SAlt i <$> mkElemParse r p) [0..] xs
mkElemParse _ (Chr c)  = SChar <$> char c
mkElemParse _ (ChrRange l h) = SChar <$> satisfy (show l ++ ".." ++ show h) (\ c -> l <= c && c <= h)
mkElemParse _ (Str s) = SStr <$> string s
mkElemParse r (Not x) = SUnit <$ notFollowedBy (mkElemParse r x)
mkElemParse r (Many x) = SMany <$> many (mkElemParse r x)
mkElemParse r (EMany x) = SMany <$> emany (mkElemParse r x)
mkElemParse r (Look x) = SUnit <$ lookAhead (mkElemParse r x)
mkElemParse r (Opt x) = SOpt <$> optional (mkElemParse r x)
mkElemParse r (NonTerm n) = fromMaybe (error $ "undefined " ++ n) $ M.lookup n r
mkElemParse r (Code c) = mkCode r c
mkElemParse _ (Deref v) = do l <- gets head; SStr <$> string (expr l (EVar v))

mkCode :: RuleEnv -> Code -> P ParseTree
mkCode _ Push = SUnit <$ modify (\ st -> head st : st)
mkCode _ Pop  = SUnit <$ modify tail
mkCode _ (Set s e) = SUnit <$ modify (\ st -> xset s (expr (head st) e) (head st) : tail st)
mkCode r (CSeq cs) = SSeq <$> mapM (mkCode r) cs
mkCode r (Parse s x) = do p <- mkElemParse r x; modify (\ st -> xset s (flattenParseTree p) (head st) : tail st); pure p
mkCode r (If e c mc) = do
  l <- gets head
  if xread (expr l e) then mkCode r c else maybe (pure SUnit) (mkCode r) mc
mkCode _ Error = fail "Error"

xset :: String -> String -> LexState -> LexState
xset "Nest" e l = l { nest = xread e }
xset "BlockInd" e l = l { blockInd = e }
xset "LineInd" e l = l { lineInd = e }
xset "LinePrefix" e l = l { linePrefix = e }
xset "ThisInd" e l = l { thisInd = e }
xset s _ _ = error $ "xset: undefined " ++ s

expr :: LexState -> Expr -> String
expr _ (EVar "true") = show True
expr _ (EVar "false") = show False
expr l (EVar "Nest") = show (nest l)
expr l (EVar "BlockInd") =  (blockInd l)
expr l (EVar "LineInd") =  (lineInd l)
expr l (EVar "LinePrefix") =  (linePrefix l)
expr l (EVar "ThisInd") =  (thisInd l)
expr _ (EVar s) = error $ "expr: undefined " ++ s
expr _ (EStr s) =  s
expr l (EGT e1 e2) = show (expr l e1 >  expr l e2)
expr l (ELT e1 e2) = show (expr l e1 <  expr l e2)
expr l (EGE e1 e2) = show (expr l e1 >= expr l e2)
expr l (ELE e1 e2) = show (expr l e1 <= expr l e2)
expr l (EEQ e1 e2) = show (expr l e1 == expr l e2)
expr l (Enot e) = show $ not $ xread $ expr l e
expr l (Eand e1 e2) = show $ xread (expr l e1) && xread (expr l e2)
expr l (Eor  e1 e2) = show $ xread (expr l e1) || xread (expr l e2)

xread :: (HasCallStack, Read a) => String -> a
xread s = fromMaybe (error s) $ readMaybe s

-----------------------------

-- Fix some silly grammar ambiguities.
hackAmbig :: ParseTree -> ParseTree
hackAmbig = transformBi f
  where f (SOpt (Just t)) | flattenParseTree t == "" = SOpt Nothing
        f x = x
