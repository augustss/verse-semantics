module ParseVerse where
--import Control.Monad
import qualified Control.Monad.State.Strict as S
import qualified Data.Map as M
import Data.Maybe
import Data.Void
import Text.Megaparsec hiding(try)
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char
--import qualified Text.Megaparsec.Char.Lexer as L

import ParseEBNF

data LexState = LexState
  { nest     :: !Bool
  , blockInd :: !String
  , lineInd  :: !String
  }
  deriving (Show)

initLexState :: LexState
initLexState = LexState { nest = True, blockInd = "", lineInd = "" }

type P = ParsecT Void String (S.State [LexState])

-- The regular try combinator does not backtrack the LexState.
-- This version has an error handler that resets the LexState.
try :: P a -> P a
try p = do
  ls <- S.get -- Get initial state.
  let err e = do
        S.put ls -- Reset state,
        parseError e -- and signal error.
  M.try (withRecovery err p) -- Use 'try' with special error handler.

runP :: P a -> FilePath -> String -> Either (ParseErrorBundle String Void) a
runP pa fn s = S.evalState (runParserT pa fn s) [initLexState]

parseDie :: P a -> FilePath -> String -> a
parseDie p fn file =
  case runP p fn file of
    Left err -> error $ errorBundlePretty err
    Right x -> x

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
  deriving (Show, Eq)

sSeq :: [ParseTree] -> ParseTree
sSeq axs =
  case filter (/= SUnit) axs of
    [x] -> x
    xs -> SSeq xs

{-
dropUnit :: ParseTree -> ParseTree
dropUnit (SSeq ss) = SSeq $ filter (/= SUnit) $ map dropUnit ss
dropUnit (SAlt i s) = SAlt i $ dropUnit s
dropUnit s@SChar{} = s
dropUnit s@SStr{} = s
dropUnit (SMany ss) = SMany $ map dropUnit ss
dropUnit (SOpt ms) = SOpt $ fmap dropUnit ms
dropUnit (SName n s) = SName n $ dropUnit s
dropUnit s@SUnit = s
-}

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
  let r = M.fromList $  [ (n, SName n <$> mkElemParse r x) | Rule{name=n, rhs=x} <- rs ]
                     ++ [ ("end", SUnit <$ eof) ]
  in  fromMaybe undefined $ M.lookup t r

type RuleEnv = M.Map String (P ParseTree)

mkElemParse :: RuleEnv -> Elem -> P ParseTree
mkElemParse r (Seq xs) = sSeq <$> mapM (mkElemParse r) xs
mkElemParse r (Alt xs) = choice $ zipWith (\ i p -> SAlt i <$> mkElemParse r p) [0..] xs
mkElemParse _ (Chr c)  = SChar <$> char c
mkElemParse _ (ChrRange l h) = SChar <$> satisfy (\ c -> l <= c && c <= h)
mkElemParse _ (Str s) = SStr <$> string s
mkElemParse r (Not x) = SUnit <$ notFollowedBy (mkElemParse r x)
mkElemParse r (Many x) = SMany <$> many (mkElemParse r x)
mkElemParse r (Look x) = SUnit <$ lookAhead (mkElemParse r x)
mkElemParse r (Opt x) = SOpt <$> optional (mkElemParse r x)
mkElemParse r (NonTerm n) = fromMaybe (error $ "undefined " ++ n) $ M.lookup n r
mkElemParse r (Code c) = SUnit <$ mkCode r c

mkCode :: RuleEnv -> Code -> P ()
mkCode _ Push = S.modify $ \ st -> head st : st
mkCode _ Pop  = S.modify tail
mkCode _ (Set s e) = S.modify $ \ st -> xset s (expr (head st) e) (head st) : tail st
mkCode r (CSeq cs) = mapM_ (mkCode r) cs
mkCode r (Parse s x) = do p <- mkElemParse r x; S.modify $ \ st -> xset s (show (flattenParseTree p)) (head st) : tail st
mkCode r (If e c mc) = do
  l <- S.gets head
  if read (expr l e) then mkCode r c else maybe (pure ()) (mkCode r) mc
  pure ()
mkCode _ Error = fail "Error"

xset :: String -> String -> LexState -> LexState
xset "Nest" e l = l { nest = read e }
xset "BlockInd" e l = l { blockInd = read e }
xset "LineInd" e l = l { lineInd = read e }
xset _ _ _ = undefined

expr :: LexState -> Expr -> String
expr _ (EVar "true") = show True
expr _ (EVar "false") = show False
expr l (EVar "Nest") = show (nest l)
expr l (EVar "BlockInd") = show (blockInd l)
expr l (EVar "LineInd") = show (lineInd l)
expr _ (EVar _s) = undefined
expr _ (EStr s) = show s
expr l (EGT e1 e2) = show (expr l e1 >  expr l e2)
expr l (ELE e1 e2) = show (expr l e1 <= expr l e2)
expr l (EEQ e1 e2) = show (expr l e1 == expr l e2)
expr l (Enot e) = show $ not $ read $ expr l e
expr l (Eand e1 e2) = show $ read (expr l e1) && read (expr l e2)
expr l (Eor  e1 e2) = show $ read (expr l e1) || read (expr l e2)
