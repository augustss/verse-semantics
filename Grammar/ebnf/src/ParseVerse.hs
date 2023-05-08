module ParseVerse where
--import Control.Monad
import qualified Data.Map as M
import Data.Maybe

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

runP :: P a -> FilePath -> String -> Either String a
runP p fn f = fst $ runPrsr [initLexState] p fn f

parseDie :: P a -> FilePath -> String -> a
parseDie p fn file =
  case runP p fn file of
    Left err -> error err
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
  -- Compound nodes during translation
  | SNum String
  | SIdent String
  | SPath String
  deriving (Show, Eq)

sSeq :: [ParseTree] -> ParseTree
sSeq axs =
  case filter (/= SUnit) axs of
    [x] -> x
    xs -> SSeq xs

sMany :: [ParseTree] -> ParseTree
sMany axs = SMany $ filter (/= SUnit) axs

sOpt :: Maybe ParseTree -> ParseTree
sOpt (Just SUnit) = SUnit
sOpt mx = SOpt mx

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
flattenParseTree (SNum s) = s
flattenParseTree (SIdent s) = s
flattenParseTree (SPath s) = s

mkRulesParse :: String -> [Rule] -> P ParseTree
mkRulesParse t rs =
  let r = M.fromList $  [ (n, SName n <$> (mkElemParse r x <?> n)) | Rule{name=n, rhs=x} <- rs ]
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
mkElemParse r (Code c) = mkCode r c
mkElemParse _ (Deref v) = do l <- gets head; SStr <$> string (read (expr l (EVar v)))

mkCode :: RuleEnv -> Code -> P ParseTree
mkCode _ Push = SUnit <$ modify (\ st -> head st : st)
mkCode _ Pop  = SUnit <$ modify tail
mkCode _ (Set s e) = SUnit <$ modify (\ st -> xset s (expr (head st) e) (head st) : tail st)
mkCode r (CSeq cs) = sSeq <$> mapM (mkCode r) cs
mkCode r (Parse s x) = do p <- mkElemParse r x; modify (\ st -> xset s (show (flattenParseTree p)) (head st) : tail st); pure p
mkCode r (If e c mc) = do
  l <- gets head
  if read (expr l e) then mkCode r c else maybe (pure SUnit) (mkCode r) mc
mkCode _ Error = fail "Error"

xset :: String -> String -> LexState -> LexState
xset "Nest" e l = l { nest = read e }
xset "BlockInd" e l = l { blockInd = read e }
xset "LineInd" e l = l { lineInd = read e }
xset "LinePrefix" e l = l { linePrefix = read e }
xset "ThisInd" e l = l { thisInd = read e }
xset s _ _ = error $ "xset: undefined " ++ s

expr :: LexState -> Expr -> String
expr _ (EVar "true") = show True
expr _ (EVar "false") = show False
expr l (EVar "Nest") = show (nest l)
expr l (EVar "BlockInd") = show (blockInd l)
expr l (EVar "LineInd") = show (lineInd l)
expr l (EVar "LinePrefix") = show (linePrefix l)
expr l (EVar "ThisInd") = show (thisInd l)
expr _ (EVar s) = error $ "expr: undefined " ++ s
expr _ (EStr s) = show s
expr l (EGT e1 e2) = show (expr l e1 >  expr l e2)
expr l (ELE e1 e2) = show (expr l e1 <= expr l e2)
expr l (EEQ e1 e2) = show (expr l e1 == expr l e2)
expr l (Enot e) = show $ not $ read $ expr l e
expr l (Eand e1 e2) = show $ read (expr l e1) && read (expr l e2)
expr l (Eor  e1 e2) = show $ read (expr l e1) || read (expr l e2)

-------------------------

trimParseTree :: ParseTree -> ParseTree
trimParseTree (SSeq xs) = sSeq $ map trimParseTree xs
trimParseTree (SAlt i x) = SAlt i $ trimParseTree x
trimParseTree (SMany xs) = sMany $ map trimParseTree xs
trimParseTree (SOpt mx) = sOpt $ fmap trimParseTree mx
trimParseTree (SName "Scan" _) = SUnit
trimParseTree (SName "ScanKey" _) = SUnit
trimParseTree (SName "Space" _) = SUnit
trimParseTree (SName "NewLine" _) = SUnit
trimParseTree (SName "Num" x) = SNum $ flattenParseTree x
trimParseTree (SName "Ident" x) = SIdent $ flattenParseTree x
trimParseTree (SName "Path" x) = SPath $ flattenParseTree x
trimParseTree (SName n x) = SName n $ trimParseTree x
trimParseTree x = x
