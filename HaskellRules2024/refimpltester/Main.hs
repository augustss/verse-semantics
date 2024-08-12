{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where
--import Data.Char
import Control.Monad
import Control.Monad.Supply
--import Control.Monad.Trans.Except
--import Control.Monad.Verse (runVerseT)
import Control.Monad.Wrong

import Data.ByteString qualified as ByteString
import Data.List
--import Data.Maybe
import Data.Traversable

--import Language.Verse
import qualified Language.Verse.Effect.Split as S
import Language.Verse.Error
import Language.Verse.Ident
import Language.Verse.Label
import Language.Verse.Loc
import Language.Verse.Mode
import Language.Verse.Parse2
import qualified Language.Verse.Parse.Exp as P
import qualified Language.Verse.Pos as Pos
import qualified Language.Verse.Rewrite.Exp as R
import qualified Language.Verse.Rewrite as R
import Language.Verse.SimpleName
import qualified Language.Verse.Val as V

import Prettyprinter

import System.Directory
import System.FilePath
import System.IO.Error

import Test.HUnit hiding (Label)

import qualified FrontEnd.Flags as F
import qualified FrontEnd.Expr as F
import qualified FrontEnd.Desugar
import qualified FrontEnd.ToCore
import Data.Scientific
import Data.Text(unpack)
import qualified Rules.Core as Rules
import Rules.Verifier( verificationRules )
import TRS.Traced(term)
import Epic.Print(prettyShow)
--import Debug.Trace

main :: IO ()
main = do
  setCurrentDirectory "../VerseReferenceImpl"  -- XXX
  executionTest <- getTest Execution $ "test" </> "execution"
--  verificationTest <- getTest Verification $ "test" </> "verification"
  runTestTTAndExit $ TestList [executionTest] -- , verificationTest]

okTest :: String -> Bool
--okTest s | trace (show s) False = undefined
okTest s =
--  s == "16.verse" &&
  not ("verify/" `isInfixOf` s) &&
  not ("var/" `isInfixOf` s) &&
  not ("assume/" `isInfixOf` s) &&
  not ("attributes/" `isInfixOf` s) &&
  not ("struct/" `isInfixOf` s) &&
  not ("class/" `isInfixOf` s) &&
  notElem s structs &&
  notElem s floats &&
  notElem s overloads &&
  notElem s broken

broken :: [FilePath]
broken =
  [ -- All these seem to be instances of the same problem.
    "12.verse" -- , "17.verse", "19.verse", "21.verse", "32.verse", "33.verse"
  ]

structs :: [FilePath]
structs = [ "37.verse", "92.verse", "99.verse", "arrow/3.verse", "arrow/4.verse" ]

floats :: [FilePath]
floats = [ "45.verse", "55.verse", "56.verse", "57.verse" ]

overloads :: [FilePath]
overloads = [ "85.verse", "89.verse" ]

getTest :: Mode -> FilePath -> IO Test
getTest mode directory = do
  filePaths <- listDirectory' directory
  putStrLn $ "Files found: " ++ show (length filePaths)
  let verseFiles = take 10000 $ filter okTest $ sort $ filter ((== ".verse") . takeExtension) filePaths
  --error $ show verseFiles
  pure . TestList $ mkTestCase mode . (directory </>) <$> verseFiles

evalFile :: Mode
         -> FilePath
         -> IO (Either Error (Maybe [V.FrozenVal]))
evalFile _mode verseFile = do
  putStrLn $ "\nfile " ++ verseFile
  file <- ByteString.readFile verseFile
  case parse2 verseFile file of
    Left err -> return (Left err)
    Right e -> pure <$> rulesEval e

mkTestCase :: Mode -> FilePath -> Test
mkTestCase mode verseFile = TestLabel verseFile . TestCase $
  evalFile mode verseFile >>= \ case
    Left e -> handleError e
    Right Nothing -> handleError StuckError
    Right (Just xs) -> do
      let outFile = replaceExtension verseFile "out"
      expected <- readFile outFile `catchIOError` \ e ->
        if isDoesNotExistError e then pure "" else ioError e
      let actual = show $ foldr (\ x z -> pretty x <> line <> z) mempty xs
      assertEqual outFile expected actual
  where
    handleError e = do
      let errFile = replaceExtension verseFile "err"
      expected <- readFile errFile `catchIOError` \ e ->
        if isDoesNotExistError e then pure "" else ioError e
      let actual = show $ pretty e <> line
      assertEqual errFile expected actual

listDirectory' :: FilePath -> IO [FilePath]
listDirectory' x = do
  xs <- listDirectory x `catchIOError` const (pure [])
  join <$> (for xs $ \ y -> (y:) <$> fmap (y </>) <$> listDirectory' (x </> y))

--------------------------

rulesEval :: L (P.Exp SimpleName) -> IO (Maybe [V.FrozenVal])
rulesEval e = do
  --print e
  mv <- evalExpr (lexp (desugar e))
  --print v
  --when (v /= v) $ error "???"
  return $ toFrozen <$> mv

newtype M a = M { unM :: Label -> (Label, a) }
instance Functor M where
  fmap f ma = M $ \ l -> case unM ma l of (l', a) -> (l', f a)
instance Applicative M where
  pure a = M $ \ l -> (l, a)
  (<*>) = ap
instance Monad M where
  ma >>= k = M $ \ l -> case unM ma l of (l', a) -> unM (k a) l'
instance MonadWrong Error M where
  wrong e = error $ show e
instance MonadSupply Label M where
  supply = M $ \ l -> let !l' = l + 1 in (l', l)
runM :: M a -> a
runM (M a) = snd (a 0)

desugar :: L (P.Exp SimpleName) -> L (R.Exp L Ident)
desugar e = runM (R.rewrite e)

lexp :: L (R.Exp L Ident) -> F.SrcExpr
lexp (L l e) = expToSrcExpr l e

strIdent :: Loc -> String -> F.Ident
strIdent (Loc (Pos.Pos l c _) _) s = F.Ident (F.mkLoc "?" l c) s

ident :: Loc -> Ident -> F.Ident
ident l i = strIdent l (f i)
  where f (Name s) = unpack s
        f (Label l) = "_" ++ show l

inOp :: Loc -> Ident -> F.Ident
inOp l s = ident l s

preOp :: Loc -> Ident -> F.Ident
preOp l s = ident l s

{-
postOp :: Loc -> Ident -> F.Ident
postOp l s = ident l s
-}

macro :: Loc -> Ident -> F.Ident
macro l s = ident l s

expToSrcExpr :: Loc -> R.Exp L Ident -> F.SrcExpr
expToSrcExpr l (e1 R.:=:  e2) = F.InfixOp (lexp e1) (inOp l "=")  (lexp e2)
-- expToSrcExpr l (e1 R.:.:  e2) = F.InfixOp (lexp e1) (inOp l ".")  (lexp e2)
expToSrcExpr l (e1 R.:|:  e2) = F.InfixOp (lexp e1) (inOp l "|")  (lexp e2)
expToSrcExpr _ (R.List es) = F.Seq (map lexp es)
expToSrcExpr l (R.Where e1 e2) = F.InfixOp (lexp e1) (inOp l "where") (lexp e2)
expToSrcExpr _ R.Fail = F.Fail
expToSrcExpr l (R.One e) = F.Macro1 (macro l "one") [] (lexp e)
expToSrcExpr l (R.All e) = F.Macro1 (macro l "all") [] (lexp e)
expToSrcExpr l (R.Not e) = F.PrefixOp (preOp l "not") (lexp e)
expToSrcExpr l (R.Verify e) = F.Macro1 (macro l "verify") [] (lexp e)
expToSrcExpr l (R.Check eff e) = F.Macro1 (strIdent l (effToString eff)) [] (lexp e)
expToSrcExpr l (R.OfType e1 e2) = F.InfixOp (lexp e1) (inOp l ":") (lexp e2)
expToSrcExpr l (R.Assume e) = F.Macro1 (macro l "assume") [] (lexp e)
-- expToSrcExpr l (R.Module e) = XXX
-- expToSrcExpr l (R.Struct e) = XXX
-- expToSrcExpr l (R.Class e) = XXX
-- expToSrcExpr l (R.Inst e1 e2) = XXX
-- expToSrcExpr l (R.Enum e) = XXX
expToSrcExpr _ (R.IfThenElse e1 e2 e3) = F.If3 (lexp e1) (lexp e2) (lexp e3)
expToSrcExpr _ (R.ForDo e1 e2) = F.For2 (lexp e1) (lexp e2)
expToSrcExpr _ (R.Block e) = F.Block (lexp e)
expToSrcExpr _ (R.BracketInvoke f a) = F.ApplyD (lexp f) (lexp a)
expToSrcExpr _ (R.Exists (L l i)) = F.DefineV (ident l i)
-- expToSrcExpr _ (Forall e) = XXX
-- expToSrcExpr _ (Alloc2 ) = XXX
-- expToSrcExpr _ (Alloc3 ) = XXX
expToSrcExpr l (R.Set (L l' x) e) = F.Set (F.Variable (ident l' x)) (ident l "=") (lexp e)
expToSrcExpr _ (R.Tuple es) = F.Tuple (map lexp es)
expToSrcExpr _ (R.Truth e) = F.Truth (lexp e)
expToSrcExpr _ (R.Int i) = F.Lit (F.LInt i)
expToSrcExpr _ (R.Float f) = F.Lit (F.LRat (fromFloatDigits f) (show f))
expToSrcExpr _ (R.Char c) = F.Lit (F.LChar (toEnum (fromEnum c)))
expToSrcExpr _ (R.Char32 c) = F.Lit (F.LChar c)
expToSrcExpr l (R.Lam e1 oc eff e2) = F.Function [(lexp e1, rs)] (lexp e2)
  where rs = [ strIdent l (case oc of R.O -> "open"; R.C -> "closed")
             , strIdent l (effToString eff)
             ]
expToSrcExpr l (R.InfixColonEqual _ q (L l' x) e) | ok q = F.InfixOp (F.Variable (ident l' x)) (inOp l ":=") (lexp e)
  where ok R.Var = False
        ok _ = True
expToSrcExpr l (R.PrefixColon e) = F.PrefixOp (preOp l ":") (lexp e)
expToSrcExpr l (R.MixfixArrowColonEqual (L lx x) (L ly y) e) = F.InfixOp lhs (ident l ":=") (lexp e)
  where lhs = F.InfixOp (F.Variable (ident lx x)) (strIdent l "->") (F.Variable (ident ly y))
expToSrcExpr l (R.Name n) = F.Variable (ident l n)
-- expToSrcExpr QualName
expToSrcExpr _ (R.IfArchetypeName _ e1 e2) | x1 == x2 = x1
  where x1 = lexp e1; x2 = lexp e2
expToSrcExpr _ (R.IfArchetypeName _ _ e2) = lexp e2
-- expToSrcExpr Domain
expToSrcExpr _ e = error $ "expToSrcExpr: unimp " ++ show (pretty e) ++ "\n" ++ show e

effToString :: S.Effect -> String
effToString eff = case eff of S.Fails -> "fails"; S.Succeeds -> "succeeds"; S.Decides -> "decides"

toFrozen :: Rules.Expr -> [V.FrozenVal]
toFrozen (Rules.Lit (Rules.LInt i)) = pure $ V.FrozenVal (Just (V.Int i))
toFrozen (Rules.Lit (Rules.LRat i _)) = pure $ V.FrozenVal (Just (V.Rational $ toRational i))
toFrozen (Rules.Lit (Rules.LChar i)) = pure $ V.FrozenVal (Just (V.Char32 i))
toFrozen (Rules.Arr vs) = do fs <- mapM toFrozen vs; pure $ V.FrozenVal (Just (V.Tuple fs))
toFrozen (Rules.Tru v) = do f <- toFrozen v; pure $ V.FrozenVal (Just (V.Truth f))
toFrozen (e1 Rules.:|: e2) = toFrozen e1 ++ toFrozen e2
toFrozen (Rules.Fail) = []
toFrozen e = error $ "toFrozen: " ++ prettyShow e

isOKResult :: Rules.Expr -> Bool
isOKResult (Rules.Lit _) = True
isOKResult (Rules.Arr es) = all isOKResult es
isOKResult (Rules.Tru e) = isOKResult e
isOKResult (e1 Rules.:|: e2) = isOKResult e1 && isOKResult e2
isOKResult (Rules.Fail) = True
isOKResult _ = False

--------------

srcToCore :: F.Flags -> Bool -> F.SrcExpr -> IO Rules.Expr
srcToCore flags add_verification e = do
--  putStrLn $ "\ne=\n" ++ prettyShow e
  e1 <- FrontEnd.Desugar.desugar flags add_verification e
--  putStrLn $ "\ne1=\n" ++ prettyShow e1
  e2 <- FrontEnd.ToCore.convertToCore flags e1
--  putStrLn $ "\ne2=\n" ++ prettyShow e2
  let e3 = Rules.prep e2
  return e3

evalExpr :: F.SrcExpr -> IO (Maybe Rules.Expr)
evalExpr e = do
  ce <- srcToCore F.defaultFlags Prelude.False e
  let (r, tr) = Rules.normalize steps verificationRules ce
      v = term tr
      steps = 2000
  case r of
    Rules.NormOK | isOKResult v -> return (Just v)
                 | otherwise -> return Nothing
    Rules.NormExpired -> do
      putStrLn "*** Ran out of fuel"
      return Nothing
    Rules.NormInvalid -> error $ "Invalid reduction result:\n" ++ prettyShow v
