{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ParserComb(
  get, gets, put, modify,
  Prsr, runPrsr,
  satisfy, char, string, eof,
  choice,
  many, optional,
  (<?>),
  notFollowedBy, lookAhead,
  )where
import Control.Monad.State.Strict
import Data.Char(isSpace)
import Data.List
--import Debug.Trace

#if 0
import Data.Void
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
--import qualified Text.Megaparsec.Char.Lexer as L

type Prsr s a = M.ParsecT Void String (State s) a

-- The regular try combinator does not backtrack the LexState.
-- This version has an error handler that resets the LexState.
try :: Prsr s a -> Prsr s a
try p = do
  ls <- get -- Get initial state.
  let err e = do
        put ls -- Reset state,
        M.parseError e -- and signal error.
  M.try (M.withRecovery err p) -- Use 'try' with special error handler.

choice = M.choice . map try

runPrsr s pa fn str =
  case runState (M.runParserT pa fn str) s of
    (Left e, _) -> Left $ M.errorBundlePretty e
    (Right a, s') -> Right [(a, s')]

satisfy _ = M.satisfy

char = M.char

string = M.string

eof = M.eof

many :: Prsr s a -> Prsr s [a]
many = M.many

optional :: Prsr s a -> Prsr s (Maybe a)
optional = M.optional

(<?>) = (M.<?>)

notFollowedBy = M.notFollowedBy

lookAhead = pure () <$ M.lookAhead
#else
import Control.Applicative

data LastFail
  = LastFail Int [(String, [String])]
  deriving (Show)

noFail :: LastFail
noFail = LastFail maxBound []

longest :: LastFail -> LastFail -> LastFail
longest lf1@(LastFail l1 x1) lf2@(LastFail l2 x2) =
  case compare l1 l2 of
    LT -> lf1
    GT -> lf2
    EQ -> LastFail l1 (x1 ++ x2)

longests :: [LastFail] -> LastFail
longests [] = undefined
longests xs = foldl1 longest xs

data Res s a = Many [(a, (String, s))] LastFail
  deriving (Show)

newtype Prsr s a = P { runP :: (String, s) -> Res s a}

instance Functor (Prsr s) where
  fmap f p = P $ \ t ->
    case runP p t of
      Many aus lf -> Many [ (f a, u) | (a, u) <- aus ] lf

instance Applicative (Prsr s) where
  (<*>) = ap
  pure a = P $ \ t -> Many [(a, t)] noFail

instance Monad (Prsr s) where
  return = pure
  p >>= k = P $ \ t ->
    case runP p t of
      Many aus plf ->
        let xss = [ runP (k a) u | (a, u) <- aus ]
        in  let (rss, lfs) = unzip [ (rs, lf) | Many rs lf <- xss ]
            in  Many (concat rss) (longests $ plf : lfs)

instance MonadFail (Prsr s) where
  fail m = P $ \ (r, _) -> Many [] (LastFail (length r) [(m, [])])

instance MonadState s (Prsr s) where
  get = P $ \ t@(_, s) -> Many [(s, t)] noFail
  put s = P $ \ (f, _) -> Many [((), (f, s))] noFail

instance Alternative (Prsr s) where
  empty = P $ \ (r, _) -> Many [] (LastFail (length r) [("empty", [])])
  p <|> q = P $ \ t ->
    case (runP p t, runP q t) of
      (Many a lfa, Many b lfb) -> Many (a ++ b) (longest lfa lfb)

runPrsr s (P p) _ f =
  case p (f, s) of
    Many [] lf -> Left $ formatFailed f lf
    Many xs _  -> Right [(a, s') | (a, (_, s')) <- xs ]

formatFailed :: String -> LastFail -> String
formatFailed file (LastFail len xs) | len == maxBound = "No failure"
                                    | otherwise =
  let (pre, post) = splitAt (length file - len) file
      (line, col) = foldl f (1::Int, 0::Int) pre
        where f (l, _) '\n' = (l+1, 0)
              f (l, c) _    = (l, c+1)
      xs' = nub $ map trim xs
      pr e = "   expeced: " ++ e
      trim (_, es) = last $ init $ "" : "" : es
  in  "line " ++ show line ++ ", col " ++ show col ++ ":\n" ++
      "   found: " ++ show (takeWhile (not . isSpace) post) ++ "\n" ++
      unlines (map pr xs')

choice [] = empty
choice ps = foldr1 (<|>) ps
satisfy msg f = P $ \ t ->
  case t of
    (c:cs, s) | f c -> Many [(c, (cs, s))] noFail
    (cs, _) -> Many [] (LastFail (length cs) [("satisfy", [msg])])
char c = P $ \ t ->
  case t of
    (c':cs, s) | c == c' -> Many [(c, (cs, s))] noFail
    (cs, _) -> Many [] (LastFail (length cs) [("char", [show c])])
string str = P $ \ t ->
  case t of
    (cs, s) | Just cs' <- stripPrefix str cs -> Many [(str, (cs', s))] noFail
            | otherwise -> Many [] (LastFail (length cs) [("string", [show str])])
eof = P $ \ t ->
  case t of
    ("", _) -> Many [((), t)] noFail
    (cs, _) -> Many [] (LastFail (length cs) [("eof", ["end-of-file"])])
p <?> e = P $ \ t ->
--  trace ("<?> " ++ show e) $
  case runP p t of
    Many rs (LastFail l xs) -> Many rs (LastFail l [(m, e:es) | (m, es) <- xs ])
notFollowedBy p = P $ \ t@(cs,_) ->
  case runP p t of
    Many [] _ -> Many [((), t)] noFail
    Many _ _ -> Many [] (LastFail (length cs) [("notFollowedBy", [])])
lookAhead p = P $ \ t ->
  case runP p t of
    Many [] (LastFail l xs) -> Many [] (LastFail l [("lookAhead-" ++ m, es) | (m, es) <- xs])
    Many _ _ -> Many [((), t)] noFail
#endif

runPrsr :: (Show a, Show s) => s -> Prsr s a -> FilePath -> String -> Either String [(a, s)]
choice :: [Prsr s a] -> Prsr s a
satisfy :: String -> (Char -> Bool) -> Prsr s Char
char :: Char -> Prsr s Char
string :: String -> Prsr s String
eof :: Prsr s ()
(<?>) :: Prsr s a -> String -> Prsr s a
notFollowedBy :: Prsr s a -> Prsr s ()
lookAhead :: Prsr s a -> Prsr s ()
