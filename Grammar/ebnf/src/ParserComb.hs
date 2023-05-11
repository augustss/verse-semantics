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
--import qualified Text.Megaparsec.Char.Lexer as L

#if 0
import Data.Void
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

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

satisfy = M.satisfy

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

newtype Prsr s a = P { runP :: (String, s) -> [(a, (String, s))] }

instance Functor (Prsr s) where
  fmap f p = P $ \ t -> [ (f a, u) | (a, u) <- runP p t ]

instance Applicative (Prsr s) where
  (<*>) = ap
  pure a = P $ \ t -> [(a, t)]

instance Monad (Prsr s) where
  return = pure
  p >>= k = P $ \ t ->
    concat [ runP (k a) u | (a, u) <- runP p t ]

instance MonadFail (Prsr s) where
  fail _ = P $ \ _ -> []

instance MonadState s (Prsr s) where
  get = P $ \ t@(_, s) -> [(s, t)]
  put s = P $ \ (f, _) -> [((), (f, s))]

instance Alternative (Prsr s) where
  empty = P $ \ _ -> []
  p <|> q = P $ \ t -> runP p t ++ runP q t

runPrsr s (P p) _ f =
  case p (f, s) of
    [] -> Left "no parse"
    xs -> Right [(a, s') | (a, (_, s')) <- xs ]

choice [] = empty
choice ps = foldr1 (<|>) ps
satisfy f = P $ \ t ->
  case t of
    (c:cs, s) | f c -> [(c, (cs, s))]
    _ -> []
char c = satisfy (c ==)
string s = do mapM_ char s; pure s
eof = P $ \ t ->
  case t of ("", _) -> [((), t)]; _ -> []
p <?> _ = p
notFollowedBy p = P $ \ t ->
  case runP p t of
    [] -> [((), t)]
    _ -> []
lookAhead p = P $ \ t ->
  case runP p t of
    [] -> []
    _ -> [((), t)]
#endif

runPrsr :: (Show a, Show s) => s -> Prsr s a -> FilePath -> String -> Either String [(a, s)]
choice :: [Prsr s a] -> Prsr s a
satisfy :: (Char -> Bool) -> Prsr s Char
char :: Char -> Prsr s Char
string :: String -> Prsr s String
eof :: Prsr s ()
(<?>) :: Prsr s a -> String -> Prsr s a
notFollowedBy :: Prsr s a -> Prsr s ()
lookAhead :: Prsr s a -> Prsr s ()
