module ParserComb(
  S.get, S.gets, S.put, S.modify,
  Prsr, runPrsr,
  satisfy, char, string, eof,
  choice,
  many, optional,
  (<?>),
  notFollowedBy, lookAhead,
  )where
import qualified Control.Monad.State.Strict as S
import Data.Void
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
--import qualified Text.Megaparsec.Char.Lexer as L

type Prsr s a = M.ParsecT Void String (S.State s) a

-- The regular try combinator does not backtrack the LexState.
-- This version has an error handler that resets the LexState.
try :: Prsr s a -> Prsr s a
try p = do
  ls <- S.get -- Get initial state.
  let err e = do
        S.put ls -- Reset state,
        M.parseError e -- and signal error.
  M.try (M.withRecovery err p) -- Use 'try' with special error handler.

choice :: [Prsr s a] -> Prsr s a
choice = M.choice . map try

runPrsr :: s -> Prsr s a -> FilePath -> String -> (Either String a, s)
runPrsr s pa fn str =
  case S.runState (M.runParserT pa fn str) s of
    (r, s') -> (either (Left . M.errorBundlePretty) Right r, s')

satisfy :: (Char -> Bool) -> Prsr s Char
satisfy = M.satisfy

char :: Char -> Prsr s Char
char = M.char

string :: String -> Prsr s String
string = M.string

eof :: Prsr s ()
eof = M.eof

many :: Prsr s a -> Prsr s [a]
many = M.many

optional :: Prsr s a -> Prsr s (Maybe a)
optional = M.optional

(<?>) :: Prsr s a -> String -> Prsr s a
(<?>) = (M.<?>)

notFollowedBy :: Prsr s a -> Prsr s ()
notFollowedBy = M.notFollowedBy

lookAhead :: Prsr s a -> Prsr s a
lookAhead = M.lookAhead
