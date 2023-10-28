module Language.Verse
  ( eval
  , eval'
  ) where

import Control.Monad ((<=<))
import Control.Monad.Abort
import Control.Monad.Fix
import Control.Monad.Ref
import Control.Monad.Supply
import Control.Monad.Trans.Class
import Control.Monad.Verse (VerseT)

import Data.ByteString (ByteString)

import Language.Verse.Desugar
import Language.Verse.Desugar.Exp
import Language.Verse.Error
import Language.Verse.Eval qualified as Eval
import Language.Verse.Label
import Language.Verse.Lexer
import Language.Verse.Mode
import Language.Verse.Parse
import Language.Verse.Rewrite
import Language.Verse.Val

import Debug.Trace
import Prettyprinter

eval :: ( MonadAbort Error m
        , MonadFix m
        , MonadRef m
        , MonadSupply Label m
        , EqRef (Ref m)
        ) => ByteString -> VerseT m FrozenVal
eval xs = do
  (e1, e2) <- liftEither $ runSupplyT $ do
    e <- rewrite =<< lift (runLexer parse xs)
    (,) <$> desugar Verification e <*> desugar Execution e
  traceM . show $ pretty e1
  Eval.eval $ verify (succeeds e1) `then'` e2

eval' :: ( MonadAbort Error m
         , MonadFix m
         , MonadRef m
         , MonadSupply Label m
         , EqRef (Ref m)
         ) => Mode -> ByteString -> VerseT m FrozenVal
eval' mode =
  Eval.eval <=<
  liftEither . (runSupplyT . (desugar mode <=< rewrite) <=< runLexer parse)
