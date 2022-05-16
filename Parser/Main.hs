{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception
import Control.Monad

import Print
import Desugar
import Expr
import Parse
import Command
import Core
import Eval

tryIt :: IO b -> (a -> IO b) -> IO a -> IO b
tryIt iob aiob ioa = do
  e <- try ioa
  case e of
    Left (exn :: SomeException) -> do
      print exn
      iob
    Right a -> aiob a

-------------------

main :: IO ()
main = runCommand command

data CState = CState
  { lastExpr :: SomeExpr
  }

data SomeExpr = NoExpr | Parsed Expr | Desugared Expr | Cored Core

asExpr :: SomeExpr -> Expr
asExpr NoExpr = error "No current expression"
asExpr (Parsed e) = e
asExpr (Desugared e) = e
asExpr (Cored _) = error "Current expression is Core"

asDesugared :: SomeExpr -> Expr
asDesugared (Parsed e) = desugar e
asDesugared e = asExpr e

asCore :: SomeExpr -> Core
asCore (Cored e) = e
asCore e = exprToCore $ asDesugared e

instance Show SomeExpr where
  show NoExpr = "No current expression"
  show (Parsed e) = show e
  show (Desugared e) = show e
  show (Cored e) = show e

instance Pretty SomeExpr where
  pPrintPrec _ _ NoExpr = text "No current expression"
  pPrintPrec l p (Parsed e) = pPrintPrec l p e
  pPrintPrec l p (Desugared e) = pPrintPrec l p e
  pPrintPrec l p (Cored e) = pPrintPrec l p e

command :: Command CState
command = Command
  { c_commands =
      [ Cmd "read FILE"       "Parse a file"                          cRead
      , Cmd "desugar [EXPR]"  "Desugar [last] expression"             cDesugar
      , Cmd "show [EXPR]"     "Show [last] expression"                cShow
      , Cmd "simplify [EXPR]" "Simplify [last] expression"            cSimplify
      , Cmd "core [EXPR]"     "Generate core for [last] expression"   cCore
      , Cmd "eval [EXPR]"     "Evaluate [last] expression"            cEval
      , Cmd "print [EXPR]"    "Pretty print [last] expression"        cPrint
      ]
  , c_exec = cParseLine
  , c_help = helpMsg
  , c_greet = "Verse parse&desugar testing.\nUse :help for help, and :quit to quit."
  , c_bye = "Bye!"
  , c_prompt = "> "
  , c_state = CState { lastExpr = NoExpr }
  , c_history = Just ".versei"
  }

updateLastExpr :: CState -> SomeExpr -> IO CState
updateLastExpr s e = pure s{ lastExpr = e }

cRead :: Run CState
cRead fn s =
  tryIt (pure s) (updateLastExpr s . Parsed) $ do
    file <- readFile fn
    let prog = parseDie pFile fn file
    when (prog == prog) $
      putStrLn "OK"
    pure prog

cParseLine :: Run CState
cParseLine line s =
  tryIt (pure s) (updateLastExpr s . Parsed) $ do
    let prog = parseDie pFile "<interactive>" line
    pp prog
    pure prog

withLastExpr :: (SomeExpr -> CState -> IO CState) -> Run CState
withLastExpr cmd line s = do
  s' <- if null line then pure s else cParseLine line s
  cmd (lastExpr s') s'

cTransform :: (SomeExpr -> SomeExpr) -> Run CState
cTransform tr =
  withLastExpr $ \ e s ->
    tryIt (pure s) (updateLastExpr s) $ do
      let e' = tr e
      pp e'
      pure e'

cDesugar :: Run CState
cDesugar = cTransform (Desugared . desugar . asExpr)

cSimplify :: Run CState
cSimplify = cTransform (Desugared . simplify . asExpr)

cCore :: Run CState
cCore = cTransform (Cored . exprToCore . asDesugared)

cEval :: Run CState
cEval = cTransform (Cored . eval . asCore)

cShow :: Run CState
cShow =
  withLastExpr $ \ e s -> do
    print e
    pure s

cPrint :: Run CState
cPrint =
  withLastExpr $ \ e s -> do
    pp e
    pure s

helpMsg :: String
helpMsg = "\
\Enter an EXPR to parse or a command.\n\
\Many commands operate on the last printed expression.\n\
\Try\n\
\  > 1+2\n\
\  > :show\n\
\  > :desugar\n\
\  > :show\n\
\Commands (can be abbreviated):\
\"

