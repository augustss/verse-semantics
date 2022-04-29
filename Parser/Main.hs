{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception
import Control.Monad

import Print
import Desugar
import Expr
import Parse
import Command
import Core

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
  { lastExpr :: Maybe Expr
  }

command :: Command CState
command = Command
  { c_commands =
      [ Cmd "read FILE"       "Parse a file"                   cRead
      , Cmd "desugar [EXPR]"  "Desugar [last] expression = :li + :fu + :un + :sc"
                                                               cDesugar
      , Cmd "light [EXPR]"    "Initial desugaring"             cDesugarLight
      , Cmd "function [EXPR]" "Function desugaring"            cFunction
      , Cmd "scope [EXPR]"    "Insert defs"                    cScope
      , Cmd "show [EXPR]"     "Show [last] expression"         cShow
      , Cmd "simplify [EXPR]" "Show [last] expression"         cSimplify
      , Cmd "uniq [EXPR]"     "Make identifiers unique"        cUniq
      , Cmd "print [EXPR]"    "Pretty print [last] expression" cPrint
      ]
  , c_exec = cParseLine
  , c_help = helpMsg
  , c_greet = "Verse parse&desugar testing.\nUse :help for help, and :quit to quit."
  , c_bye = "Bye!"
  , c_prompt = "> "
  , c_state = CState { lastExpr = Nothing }
  , c_history = Just ".versei"
  }

updateLastExpr :: CState -> Expr -> IO CState
updateLastExpr s e = pure s{ lastExpr = Just e }

cRead :: Run CState
cRead fn s =
  tryIt (pure s) (updateLastExpr s) $ do
    file <- readFile fn
    let prog = parseDie pFile fn file
    when (prog == prog) $
      putStrLn "OK"
    pure prog

cParseLine :: Run CState
cParseLine line s =
  tryIt (pure s) (updateLastExpr s) $ do
    let prog = parseDie pFile "<interactive>" line
    pp prog
    pure prog

withLastExpr :: (Expr -> CState -> IO CState) -> Run CState
withLastExpr cmd line s = do
  s' <- if null line then pure s else cParseLine line s
  case lastExpr s' of
    Nothing -> do putStrLn "No last expression"; pure s'
    Just e -> cmd e s'

cTransform :: (Expr -> Expr) -> Run CState
cTransform tr =
  withLastExpr $ \ e s ->
    tryIt (pure s) (updateLastExpr s) $ do
      let e' = tr e
      pp e'
      pure e'

cDesugar :: Run CState
cDesugar = cTransform desugar

cDesugarLight :: Run CState
cDesugarLight = cTransform desugarLight

cScope :: Run CState
cScope = cTransform scope

cFunction :: Run CState
cFunction = cTransform desugarFunction

cSimplify :: Run CState
cSimplify = cTransform simplify

cUniq :: Run CState
cUniq = cTransform makeUniq

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

