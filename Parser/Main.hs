{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception
import Control.Monad

import Epic.Print
import Desugar
import Expr
import Parse
import Command

tryIt :: IO b -> (a -> IO b) -> IO a -> IO b
tryIt iob aiob ioa = do
  e <- try ioa
  case e of
    Left (exn :: SomeException) -> do
      print exn
      iob
    Right a -> aiob a

main :: IO ()
main = runCommand command

data CState = CState
  { lastExpr :: Maybe Expr
  }

command :: Command CState
command = Command
  { c_commands =
      [ Cmd "read FILE"      "Parse a file"                   cRead
      , Cmd "desugar [EXPR]" "Desugar [last] expression"      cDesugar
      , Cmd "show [EXPR]"    "Show [last] expression"         cShow
      , Cmd "print [EXPR]"   "Pretty print [last] expression" cPrint
      ]
  , c_exec = cParseLine
  , c_help = "Enter an EXPR to parse or\na command:"
  , c_greet = "Verse parser testing."
  , c_bye = "Bye!"
  , c_prompt = "> "
  , c_state = CState { lastExpr = Nothing }
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
    Nothing -> do putStrLn "No current expression"; pure s'
    Just e -> cmd e s'

cDesugar :: Run CState
cDesugar =
  withLastExpr $ \ e s ->
    tryIt (pure s) (updateLastExpr s) $ do
      let e' = desugar e
      pp e'
      pure e'

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
