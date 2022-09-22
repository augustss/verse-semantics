{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Main(main, test) where
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Text.Printf

import Print
import Desugar
import Expr
import Parse
import Command
import Core
import CoreSimp
import Eval
import qualified Testing
import TRSAdapter

tryIt :: IO b -> (a -> IO b) -> IO a -> IO b
tryIt iob aiob ioa = do
  e <- try ioa
  case e of
    Left (exn :: SomeException) -> do
      print exn
      iob
    Right a -> aiob a

-------------------

test :: IO ()
test = Testing.main

main :: IO ()
main = runCommand command

data CState = CState
  { lastExpr    :: !SomeExpr
  , lastFile    :: !(Maybe FilePath)
  , tracing     :: !Bool
  , definitions :: ![Expr]
  , prelude     :: !(Maybe Expr)
  , useRewrite  :: !Bool
  , useSplit    :: !Bool
  , useSimp     :: !Bool
  }

data SomeExpr = NoExpr | Parsed Expr | Desugared Expr | Cored Core | Cores [Core]

asExpr :: SomeExpr -> Expr
asExpr NoExpr = error "No current expression"
asExpr (Parsed e) = e
asExpr (Desugared e) = e
asExpr (Cored _) = error "Current expression is Core"
asExpr (Cores _) = error "Current expression is [Core]"

asDesugared :: SomeExpr -> Expr
asDesugared (Parsed e) = desugar e
asDesugared e = asExpr e

asCore :: CState -> SomeExpr -> Core
asCore _ (Cored e) = e
asCore _ (Cores [e]) = e
asCore _ Cores{} = error "Multiple Core values"
asCore s e = exprToCore (useSplit s) $ asDesugared e

instance Show SomeExpr where
  show NoExpr = "No current expression"
  show (Parsed e) = show e
  show (Desugared e) = show e
  show (Cored e) = show e
  show (Cores e) = show e

instance Pretty SomeExpr where
  pPrintPrec _ _ NoExpr = text "No current expression"
  pPrintPrec l p (Parsed e) = pPrintPrec l p e
  pPrintPrec l p (Desugared e) = pPrintPrec l p e
  pPrintPrec l p (Cored e) = pPrintPrec l p e
  pPrintPrec _ _ (Cores []) = text "No reduction results !?!"
  pPrintPrec l p (Cores [e]) = pPrintPrec l p e
  pPrintPrec l _ (Cores es) = vcat $ text "Multiple results:" :
                                     map (pPrintPrec l 0) es

command :: Command CState
command = Command
  { c_commands =
      [ Cmd "read FILE"            "Parse a file"                          cRead
      , Cmd "desugar [EXPR]"       "Desugar [last] expression"             cDesugar
      , Cmd "show [EXPR]"          "Show [last] expression"                cShow
      , Cmd "simplify [EXPR]"      "Simplify [last] expression"            cSimplify
      , Cmd "csimplify [EXPR]"     "Simplify [last] core expression"       cCoreSimplify
      , Cmd "core [EXPR]"          "Generate core for [last] expression"   cCore
      , Cmd "compile [EXPR]"       "Generate core for [last] expression"   cCompile
      , Cmd "eval [EXPR]"          "Evaluate [last] expression"            cEval
      , Cmd "print [EXPR]"         "Pretty print [last] expression"        cPrint
      , Cmd "trace"                "Turn on evaluation tracing"            (cTrace True)
      , Cmd "notrace"              "Turn off evaluation tracing"           (cTrace False)
      , Cmd "rewrite"              "Rewrite [last] expression with accurate rules"           cRewrite
      , Cmd "define [EXPR]"        "Add [last] expression to global defs"  cDefine
      , Cmd "clear"                "Clear global defs"                     cClear
      , Cmd "deval [EXPR]"         "Evaluate [last] expression with global defs"  cDefEval
      , Cmd "display"              "Show current global defs"              cDisplay
      , Cmd "prelude"              "Load prelude.verse"                    cPrelude
      , Cmd "set"                  "Turn on flag"                          (cSet True)
      , Cmd "unset"                "Turn off flag"                         (cSet False)
      ]
  , c_exec = cParseLine
  , c_help = helpMsg
  , c_greet = "Verse parse, desugar, and evaluation testing.\nUse :help for help, and :quit to quit."
  , c_bye = "Bye!"
  , c_prompt = "> "
  , c_state = CState { lastExpr = NoExpr, lastFile = Nothing, tracing = False
                     , definitions = [], prelude = Nothing, useRewrite = False, useSplit = True, useSimp = False }
  , c_history = Just ".versei"
  }

updateLastExpr :: CState -> SomeExpr -> IO CState
updateLastExpr s e = pure s{ lastExpr = e }

cTrace :: Bool -> Run CState
cTrace b _ s = pure s{ tracing = b }

cSet :: Bool -> Run CState
cSet _ "" s = do
  let f (d,(g,_)) = printf "  %-10s %s\n" d $ if g s then "on" else "off"
  putStr $ concatMap f flags
  pure s
cSet b l s =
  case find (isPrefixOf l . fst) flags of
    Nothing -> do putStrLn "Unknown flag"; pure s
    Just (_, (_, set)) -> pure $ set b s

flags :: [(String, (CState -> Bool, Bool -> CState -> CState))]
flags =
  [("tracing",  (tracing,    \ b s -> s{tracing=b}))
  ,("rewrite",  (useRewrite, \ b s -> s{useRewrite=b}))
  ,("split",    (useSplit,   \ b s -> s{useSplit=b}))
  ,("simplify", (useSimp,    \ b s -> s{useSimp=b}))
  ]

cRead :: Run CState
cRead afn s = do
  let fn = if afn == "" then fromMaybe (error "No previous file name") (lastFile s) else afn
      s' = s{lastFile = Just fn}
  tryIt (pure s') (updateLastExpr s' . Parsed) $ do
    file <- readFile fn
    let prog = parseDie pFile fn file
    when (prog == prog) $
      putStrLn "OK"
    pure prog

cPrelude :: Run CState
cPrelude fn s =
  tryIt (pure s) (\ e -> pure s{ prelude = Just e }) $ do
    file <- readFile $ if null fn then "prelude.verse" else fn
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

cCoreSimplify :: Run CState
cCoreSimplify c s = cTransform (Cored . simpCore . asCore s) c s

cCore :: Run CState
cCore c s = cTransform (Cored . asCore s) c s

cCompile :: Run CState
cCompile c s = cTransform (Cored . compile s) c s

cEval :: Run CState
cEval c s =
  cTransform (Cored . eval flg . compile s) c s
  where flg = Flags { underLambda = False, traceEval = tracing s }

cDefEval :: Run CState
cDefEval c s = do
  let addDefs e = Seq $ maybeToList (prelude s) ++ definitions s ++ [e]
      flg = Flags { underLambda = False, traceEval = tracing s }
  cTransform (Cored . eval flg . simpCore . asCore s . Parsed . addDefs . asExpr) c s

cRewrite :: Run CState
cRewrite c s =
  cTransform (Cores . rewrite 1000 . compile s) c s

compile :: CState -> SomeExpr -> Core
compile s = (if useSimp s then simpCore else id) . replacePrelude . asCore s

cDefine :: Run CState
cDefine =
  withLastExpr $ \ e s -> do
    let !e' = asExpr e
    pure  s{ definitions = definitions s ++ [e'] }

cClear :: Run CState
cClear _ s = pure s{ definitions = [] }

cDisplay :: Run CState
cDisplay _ s = do
  case prelude s of
    Nothing -> pure ()
    Just e -> do putStrLn "prelude:"; pp e
  putStrLn "definitions:"
  mapM_ pp $ definitions s
  pure s

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
\  > :eval\n\
\Commands (can be abbreviated):\
\"
