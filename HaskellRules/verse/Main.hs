{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Main(main) where
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Text.Printf
import Text.Read(readMaybe)
import Options.Applicative hiding (command)

import Epic.Print hiding ((<>))
import FrontEnd.Desugar
import FrontEnd.Expr
import FrontEnd.Parse(parseDie, pFile)
import qualified FrontEnd.Parse as P
import VerseRepl.Command
import FrontEnd.Core
import FrontEnd.CoreSimp
import FrontEnd.Eval
--import qualified Parser.Testing as Testing
import FrontEnd.TRSAdapter
import FrontEnd.Run
--import DenSem.DenSem
import Rules.Systems
import Rules.Core(defaultTRSFlags)

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
main = do
  args <- mainArgs
  let cmd =
        case rulesys args of
          Nothing -> command
          Just name -> command{ c_state = (c_state command){ esystem = either error id $ lookupSystem name } }
  runCommand cmd

data MainFlags = MainFlags
  { rulesys  :: !(Maybe String)
  }

mainFlags :: Parser MainFlags
mainFlags = MainFlags
  <$> optional (strOption
         ( long "rules"
        <> short 'r'
        <> metavar "NAME"
        <> help "Use rule system NAME" ))

mainArgs :: IO MainFlags
mainArgs = do
  let prf = prefs disambiguate
  customExecParser prf $ info (mainFlags <**> helper)
             ( fullDesc
            <> progDesc "Verse interactive system"
            <> header "verse - Parse, desugar, and evaluate Verse expressions"
             )

-------------------

data CState = CState
  { lastExpr    :: !SomeExpr
  , lastFile    :: !(Maybe FilePath)
  , definitions :: ![Expr]
  , prelude     :: !(Maybe Expr)
  , flags       :: !Flags
  , esystem     :: !ESystem
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

asCore :: Flags -> SomeExpr -> Core
asCore _ (Cored e) = e
asCore _ (Cores [e]) = e
asCore _ Cores{} = error "Multiple Core values"
asCore s e = exprToCore s $ asDesugared e

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
  pPrintPrec _ _ (Cores []) = text "No results"
  pPrintPrec l p (Cores [e]) = pPrintPrec l p e
  pPrintPrec l _ (Cores es) = vcat $ text "Multiple results:" :
                                     map (\ e -> pPrintPrec l 0 e $$ text "------------") es

command :: Command CState
command = Command
  { c_commands =
      [ Cmd "read FILE"            "Parse a file"                          cRead
      , Cmd "desugar [EXPR]"       "Desugar [last] expression"             cDesugar
      , Cmd "show [EXPR]"          "Show [last] expression"                cShow
      , Cmd "simplify [EXPR]"      "Simplify [last] expression"            cSimplify
      , Cmd "csimplify [EXPR]"     "Simplify [last] core expression"       cCoreSimplify
      , Cmd "core [EXPR]"          "Generate core for [last] expression"   cCore
--      , Cmd "compile [EXPR]"       "Generate core for [last] expression"   cCompile
      , Cmd "print [EXPR]"         "Pretty print [last] expression"        cPrint
      , Cmd "eval [EXPR]"          "Evaluate [last] expression"            cEval
      , Cmd "rewrite [EXPR]"       "Rewrite [last] expression with selected rules"           cRewrite
--      , Cmd "denote [EXPR]"        "Evaluate with (very restricted) denonational semantics"  cDenSem
--      , Cmd "run [EXPR]"           "Eval/rewrite [last] expression"        cRun
--      , Cmd "define [EXPR]"        "Add [last] expression to global defs"  cDefine
--      , Cmd "clear"                "Clear global defs"                     cClear
--      , Cmd "deval [EXPR]"         "Evaluate [last] expression with global defs"  cDefEval
--      , Cmd "display"              "Show current global defs"              cDisplay
--      , Cmd "prelude"              "Load prelude.verse"                    cPrelude
      , Cmd "set"                  "Turn on flag"                          (cSet True)
      , Cmd "unset"                "Turn off flag"                         (cSet False)
      , Cmd "rules [NAME]"         "Select rule system"                    cRules
      , Cmd "parsecore EXPR"       "Enter a Core expression"               cParseCore
      ]
  , c_exec = cParseLine
  , c_help = helpMsg
  , c_greet = "Verse parse, desugar, and evaluation testing.\nUse :help for help, and :quit to quit."
  , c_bye = "Bye!"
  , c_prompt = "> "
  , c_state = CState { lastExpr = NoExpr, lastFile = Nothing, definitions = []
                     , prelude = Nothing, flags = defaultFlags{fSplit=False} , esystem = dummySystem }
  , c_history = Just ".versei"
  }

dummySystem :: ESystem
dummySystem = TRSystem { sname = "none", description = "no rule system selected",
  ruleEnv = defaultTRSFlags,
  preProcess = id, postProcess = id, rules = noRules, rulesHaveStructural = False,
  confluenceRules = noRules, validExpr = const undefined }
  where noRules _ _ = error "No rule system selected"

updateLastExpr :: CState -> SomeExpr -> IO CState
updateLastExpr s e = pure s{ lastExpr = e }

cSet :: Bool -> Run CState
cSet _ "" s = do
  let f (d,(g,_)) = printf "  %-12s %s\n" d $ if g (flags s) then "on" else "off"
  putStr $ concatMap f flagTable
  printf "  %-12s %d\n" "steps" (fRewriteSteps (flags s))
  pure s
cSet True l s | Just l' <- stripPrefix "steps=" l, Just n <- readMaybe l' =
  pure $ s{ flags = (flags s){fRewriteSteps = n} }
cSet b l s =
  case filter (isPrefixOf l . fst) flagTable of
    [] -> do putStrLn "Unknown flag"; pure s
    [(_, (_, set))] -> pure $ s{ flags = set b (flags s) }
    xs -> do putStrLn $ "Ambiguous flag: " ++ show (map fst xs); pure s

flagTable :: [(String, (Flags -> Bool, Bool -> Flags -> Flags))]
flagTable =
  [("rewrite",     (fRewrite,      \ b s -> s{fRewrite=b}))
  ,("simplify",    (fSimplify,     \ b s -> s{fSimplify=b}))
  ,("split",       (fSplit,        \ b s -> s{fSplit=b}))
  ,("trace",       (fTrace,        \ b s -> s{fTrace=b}))
  ,("underLambda", (fUnderLambda,  \ b s -> s{fUnderLambda=b}))
  ,("timLambda",   (fTimLambda,    \ b s -> s{fTimLambda=b}))
--  ,("densem",      (fDenSem,       \ b s -> s{fDenSem=b}))
--  ,("fresh",       (fFresh,        \ b s -> s{fFresh=b}))
  ,("latex",       (fLatex,        \ b s -> s{fLatex=b}))
  ,("dfs",         (fDfs,          \ b s -> s{fDfs=b}))
  ,("finalInline", (fFinalInline,  \ b s -> s{fFinalInline=b}))
--  ,("alias",       (fAlias,        \ b s -> s{fAlias=b}))
--  ,("unify-equal", (fUnifyEq,      \ b s -> s{fUnifyEq=b}))
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

{-
cPrelude :: Run CState
cPrelude fn s =
  tryIt (pure s) (\ e -> pure s{ prelude = Just e }) $ do
    file <- readFile $ if null fn then "prelude.verse" else fn
    let prog = parseDie pFile fn file
    when (prog == prog) $
      putStrLn "OK"
    pure prog
-}

cParseLine :: Run CState
cParseLine line s =
  tryIt (pure s) (updateLastExpr s) $ do
    let prog = parseDie ((Parsed <$> P.try pFile) <|> (Cored <$> pCoreFile)) "<interactive>" line
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
cCoreSimplify c s = cTransform (Cored . simpCore . asCore (flags s)) c s

cCore :: Run CState
cCore c s = cTransform (Cored . asCore (flags s)) c s

{-
cCompile :: Run CState
cCompile c s = cTransform (Cored . compile (flags s)) c s
-}
{-
cRun :: Run CState
cRun c s = cTransform (Cored . run flg' (esystem s) . asCore flg') c s
  where flg = flags s
        flg' = flg -- if fDenSem flg then flg{ fTimLambda = True, fSplit = False } else flg
-}
cEval :: Run CState
cEval c s =
  cTransform (Cored . eval flg . compile (flags s)) c s
  where flg = EFlags { underLambda = fUnderLambda (flags s), traceEval = fTrace (flags s), steps = fEvalSteps (flags s) }

cParseCore :: Run CState
cParseCore line s =
  tryIt (pure s) (updateLastExpr s . Cored) $ do
    let prog = parseDie pCoreFile "<interactive>" line
    pp prog
    pure prog

cRules :: Run CState
cRules "" s = do putStrLn $ "rules: " ++ sname (esystem s) ++ " - " ++ description (esystem s); pure s
cRules line s =
  case lookupSystem line of
    Left msg -> do putStrLn msg; pure s
    Right e -> do putStrLn $ "Selected: " ++ description e; pure s{ esystem = e }

{-
cDefEval :: Run CState
cDefEval c s = do
  let addDefs e = Seq $ maybeToList (prelude s) ++ definitions s ++ [e]
      flg = EFlags { underLambda = fUnderLambda (flags s), traceEval = fTrace (flags s), steps = fEvalSteps (flags s) }
  cTransform (Cored . eval flg . simpCore . asCore (flags s) . Parsed . addDefs . asExpr) c s
-}

cRewrite :: Run CState
cRewrite c s =
  cTransform (Cores . rewrite flg sys . compile flg) c s
  where flg = flags s
        sys = esystem s

{-
cDenSem :: Run CState
cDenSem c s =
  cTransform (Cored . denSem . compile flgs) c s
  where flgs = (flags s){ fSplit = False, fSimplify = True, fTimLambda = True }
-}

compile :: Flags -> SomeExpr -> Core
compile s = (if fSimplify s then simpCore else id) . replacePrelude . (if fSimplify s then simpCore else id) . asCore s

{-
cDefine :: Run CState
cDefine =
  withLastExpr $ \ e s -> do
    let !e' = asExpr e
    pure  s{ definitions = definitions s ++ [e'] }
-}
{-
cClear :: Run CState
cClear _ s = pure s{ definitions = [] }
-}
{-
cDisplay :: Run CState
cDisplay _ s = do
  case prelude s of
    Nothing -> pure ()
    Just e -> do putStrLn "prelude:"; pp e
  putStrLn "definitions:"
  mapM_ pp $ definitions s
  pure s
-}

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
\\n\
\Available rule systems:\n\
\" ++
  concat [printf "%-10s - %s\n" (sname e) (description e) | e <- allSystems ]
  ++ "\n\
\Commands (can be abbreviated):\
\"
