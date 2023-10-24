{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Main(main) where
import Control.Exception(SomeException, try)
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
import FrontEnd.Flags
--import qualified Parser.Testing as Testing
import FrontEnd.ParseCore
import FrontEnd.Prelude
import FrontEnd.Run(run, findSystem, blockSystem, everySystem, adjustFlags)
import FrontEnd.TRSAdapter(coreToTrs, trsToCore)
--import DenSem.DenSem
import Rules.Systems(ESystem, TRSystem(..))
--import Rules.Core(defaultTRSFlags)
--import Verifier.Verify
import Rules.Verifier
import TRS.Traced(toList, showTrace)
-- import TRS.Bind(free)

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
  let msys = fmap (either error id . findSystem) (rulesys args)
  let cs1 = c_state command
  let cs2 =
        case msys of
          Nothing -> cs1
          Just sys -> cs1{ esystem = sys }
  cs3 <- setPrelude (preludeName args) cs2
  let cs4 = cs3{ flags = adjustFlags (esystem cs3) flg }
      flg = (flags cs3){ fSimplify = simplify args }
  let comm = command{ c_nl = wslbug args, c_state = cs4 }
  if null (fileNames args) then do
    runCommand comm
   else
    mapM_ (runFile (flags cs4) (esystem cs4) (dDesugar args)) (fileNames args)

data MainFlags = MainFlags
  { rulesys     :: !(Maybe String)
  , wslbug      :: !Bool
  , preludeName :: !String
  , dDesugar    :: !Bool
  , simplify    :: !Bool
  , fileNames   :: ![FilePath]          -- input files
  }

mainFlags :: Parser MainFlags
mainFlags = MainFlags
  <$> optional (strOption
         ( long "rules"
        <> short 'r'
        <> metavar "NAME"
        <> help "Use rule system NAME" ))
  <*> switch
         ( long "wsl"
        <> help "Add extra NL to compensate for WSL bug" )
  <*> strOption
         ( long "prelude"
        <> short 'p'
        <> metavar "NAME"
        <> value "miniprelude"
        <> help "Use built in prelude NAME" )
  <*> switch
         ( long "ddesugar"
        <> help "Debug - show desugared" )
  <*> switch
         ( long "simplify"
        <> help "simplify core" )
  <*> many (argument str (metavar "FILES..."))

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
  , flags       :: !Flags
  , esystem     :: !ESystem
  }


data SomeExpr = NoExpr | Parsed Expr | Desugared Expr | Cores [Core]

asParsed :: SomeExpr -> Expr
asParsed NoExpr = error "No current expression"
asParsed (Parsed e) = e
asParsed (Desugared _) = error "Current expression is desugared"
asParsed (Cores _) = error "Current expression is [Core]"

asExpr :: SomeExpr -> Expr
asExpr (Desugared e) = e
asExpr e = asParsed e

asDesugared :: Flags -> SomeExpr -> Expr
asDesugared f (Parsed e) = desugar f e
asDesugared _ e = asExpr e

asCore :: Flags -> SomeExpr -> Core
asCore _ (Cores [e]) = e
asCore _ Cores{} = error "Not a singleton Core value"
asCore s e = asDesugared s e

instance Show SomeExpr where
  show NoExpr = "No current expression"
  show (Parsed e) = show e
  show (Desugared e) = show e
  show (Cores e) = show e

instance Pretty SomeExpr where
  pPrintPrec _ _ NoExpr = text "No current expression"
  pPrintPrec l p (Parsed e) = pPrintPrec l p e
  pPrintPrec l p (Desugared e) = pPrintPrec l p e
  pPrintPrec _ _ (Cores []) = text "No results"
  pPrintPrec l p (Cores [e]) = pPrintPrec l p e
  pPrintPrec l _ (Cores es) = vcat $ text "Multiple results:" :
                                     map (\ e -> pPrintPrec l 0 e $$ text "------------") es

command :: Command CState
command = Command
  { c_commands =
      [ Cmd "read FILE"            "Parse a file"                          cRead
      , Cmd "desugar [EXPR]"       "Desugar [last] expression"             cDesugar
      , Cmd "vdesugar [EXPR]"      "Desugar (for verification) [last] expression"             cDesugarVerify
      , Cmd "show [EXPR]"          "Show [last] expression"                cShow
      , Cmd "print [EXPR]"         "Pretty print [last] expression"        cPrint
      , Cmd "eval [EXPR]"          "Evaluate [last] expression"            cEval
--      , Cmd "define [EXPR]"        "Add [last] expression to global defs"  cDefine
--      , Cmd "clear"                "Clear global defs"                     cClear
--      , Cmd "deval [EXPR]"         "Evaluate [last] expression with global defs"  cDefEval
--      , Cmd "display"              "Show current global defs"              cDisplay
      , Cmd "preprocess"           "Preprocess for rule set"                 cPreprocess
      , Cmd "set"                  "Turn on flag"                          (cSet True)
      , Cmd "unset"                "Turn off flag"                         (cSet False)
      , Cmd "rules [NAME]"         "Select rule system"                    cRules
      , Cmd "prelude [NAME]"       "Select prelude"                        cPrelude
      , Cmd "verify [EXPR]"        "Verify [last] expression"              cVerify
      , Cmd "pcore EXPR"           "parse core expression"                 cPcore
      ]
  , c_exec = cParseLine
  , c_help = helpMsg
  , c_greet = "Verse parse, desugar, and evaluation testing.\nUse :help for help, and :quit to quit."
  , c_bye = "Bye!"
  , c_prompt = "> "
  , c_state = CState { lastExpr = NoExpr, lastFile = Nothing, definitions = []
                     , flags = defaultFlags{fSplit=True, fNoFuelStop=True, fSimplify=True}
                     , esystem = blockSystem
                     }
  , c_history = Just ".versei"
  , c_nl = False
  }

updateLastExpr :: CState -> SomeExpr -> IO CState
updateLastExpr s e = pure s{ lastExpr = e }

cSet :: Bool -> Run CState
cSet _ "" s = do
  let f (d,(g,_)) = printf "  %-12s %s\n" d $ if g (flags s) then "on" else "off"
  putStr $ concatMap f flagTable
  printf "  %-12s %d\n" "steps" (fRewriteSteps (flags s))
  printf "  %-12s %s\n" "desugar" (show (fDesugar (flags s)))
  pure s
cSet True l s | Just l' <- stripPrefix "steps=" l, Just n <- readMaybe l' =
  pure $ s{ flags = (flags s){fRewriteSteps = n} }
cSet True l s | Just l' <- stripPrefix "desugar=" l, Just d <- readMaybe l' =
  pure $ s{ flags = (flags s){fDesugar = d} }
cSet b l s =
  case filter (isPrefixOf l . fst) flagTable of
    [] -> do putStrLn "Unknown flag"; pure s
    [(_, (_, set))] -> pure $ s{ flags = set b (flags s) }
    xs -> do putStrLn $ "Ambiguous flag: " ++ show (map fst xs); pure s

flagTable :: [(String, (Flags -> Bool, Bool -> Flags -> Flags))]
flagTable =
  [("simplify",    (fSimplify,     \ b s -> s{fSimplify=b}))
  ,("split",       (fSplit,        \ b s -> s{fSplit=b}))
  ,("trace",       (fTrace,        \ b s -> s{fTrace=b}))
  ,("underLambda", (fUnderLambda,  \ b s -> s{fUnderLambda=b}))
--  ,("densem",      (fDenSem,       \ b s -> s{fDenSem=b}))
  ,("latex",       (fLatex,        \ b s -> s{fLatex=b}))
  ,("dfs",         (fDfs,          \ b s -> s{fDfs=b}))
  ,("finalInline", (fFinalInline,  \ b s -> s{fFinalInline=b}))
  ,("desugartrace",(fTraceDesugar, \ b s -> s{fTraceDesugar=b}))
  ,("verifytrace", (fTraceVerify,  \ b s -> s{fTraceVerify=b}))
  ,("assumeVerified", (fAssumeVerified, \ b s -> s{fAssumeVerified=b}))
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

cParseLine :: Run CState
cParseLine line s =
  tryIt (pure s) (updateLastExpr s) $ do
    let prog = parseDie ((Parsed <$> P.try pFile) <|> (Desugared <$> pCoreFile)) "<interactive>" line
    pp prog
    pure prog

cPcore :: Run CState
cPcore line s =
  tryIt (pure s) (updateLastExpr s) $ do
    let prog = parseDie (Desugared <$> pCoreFile) "<interactive>" line
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
cDesugar c s = do
  let flg = flags s
  putStrLn $ "Desugar for execution: rules=" ++ show (fDesugar flg) ++ ", prelude=" ++ fst (fPrelude flg)
  cTransform (Desugared . desugar flg . asExpr) c s

isVerifyPrelude :: (String, Expr) -> Bool
isVerifyPrelude (pn, _) = "verify" `isPrefixOf` pn

cDesugarVerify :: Run CState
cDesugarVerify c s = do
  let aflg = flags s
      -- This is a hack to avoid the default prelude for verification.
      prel = if isVerifyPrelude (fPrelude aflg) then fPrelude aflg else either error id $ findPrelude "verifyprelude"
      flg = aflg{ fVerify = True, fSplit = False, fAssumeVerified = False, fPrelude = prel }
  putStrLn $ "Desugar for verification: rules=" ++ show (fDesugar flg) ++ ", prelude=" ++ fst (fPrelude flg)
  cTransform (Desugared . desugar flg . asExpr) c s

cPreprocess :: Run CState
cPreprocess c s = cTransform (Desugared . pre . asCore (flags s)) c s
  where pre = trsToCore . preProcess sys (ruleEnv sys) . coreToTrs
        sys = esystem s

cEval :: Run CState
cEval c s = cTransform (Desugared . run flg (esystem s) . asCore flg) c s
  where flg = flags s

cVerify :: Run CState
cVerify = do
  withLastExpr $ \ e s ->
    tryIt (pure s) (\ _ -> pure s) $ do
      let sys = icfpeVerifier
          aflg = flags s
          flg = aflg{ fVerify = True, fSplit = False, fAssumeVerified = False, fPrelude = prel }
          prel = if isVerifyPrelude (fPrelude aflg) then fPrelude aflg else either error id $ findPrelude "verifyprelude"
          e1  = asCore flg e
          e2  = coreToTrs e1
          e' = (if True then wrapAssert else id) $ preProcess sys (ruleEnv sys) e2
      putStrLn $ "Verify: rules=" ++ show (fDesugar flg) ++ ", prelude=" ++ fst (fPrelude flg)
      putStrLn $ "Desugared:\n" ++ prettyShow e'
      let (done, trc) = verify sys e'
      when (fTraceVerify flg) $ do
        putStrLn "Verification trace:"
        putStrLn $ unlines $ showTrace trc
      if done then
        putStrLn "Verified"
       else do
        putStrLn "Not verified, residual term:"
        pp $ snd $ head $ toList trc
      pure ()

systemDescr :: ESystem -> String
systemDescr s = sname s ++ ": " ++ description s


cRules :: Run CState
cRules "" s = do putStrLn ("rules: " ++ systemDescr (esystem s)); pure s
cRules line s =
  case findSystem line of
    Left msg -> do putStrLn msg; pure s
    Right sys -> do
      putStrLn $ "Selected=" ++ systemDescr sys
      pure s{ esystem = sys,
              flags = adjustFlags sys (flags s) }

cPrelude :: Run CState
cPrelude "" s = do putStrLn $ "current prelude: " ++ fst (fPrelude (flags s)); pure s
cPrelude line s = setPrelude line s

setPrelude :: String -> CState -> IO CState
setPrelude pn cs =
  case findPrelude pn of
    Left msg -> error $ "prelude failed " ++ msg
    Right prel -> pure cs{ flags = (flags cs){ fPrelude = prel } }


{-
cDefEval :: Run CState
cDefEval c s = do
  let addDefs e = Seq $ maybeToList (prelude s) ++ definitions s ++ [e]
      flg = EFlags { underLambda = fUnderLambda (flags s), traceEval = fTrace (flags s), steps = fEvalSteps (flags s) }
  cTransform (Cored . eval flg . simpCore . asCore (flags s) . Parsed . addDefs . asExpr) c s

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
  concat [printf "%-10s - %s\n" (sname e) (description e) | e <- everySystem ]
  ++ "\n\
\Commands (can be abbreviated):\
\"

--------------------------------------------------

runFile :: Flags -> ESystem -> Bool -> FilePath -> IO ()
runFile flg sys ddesugar fn = do
  putStrLn $ "running " ++ fn
  file <- readFile fn
  let e = parseDie pFile fn file
      d = desugar flg e
  when ddesugar $
      putStrLn $ "Desugared:\n" ++ prettyShow d
  let r = run flg sys d
  putStrLn $ prettyShow r
  putStrLn "done"
