{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Main(main) where

import Prelude

import Epic.Repl

import FrontEnd.Flags( Flags(..), defaultFlags )
import FrontEnd.Expr
import FrontEnd.Parse(parseDie, pFile)
import FrontEnd.Prelude( PreludeName, findPrelude )
import FrontEnd.Error

import Epic.Print hiding( (<>) )   -- In this module (<>) is Prelude.<>

{-
import FrontEnd.Desugar
import FrontEnd.ParseCore
import FrontEnd.Run(run, findSystem, blockSystem, everySystem, adjustFlags)
import FrontEnd.TRSAdapter(coreToTrs, trsToCore)

import Rules.Systems(ESystem, TRSystem(..))
import Rules.Verifier
import TRS.Traced(toList, filterTrace, showTrace)

--import qualified Parser.Testing as Testing
--import DenSem.DenSem
--import Rules.Core(defaultTRSFlags)
--import Verifier.Verify
--import TRS.Bind(free)

-- Epic Library utilities
import Epic.Print hiding ((<>))
import Epic.Uniplate
-}

-- General library utilities
import Control.Exception(SomeException, try)
import Control.Monad
import Data.List
import Data.Maybe
import Text.Printf
import Text.Read(readMaybe)
import Options.Applicative




--------------------------------------------------------
--
--            The main program
--
--------------------------------------------------------

main :: IO ()
main = do
  main_flags :: MainFlags <- mainArgs
  let cs1 = c_state theCommandSet

--  let msys = fmap (either error id . findSystem) (rulesys flags)
--  let cs2 = case msys of
--              Nothing -> cs1
--              Just sys -> cs1{ esystem = sys }
--      cs3 = cs2{ flags = adjustFlags msys (flags cs2) }

  cs3 <- setPrelude (mf_preludeName main_flags) cs1
  let cs4 = cs3 { cs_flags = (cs_flags cs3){ fSimplify = mf_simplify main_flags } }

  let comm = theCommandSet { c_nl = mf_wslbug main_flags, c_state = cs4 }

  if null (mf_fileNames main_flags) then do
    -- Run interactively
    runCommands comm
   else
    -- Run batch on the input files
    mapM_ (runFile (cs_flags cs4) (cs_esystem cs4)
                   (mf_desugar main_flags)) (mf_fileNames main_flags)

runFile :: Flags -> ESystem -> Bool -> FilePath -> IO ()
-- Apply the REPL to one file
runFile _flg _sys _ddesugar fn = do
  putStrLn $ "running " ++ fn
  putStrLn $ "omitted"   -- ToDo!
{-
  file <- readFile fn
  let e = parseDie pFile fn file
      d = desugar flg e
  when ddesugar $
      putStrLn $ "Desugared:\n" ++ prettyShow d
  let r = run flg sys d
  putStrLn $ prettyShow r
  putStrLn "done"
-}

--------------------------------------------------------
--
--            MainFlags: command-line arguments
--
--------------------------------------------------------

data MainFlags = MainFlags
  { mf_rulesys     :: !(Maybe String)
  , mf_wslbug      :: !Bool
  , mf_preludeName :: !String
  , mf_desugar     :: !Bool
  , mf_simplify    :: !Bool
  , mf_fileNames   :: ![FilePath]          -- input files
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

--------------------------------------------------------
--
--         CState: state of the REPL
--
--------------------------------------------------------

data CState = CState
  { cs_lastExpr    :: !SomeExpr
  , cs_lastFile    :: !(Maybe FilePath)
  , cs_definitions :: ![SrcExpr]
  , cs_flags       :: !Flags
  , cs_esystem     :: !ESystem
  }

data ESystem = ESystemPlaceHolder  -- Just for now

data SomeExpr = NoExpr
              | Parsed    SrcExpr
              | Desugared SrcExpr
              | Cores     [SrcCore]

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

asParsed :: SomeExpr -> SrcExpr
asParsed NoExpr = error "No current expression"
asParsed (Parsed e) = e
asParsed (Desugared _) = error "Current expression is desugared"
asParsed (Cores _) = error "Current expression is [Core]"

asExpr :: SomeExpr -> SrcExpr
asExpr (Desugared e) = e
asExpr e = asParsed e

{-
asDesugared :: Flags -> SomeExpr -> SrcExpr
asDesugared f (Parsed e) = desugar f e
asDesugared _ e = asExpr e

asCore :: Flags -> SomeExpr -> SrcCore
asCore _ (Cores [e]) = e
asCore _ Cores{} = error "Not a singleton Core value"
asCore s e = asDesugared s e
-}

updateLastExpr :: CState -> SomeExpr -> IO CState
updateLastExpr s e = pure s{ cs_lastExpr = e }

withLastExpr :: (SomeExpr -> CState -> IO CState) -> CmdRunner CState
withLastExpr cmd _line s = do
--   s' <- if null line then pure s else cParseLine line s
  let s' = s   -- Ignore input line for now
  cmd (cs_lastExpr s') s'

cTransform :: (SomeExpr -> SomeExpr) -> CmdRunner CState
cTransform tr =
  withLastExpr $ \ e s ->
    tryIt (pure s) (updateLastExpr s) $ do
      let e' = tr e
      pp e'
      pure e'


--------------------------------------------------------
--
--         The commands!
--
--------------------------------------------------------

theCommandSet :: CommandSet CState
theCommandSet = CommandSet
  -- NB: the REPL adds :quit and :help
  { c_commands =
      [ Cmd "read FILE"            "Parse a file"                          cRead
      , Cmd "show [EXPR]"          "Show [last] expression"                cShow
      , Cmd "print [EXPR]"         "Pretty print [last] expression"        cPrint
      , Cmd "set"                  "Turn on flag"                          (cSet True)
      , Cmd "unset"                "Turn off flag"                         (cSet False)
      , Cmd "prelude [NAME]"       "Select prelude"                        cPrelude

--      , Cmd "pcore EXPR"           "parse core expression"                 cPcore
--      , Cmd "desugar [EXPR]"       "Desugar [last] expression"             cDesugar
--      , Cmd "pdesugar [EXPR]"      "Desugar [last] expression pretty"      cPDesugar
--      , Cmd "vdesugar [EXPR]"      "Desugar (for verification) [last] expression"             cDesugarVerify
--      , Cmd "pvdesugar [EXPR]"     "Desugar (for verification) [last] expression pretty"      cPDesugarVerify
--      , Cmd "eval [EXPR]"          "Evaluate [last] expression"            cEval
--      , Cmd "define [EXPR]"        "Add [last] expression to global defs"  cDefine
--      , Cmd "clear"                "Clear global defs"                     cClear
--      , Cmd "deval [EXPR]"         "Evaluate [last] expression with global defs"  cDefEval
--      , Cmd "display"              "Show current global defs"              cDisplay
--      , Cmd "preprocess"           "Preprocess for rule set"                 cPreprocess
--      , Cmd "rules [NAME]"         "Select rule system"                    cRules
--      , Cmd "verify [EXPR]"        "Verify [last] expression"              cVerify
      ]
--  , c_exec = cParseLine
  , c_exec = errorMessage "c_exec: not done yet"

  , c_help   = helpMsg
  , c_greet  = "Verse parse, desugar, and evaluation testing.\nUse :help for help, and :quit to quit."
  , c_bye    = "Bye!"
  , c_prompt = "> "
  , c_state  = CState { cs_lastExpr = NoExpr
                      , cs_lastFile = Nothing
                      , cs_definitions = []
                      , cs_flags = defaultFlags{fSplit=True, fNoFuelStop=True, fSimplify=True}
                      , cs_esystem = ESystemPlaceHolder }
  , c_history = Just ".versei"
  , c_nl = False
  }

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
\Commands (can be abbreviated):\
\"

{-
 \Available rule systems:\n\
 \" ++ concat [printf "%-10s - %s\n" (sname e) (description e) | e <- everySystem ]
  ++ "\n\
-}

--------------------------------------------------------
--
--         Setting flags
--
--------------------------------------------------------

cSet :: Bool -> CmdRunner CState
cSet _ "" s = do
  let f (d,(g,_)) = printf "  %-12s %s\n" d $ if g (cs_flags s) then "on" else "off"
  putStr $ concatMap f flagTable
  printf "  %-12s %d\n" "steps" (fRewriteSteps (cs_flags s))
  printf "  %-12s %s\n" "desugar" (show (fDesugar (cs_flags s)))
  pure s
cSet True l s | Just l' <- stripPrefix "steps=" l, Just n <- readMaybe l' =
  pure $ s{ cs_flags = (cs_flags s){fRewriteSteps = n} }
cSet True l s | Just l' <- stripPrefix "desugar=" l, Just d <- readMaybe l' =
  pure $ s{ cs_flags = (cs_flags s){fDesugar = d} }
cSet b l s =
  case filter (isPrefixOf l . fst) flagTable of
    [] -> do putStrLn "Unknown flag"; pure s
    [(_, (_, set))] -> pure $ s{ cs_flags = set b (cs_flags s) }
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
  ,("postProcess", (fPostProcess,  \ b s -> s{fPostProcess=b}))
  ,("desugartrace",(fTraceDesugar, \ b s -> s{fTraceDesugar=b}))
  ,("verifytrace", (fTraceVerify,  \ b s -> s{fTraceVerify=b}))
  ,("assumeVerified", (fAssumeVerified, \ b s -> s{fAssumeVerified=b}))
  ]

--------------------------------------------------------
--
--         Parsing
--
--------------------------------------------------------

cRead :: CmdRunner CState
cRead afn s = do
  let fn | afn == "" = fromMaybe (error "No previous file name") (cs_lastFile s)
         | otherwise = afn
      s' = s{ cs_lastFile = Just fn }
  tryIt (pure s') (updateLastExpr s' . Parsed) $ do
    file <- readFile fn
    let prog = parseDie pFile fn file
    when (prog == prog) $
      putStrLn "OK"
    pure prog



--------------------------------------------------------
--
--         Desugaring
--
--------------------------------------------------------

{-
cParseLine :: CmdRunner CState
cParseLine line s =
  tryIt (pure s) (updateLastExpr s) $ do
    let prog = parseDie ((Parsed <$> P.try pFile) <|> (Desugared <$> pCoreFile)) "<interactive>" line
    pp prog
    pure prog

cPcore :: CmdRunner CState
cPcore line s =
  tryIt (pure s) (updateLastExpr s) $ do
    let prog = parseDie (Desugared <$> pCoreFile) "<interactive>" line
    pp prog
    pure prog

cDesugar :: CmdRunner CState
cDesugar c s = do
  let flg = flags s
  putStrLn $ "Desugar for execution: rules=" ++ show (fDesugar flg) ++ ", prelude=" ++ fst (fPrelude flg)
  cTransform (Desugared . desugar flg . asExpr) c s

cPDesugar :: CmdRunner CState
cPDesugar c s = do
  let flg = (flags s){ fSimplify = True, fSplit = False, fAssumeVerified = True, fKeepIf = True,
                       fPrelude = either error id $ findPrelude "miniprelude" }
  putStrLn $ "Desugar for execution, prettyfied: rules=" ++ show (fDesugar flg) ++ ", prelude=" ++ fst (fPrelude flg)
  cTransform (Desugared . dropDollar . desugar flg . asExpr) c s

cDesugarVerify :: CmdRunner CState
cDesugarVerify c s = do
  let aflg = flags s
      -- This is a hack to avoid the default prelude for verification.
      prel = if isVerifyPrelude (fPrelude aflg) then fPrelude aflg else either error id $ findPrelude "verifyprelude"
      flg = aflg{ fVerify = True, fSplit = False, fAssumeVerified = False, fPrelude = prel }
  putStrLn $ "Desugar for verification: rules=" ++ show (fDesugar flg) ++ ", prelude=" ++ fst (fPrelude flg)
  cTransform (Desugared . desugar flg . asExpr) c s

cPDesugarVerify :: CmdRunner CState
cPDesugarVerify c s = do
  let aflg = flags s
      -- This is a hack to avoid the default prelude for verification.
      prel = if isVerifyPrelude (fPrelude aflg) then fPrelude aflg else either error id $ findPrelude "verifyprelude"
      flg = aflg{ fVerify = True, fSplit = False, fAssumeVerified = False, fPrelude = prel, fSimplify = True }
  putStrLn $ "Desugar for verification: rules=" ++ show (fDesugar flg) ++ ", prelude=" ++ fst (fPrelude flg)
  cTransform (Desugared . dropDollar . desugar flg . asExpr) c s

cPreprocess :: CmdRunner CState
cPreprocess c s = cTransform (Desugared . pre . asCore (flags s)) c s
  where pre = trsToCore . preProcess sys (ruleEnv sys) . coreToTrs
        sys = esystem s

cEval :: CmdRunner CState
cEval c s = cTransform (Desugared . run flg (esystem s) . asCore flg) c s
  where flg = flags s

systemDescr :: ESystem -> String
systemDescr s = sname s ++ ": " ++ description s
-}

--------------------------------------------------------
--         Displaying the last expression
--------------------------------------------------------

cShow :: CmdRunner CState
cShow =
  withLastExpr $ \ e s -> do
    print e
    pure s

cPrint :: CmdRunner CState
cPrint =
  withLastExpr $ \ e s -> do
    pp e
    pure s

--------------------------------------------------------
--         Adding and displaying definitions
--------------------------------------------------------

cDefine :: CmdRunner CState
cDefine =
  withLastExpr $ \ e s -> do
    let !e' = asExpr e
    pure  s{ cs_definitions = cs_definitions s ++ [e'] }

cClear :: CmdRunner CState
cClear _ s = pure s{ cs_definitions = [] }

cDisplay :: CmdRunner CState
cDisplay _ s = do
  let (prel, _) = fPrelude (cs_flags s)
  putStrLn "prelude:"; pp prel
  putStrLn "definitions:"
  mapM_ pp $ cs_definitions s
  pure s

--------------------------------------------------------
--         Seting the Prelude
--------------------------------------------------------

cPrelude :: CmdRunner CState
cPrelude "" s = do putStrLn $ "current prelude: " ++ fst (fPrelude (cs_flags s))
                   pure s
cPrelude line s = setPrelude line s

setPrelude :: String -> CState -> IO CState
setPrelude pn cs =
  case findPrelude pn of
    Left msg -> error $ "prelude failed " ++ msg
    Right prel -> pure cs{ cs_flags = (cs_flags cs){ fPrelude = prel } }


{-

cVerify :: CmdRunner CState
cVerify = do
  withLastExpr $ \ e s ->
    tryIt (pure s) (\ _ -> pure s) $ do
      let sys  = verifier
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
        let trc' = filterTrace (displayRules sys) trc
        putStrLn "Verification trace:"
        putStrLn $ unlines $ showTrace trc'
      if done then
        putStrLn "Verified"
       else do
        putStrLn "Not verified, residual term:"
        pp $ snd $ head $ toList trc
      pure ()


cRules :: CmdRunner CState
cRules "" s = do putStrLn ("rules: " ++ systemDescr (esystem s)); pure s
cRules line s =
  case findSystem line of
    Left msg -> do putStrLn msg; pure s
    Right sys -> do
      putStrLn $ "Selected=" ++ systemDescr sys
      pure s{ esystem = sys,
              flags = adjustFlags sys (flags s) }

cDefEval :: CmdRunner CState
cDefEval c s = do
  let addDefs e = Seq $ maybeToList (prelude s) ++ definitions s ++ [e]
      flg = EFlags { underLambda = fUnderLambda (flags s), traceEval = fTrace (flags s), steps = fEvalSteps (flags s) }
  cTransform (Cored . eval flg . simpCore . asCore (flags s) . Parsed . addDefs . asExpr) c s

-}

--------------------------------------------------------
--
--         Monad utilities
--
--------------------------------------------------------


tryIt :: IO b -> (a -> IO b) -> IO a -> IO b
tryIt iob aiob ioa = do
  e <- try ioa
  case e of
    Left (exn :: SomeException) -> do
      print exn
      iob
    Right a -> aiob a


--------------------------------------------------------
--
--         Other utilities
--
--------------------------------------------------------

{-
dropDollar :: SrcExpr -> SrcExpr
dropDollar = transform f . transformBi g
  where g (Ident l s) = Ident l $ filter (/= '$') s
        f (EPrim s) = EPrim $ filter (/= '$') s
        f x = x
-}

isVerifyPrelude :: (PreludeName, SrcExpr) -> Bool
isVerifyPrelude (pn, _) = "verify" `isPrefixOf` pn

