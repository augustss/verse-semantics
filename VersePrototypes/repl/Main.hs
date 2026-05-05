{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Main(main) where
import Debug.Trace
import Prelude

import Core.Expr     as Core
import Core.Rule     as Core
import Core.Rules    as TRS2024
import Core.Verifier as Verifier
import Core.Traced   as Core

import FrontEnd.CopyHook
import FrontEnd.Desugar
import FrontEnd.Expr as Src
import FrontEnd.Flags( Flags(..), defaultFlags )
import FrontEnd.ToCore
import FrontEnd.Prelude( findPrelude )

import qualified Parser.Verse   as LP
--import FrontEnd.Error

-- verse-densem
import SExp

-- plancc densem
import PlanCC(edenSem, edenSemDS, CExp)
import SExpC(srcExprToExp)

-- Tim densem
import qualified TimE (den)
import qualified SLS (den)
import qualified Pom (den)
import qualified PomPom (denS, ForUnionMode(..), ForUnitMode(..), IfUnionMode(..), Config(..), defaultConfig)
import qualified SemClass (den)
import qualified Red as Simon (run)
import FrontEnd.ENVDesugar (envDesugar)

-- Epic libraries
import Epic.Repl
import Epic.Print hiding( (<>) )   -- In this module (<>) is Prelude.<>

-- General library utilities
import Control.Exception(SomeException, try)
import Control.Monad
import Control.Arrow (second)
import Data.List
import Data.Maybe
import Text.Printf
import Text.Read(readMaybe)
import qualified Options.Applicative as OA
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Char (isSpace, isAlphaNum)
import qualified Data.ByteString.Char8 as B

--------------------------------------------------------
--
--            The main program
--
--------------------------------------------------------

main :: IO ()
main = do
  copyHook
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
  { mf_wslbug      :: !Bool
  , mf_preludeName :: !String
  , mf_desugar     :: !Bool
  , mf_simplify    :: !Bool
  , mf_fileNames   :: ![FilePath]          -- input files
  }

mainFlags :: OA.Parser MainFlags
mainFlags = MainFlags
  <$> OA.switch
      ( OA.long "wsl"
      <> OA.help "Add extra NL to compensate for WSL bug" )
  <*> OA.strOption
      ( OA.long "prelude"
      <> OA.short 'p'
      <> OA.metavar "NAME"
      <> OA.value "miniprelude"
      <> OA.help "Use built in prelude NAME" )
  <*> OA.switch
      ( OA.long "ddesugar"
      <> OA.help "Debug - show desugared" )
  <*> OA.switch
      ( OA.long "simplify"
      <> OA.help "simplify core" )
  <*> OA.many (OA.argument OA.str (OA.metavar "FILES..."))

mainArgs :: IO MainFlags
mainArgs = do
  let prf = OA.prefs OA.disambiguate
  OA.customExecParser prf $ OA.info (mainFlags OA.<**> OA.helper)
             ( OA.fullDesc
            <> OA.progDesc "Verse interactive system"
            <> OA.header "verse - Parse, desugar, and evaluate Verse expressions"
             )

--------------------------------------------------------
--
--         CState: state of the REPL
--
--------------------------------------------------------

data CState = CState
  { cs_lastExpr    :: !(Maybe SrcExpr)
  , cs_lastFile    :: !(Maybe FilePath)
  , cs_definitions :: ![SrcExpr]
  , cs_flags       :: !Flags
  , cs_esystem     :: !ESystem
  , cs_variables   :: !(VariableMap)
  -- PomPom related settings
  , cs_pp_forunion :: PomPom.ForUnionMode
  , cs_pp_forunit  :: PomPom.ForUnitMode
  , cs_pp_ifunion  :: PomPom.IfUnionMode
  , cs_pp_tree     :: Bool
  }

type VariableMap = HashMap String String
data ESystem = ESystemPlaceHolder  -- Just for now

printWithHdr :: String -> Doc -> DsM ()
printWithHdr s d
  = doIO_D (displayDoc (addHeader s d))


addHeader :: String -> Doc -> Doc
addHeader s doc
 = vcat [ text ""
        , text ("================ " ++ s ++ " ===================")
        , doc ]

updateLastExpr :: CState -> SrcExpr -> IO CState
updateLastExpr s e
  = do { display e
       ; pure s{ cs_lastExpr = Just e } }

getInputExpr :: (SrcExpr -> CState -> IO CState) -> CmdRunner CState
getInputExpr cmd line s
  = do { -- Read the input expression, or use the current one
         s' <- if null line
               then pure s
               else cParseLine line s
       ; case cs_lastExpr s' of
           Nothing -> do { putStrLn "No current expression"
                         ; return s' }
           Just e  -> cmd e s' }

--------------------------------------------------------
--
--         The commands!
--
--------------------------------------------------------

theCommandSet :: CommandSet CState
theCommandSet = CommandSet
  -- NB: the REPL adds :quit and :help
  { c_commands =
      [ Cmd "show [EXPR]"          "Show [last] expression"                cShow
      , Cmd "print [EXPR]"         "Pretty print [last] expression"        cPrint
      , Cmd "set"                  "Turn on flag (':set' shows flags)"     (cSet True)
      , Cmd "unset"                "Turn off flag"                         (cSet False)
      , Cmd "prelude [NAME]"       "Select prelude"                        cPrelude
--       , Cmd "display"              "Show current global defs"              cDisplay
--       , Cmd "clear"                "Clear global defs"                     cClear
--      , Cmd "define [EXPR]"        "Add [last] expression to global defs"  cDefine
      , Cmd "let NAME [EXPR]"      "Store the result of EXPR as NAME"      cDefine
      , Cmd "display [NAME]"       "Show the current variables"            cShowVars
      , Cmd "delete  [NAME]"       "Remove the variable[s] [NAME0 NAME1]"  cClear
      , Cmd "read FILE"            "Parse a file"                          cRead

      , Cmd "essential [EXPR]"  "Desugar [last] expression to Essential" (runGetterSrc getEssential)
      , Cmd "mini [EXPR]"       "Desugar [last] expression to Mini"      (runGetterSrc getMini)
      , Cmd "src-core [EXPR]"   "Convert [last] expression to SrcCore"   (runGetterSrc getSrcCore)
      , Cmd "core [EXPR]"       "Convert [last] expression to Core"      (runGetterCore getCore)

      , Cmd "eval [EXPR]"          "Evaluate [last] expression"            cEval
      , Cmd "old-densem [EXPR]"    "Evaluate [last] expression"            cDensem
      , Cmd "dls-densem [EXPR]"    "Evaluate [last] expression"            cDlsDensem
      , Cmd "tim-densem [EXPR]"    "Evaluate [last] expression"            cTimDensem
      , Cmd "sls-densem [EXPR]"    "Evaluate [last] expression"            cSlsDensem
      , Cmd "pom-densem [EXPR]"    "Evaluate [last] expression"            cPomDensem
      , Cmd "densem [EXPR]"        "Evaluate [last] expression"            cSemClassDensem
      , Cmd "ppom [EXPR]"          "Evaluate [last] expression"            cPomPomDensem
      , Cmd "simon [EXPR]"         "Reduce [last] expression"              cSimon

          -- Use Koen's:  normalizeTrace :: Rule -> Expr -> Traced Expr

--       , Cmd "test [FILE]"          "Run the tests in FILE"              cTest

--      , Cmd "verify [EXPR]"        "Verify [last] expression"              cVerify

--      , Cmd "pcore EXPR"           "parse core expression"                 cPcore
--      , Cmd "pdesugar [EXPR]"      "Desugar [last] expression pretty"      cPDesugar
--      , Cmd "pvdesugar [EXPR]"     "Desugar (for verification) [last] expression pretty"      cPDesugarVerify
--      , Cmd "deval [EXPR]"         "Evaluate [last] expression with global defs"  cDefEval
--      , Cmd "preprocess"           "Preprocess for rule set"                 cPreprocess
--      , Cmd "rules [NAME]"         "Select rule system"                    cRules
      ]

  -- c_exec :: CmdRunner deals with a command not starting with colon
  , c_exec = cParseLine
  , c_help   = helpMsg
  , c_greet  = "Verse parse, desugar, and evaluation testing.\nUse :help for help, and :quit to quit."
  , c_bye    = "Bye!"
  , c_prompt = "> "
  , c_state  = CState { cs_lastExpr = Nothing
                      , cs_lastFile = Nothing
                      , cs_definitions = []
                      , cs_flags = defaultFlags{fSplit=True, fNoFuelStop=True, fSimplify=True}
                      , cs_esystem = ESystemPlaceHolder
                      , cs_variables = mempty
                      , cs_pp_forunion = PomPom.forUnionMode PomPom.defaultConfig
                      , cs_pp_ifunion = PomPom.ifUnionMode PomPom.defaultConfig
                      , cs_pp_forunit = PomPom.forUnitMode PomPom.defaultConfig
                      , cs_pp_tree = PomPom.useTree PomPom.defaultConfig
                      }
  , c_history = Just ".versei"
  , c_nl = False
  }

helpMsg :: String
helpMsg = "\
\Enter an EXPR to parse or a command.\n\
\Many commands operate on the last printed expression.\n\
\Try\n\
\  > 1+2\n\
\  > :mini\n\
\  > :core\n\
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
-- Bool is True to set, False to unset

-- :set on its own shows the current settings
cSet _ "" s = do
  let f (d,(g,_)) = printf "  %-12s %s\n" d $ if g (cs_flags s) then "on" else "off"
  putStr $ concatMap f flagTable
  printf "  %-12s= %d\n" "steps" (fRewriteSteps (cs_flags s))
  printf "  %-12s= %-10s  choices:%s\n" "forunion" (show $ cs_pp_forunion s) (show [minBound..maxBound `asTypeOf` cs_pp_forunion s])
  printf "  %-12s= %-10s  choices:%s\n" "forunit" (show $ cs_pp_forunit s) (show [minBound..maxBound `asTypeOf` cs_pp_forunit s])
  printf "  %-12s= %-10s  choices:%s\n" "ifunion" (show $ cs_pp_ifunion s) (show [minBound..maxBound `asTypeOf` cs_pp_ifunion s])
  printf "  %-12s= %s\n" "tree" (show $ cs_pp_tree s)
  pure s

cSet True l s | Just l' <- stripPrefix "forunion=" l
              , Just m <- readMaybe l'
  = pure $ s{ cs_pp_forunion = m }

cSet True l s | Just l' <- stripPrefix "forunit=" l
              , Just m <- readMaybe l'
  = pure $ s{ cs_pp_forunit = m }

cSet True l s | Just l' <- stripPrefix "ifunion=" l
              , Just m <- readMaybe l'
  = pure $ s{ cs_pp_ifunion = m }

cSet b l s | l == "tree"
  = pure $ s{ cs_pp_tree = b }

-- Set fRewriteSteps
cSet True l s | Just l' <- stripPrefix "steps=" l
              , Just n <- readMaybe l'
  = pure $ s{ cs_flags = (cs_flags s){fRewriteSteps = n} }

-- Set fTraceVerbosity
cSet True l s | Just l' <- stripPrefix "verbosity=" l
              , Just n <- readMaybe l'
  = pure $ s{ cs_flags = (cs_flags s){fTraceVerbosity = n} }

-- Set/unset all the rest
cSet b l s =
  case filter (isPrefixOf l . fst) flagTable of
    [] -> do putStrLn "Unknown flag"; pure s
    [(_, (_, set))] -> pure $ s{ cs_flags = set b (cs_flags s) }
    xs -> do putStrLn $ "Ambiguous flag: " ++ show (map fst xs); pure s

flagTable :: [(String, (Flags -> Bool, Bool -> Flags -> Flags))]
flagTable =
  [("verify",      (fVerify,       \ b s -> s{fVerify=b}))
  ,("trace-eval",  (fTraceEval,    \ b s -> s{fTraceEval=b}))
  ,("ds-uniform",  (fDsUniform,    \ b s -> s{fDsUniform=b}))
  ,("quiet",       (fQuiet,        \ b s -> s{fQuiet=b}))
--  ,("simplify",    (fSimplify,     \ b s -> s{fSimplify=b}))
--  ,("split",       (fSplit,        \ b s -> s{fSplit=b}))
--  ,("trace",       (fTrace,        \ b s -> s{fTrace=b}))
--  ,("underLambda", (fUnderLambda,  \ b s -> s{fUnderLambda=b}))
--  ,("latex",       (fLatex,        \ b s -> s{fLatex=b}))
--  ,("dfs",         (fDfs,          \ b s -> s{fDfs=b}))
--  ,("desugartrace",(fTraceDesugar, \ b s -> s{fTraceDesugar=b}))
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
  tryIt (\_exc -> pure s') (updateLastExpr s') $ do
    file <- readFile fn
    let prog = LP.parseToSrcExpr fn (B.pack file)
    when (prog == prog) $
      putStrLn "OK"
    pure prog


cParseLine :: CmdRunner CState
cParseLine line' s
  = tryIt (\_exc -> pure s) (updateLastExpr s) $ do
    let !line = substitute (cs_variables s) line'
        !prog = LP.parseToSrcExpr "<interactive>" (B.pack line)
    traceM $ "cParseLine " ++ show prog
    pure prog

variableSigil :: Char
variableSigil = '$'

-- | find all variable references in the input line and splice their payload
substitute :: VariableMap -> String -> String
substitute st = loop' go
  where
    -- extract-name assumes the leading char is the sigil if we do not find the
    -- sigil we just return the input in the fst position if we do find the
    -- sigil then parse the rest of the identifier and do the lookup. Note that
    -- 'span' is the converse of 'break', i.e., it puts that which satisfies the
    -- predicate in the fst position, whereas break puts that which /does not/
    -- satisfy the predicate in the fst position
    extract_name :: String -> (String, String)
    extract_name (s:rest) | s == variableSigil = span isAlphaNum rest
    extract_name other    = (other,mempty)

    go line = previous ++ new_name ++ rest
      where
        (previous, name_start) = break (== variableSigil) line
        (name, rest) = extract_name name_start
        new_name     = case HM.lookup name st of
                           Nothing      -> error $
                             "Variable not in scope: " ++ drop 1 name ++ "\n"
                             ++ "Variables references must be a $ followed by an alpha"
                             ++ "numeric string, i.e., $[A-Za-z0-9]+"
                           Just payload -> payload

    has_variables = elem variableSigil
    loop' f input | has_variables input = loop' f $ go input
                  | otherwise           = input


--------------------------------------------------------
--
--         Desugaring
--
--------------------------------------------------------

runGetterSrc :: (Flags -> SrcExpr -> DsM SrcExpr) -> CmdRunner CState
runGetterSrc = runGetter Src.Fail

runGetterCore :: (Flags -> SrcExpr -> DsM Core.Expr) -> CmdRunner CState
runGetterCore = runGetter Core.Fail

runGetter :: a -> (Flags -> SrcExpr -> DsM a) -> CmdRunner CState
runGetter err_result getter
  = getInputExpr $ \ e s ->
    let flags = cs_flags s
    in tryIt (\_exc -> pure s) (\_ -> pure s)
             (runD flags err_result (getter flags e))

getEssential :: Flags -> SrcExpr -> DsM SrcEssential
getEssential flg e_parsed
  = do { e_prel <- addPrelude e_parsed
       ; e_ess  <- sDesugarExpr e_prel
       ; when (not $ fQuiet flg) $
           printWithHdr "Essential" (pPrint e_ess)
       ; return e_ess }

getMini :: Flags -> SrcExpr -> DsM SrcMini
getMini flags e_parsed
  = do { e_ess  <- getEssential flags e_parsed
       ; e_mini <- essToMini flags e_ess
       ; printWithHdr "Mini" (pPrint e_mini)
       ; return e_mini }

getSrcCore :: Flags -> SrcExpr -> DsM SrcCore
getSrcCore flags e_parsed
  = do { e_mini     <- getMini flags e_parsed
       ; let add_verification = fVerify flags
       ; e_src_core <- miniToCore add_verification e_mini
       ; printWithHdr "SrcCore" (pPrint e_src_core)
       ; return e_src_core }

getCore :: Flags -> SrcExpr -> DsM Core.Expr
getCore flags e_parsed
  = do { e_src_core <- getSrcCore flags e_parsed
       ; e_core     <- convert e_src_core
       ; let prepd_core = Core.prep e_core
       ; printWithHdr "Prep'd Core" (pPrint prepd_core)
       ; return prepd_core }

cEval :: CmdRunner CState
cEval
  = getInputExpr $ \e s ->
    tryIt (\_ -> pure s) (\_ -> pure s) $
    do { let flags = cs_flags s
       ; prepd_core <- runD flags Core.Fail (getCore flags e)
       ; let rules | fVerify flags = everywhere verificationRules
                   | otherwise     = everywhere runtimeRules
       ; let (res, tr) = Core.normalizeExpr rules (fRewriteSteps flags) prepd_core

       ; let eval_doc = addHeader "Evaluate" $
                        case res of
                          NormOK      -> text "Result = " <+> pPrint (Core.term tr)
                          NormExpired -> text "Ran out of fuel"
                          NormInvalid -> hang (text "Reached an invalid expression:") 2
                                         (pPrint (Core.term tr))
       ; displayDoc eval_doc

       ; when (fTraceEval flags) $ do
           let trace_doc = addHeader "Evaluation trace"
                           $ vcat
                           $ pPrintTrace (fTraceVerbosity flags) tr
           displayDoc trace_doc

       ; return () }

cDensem :: CmdRunner CState
cDensem
  = getInputExpr $ \e s ->
    tryIt (\_ -> pure s) (\_ -> pure s)  $
    do { let flags = cs_flags s
       ; e_ess <- runD flags undefined $ getEssential flags e
       ; e_ds <- denSemDesugar e_ess
       ; res <- denSem e_ds
       ; let desugared = addHeader "Desugared" $ text $ show e_ds
             den_sem   = addHeader "Den-sem"   $ text $ show res

       ; displayDoc desugared
       ; displayDoc den_sem

       ; return () }

cDlsDensem :: CmdRunner CState
cDlsDensem
  = getInputExpr $ \e s ->
    tryIt (\_ -> pure s) (\_ -> pure s) $
    do { let flags = cs_flags s
       ; e_ess <- runD flags undefined $ getEssential flags e
       ; e_ds <- dlsDenSemDesugar e_ess
       ; res <- edenSem e_ds
       ; let desugared = addHeader "Desugared" $ text $ show e_ds
             den_sem   = addHeader "D-LS Den-sem"   $ text $ show res

       ; displayDoc desugared
       ; displayDoc den_sem

       ; return () }

dlsDenSemDesugar :: SrcExpr -> IO CExp
dlsDenSemDesugar = return . edenSemDS . srcExprToExp

eSlsDesugar :: Flags -> SrcEssential -> IO SrcEssential
eSlsDesugar flg e = do
  let e_ds = envDesugar e
  when (not $ fQuiet flg) $
    displayDoc $ addHeader "ENV desugar" $ pPrint e_ds
  --print e_ds
  return e_ds

cTimDensem :: CmdRunner CState
cTimDensem
  = getInputExpr $ \e s ->
    tryIt (\_ -> pure s) (\_ -> pure s) $
    do { let flags = cs_flags s
       ; e_ess <- runD flags undefined $ getEssential flags e
       ; e_ds <- eSlsDesugar flags e_ess
       ; let res = TimE.den e_ds
       ; let den_sem = addHeader "Tim Den-sem" $ text $ show res
{-
               if null res then text "No solutions"
               else vcat $ fmap (text . show) res
-}

       ; displayDoc den_sem

       ; return () }

cSlsDensem :: CmdRunner CState
cSlsDensem
  = getInputExpr $ \e s ->
    tryIt (\_ -> pure s) (\_ -> pure s) $
    do { let flags = cs_flags s
       ; e_ess <- runD flags undefined $ getEssential flags e
       ; e_ds <- eSlsDesugar flags e_ess
       ; let res = SLS.den e_ds
       ; let den_sem = addHeader "Sls Den-sem" $ text $ show res
{-
               if null res then text "No solutions"
               else vcat $ fmap (text . show) res
-}

       ; displayDoc den_sem

       ; return () }

cPomDensem :: CmdRunner CState
cPomDensem
  = getInputExpr $ \e s ->
    tryIt (\_ -> pure s) (\_ -> pure s) $
    do { let flags = cs_flags s
       ; e_ess <- runD flags undefined $ getEssential flags e
       ; e_ds <- eSlsDesugar flags e_ess
       ; let res = Pom.den e_ds
       ; let den_sem = addHeader "Pom Den-sem" $ text $ show res
       ; displayDoc den_sem
{-
       ; let resU = Pom.denU e_ds
       ; let den_semU = addHeader "Pom Den-sem, with empties" $ text $ show resU
       ; displayDoc den_semU
-}
       ; return () }

cSemClassDensem :: CmdRunner CState
cSemClassDensem
  = getInputExpr $ \e s ->
    tryIt (\_ -> pure s) (\_ -> pure s) $
    do { let flags = cs_flags s
       ; e_ess <- runD flags undefined $ getEssential flags e
       ; e_ds <- eSlsDesugar flags e_ess
       ; let res = SemClass.den e_ds
       ; let den_sem = addHeader "M Den-sem" $ text $ show res
       ; if fQuiet flags then
           displayDoc $ text $ show res
         else
           displayDoc den_sem
{-
       ; let resU = Pom.denU e_ds
       ; let den_semU = addHeader "Pom Den-sem, with empties" $ text $ show resU
       ; displayDoc den_semU
-}
       ; return () }

cSimon :: CmdRunner CState
cSimon
  = getInputExpr $ \e s ->
    tryIt (\_ -> pure s) (\_ -> pure s) $
    do { let flags = cs_flags s
       ; e_ess <- runD flags undefined $ getEssential flags e
       ; e_ds <- eSlsDesugar flags e_ess
       ; let res = Simon.run e_ds
       ; let den_sem = addHeader "Simon reduction" $ text $ show res
       ; if fQuiet flags then
           displayDoc $ text $ show res
         else
           displayDoc den_sem
{-
       ; let resU = Pom.denU e_ds
       ; let den_semU = addHeader "Pom Den-sem, with empties" $ text $ show resU
       ; displayDoc den_semU
-}
       ; return () }

cPomPomDensem :: CmdRunner CState
cPomPomDensem
  = getInputExpr $ \e s ->
    tryIt (\_ -> pure s) (\_ -> pure s) $
    do { let flags = cs_flags s
             cfg = PomPom.Config { PomPom.forUnionMode = cs_pp_forunion s, PomPom.forUnitMode = cs_pp_forunit s,
                                   PomPom.ifUnionMode = cs_pp_ifunion s, PomPom.useTree = cs_pp_tree s }
       ; e_ess <- runD flags undefined $ getEssential flags e
       ; e_ds <- eSlsDesugar flags e_ess
       ; (res, dtrace) <- PomPom.denS cfg (fTraceEval flags) e_ds
       ; let den_sem = addHeader settings $ text res
             settings = printf "PomPom (forUnion=%s forUnit=%s ifUnion=%s useTree=%s)"
                               (show (cs_pp_forunion s))
                               (show (cs_pp_forunit s))
                               (show (cs_pp_ifunion s))
                               (show (cs_pp_tree s))
       ; displayDoc den_sem
       ; mapM_ putStrLn dtrace
       ; return () }

--------------------------------------------------------
--         Displaying the last expression
--------------------------------------------------------

cShow :: CmdRunner CState
-- Use Hakell's derived Show to display all the data constructors
cShow = getInputExpr $ \ e s ->
        do { print e  -- Uses Haskell's Show
           ; pure s }

cPrint :: CmdRunner CState
-- Use the pretty-printer
cPrint = getInputExpr $ \ e s ->
         do { display e   -- Use the pretty-printer
            ; pure s }

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




--------------------------------------------------------
--         Adding and displaying definitions
--------------------------------------------------------

cDefine :: CmdRunner CState
cDefine line s = do
  let (name,expr) = second tail $ break isSpace line
  pure s{ cs_variables = HM.insert name expr $ cs_variables s}


cShowVars :: CmdRunner CState
cShowVars "" s = do
  displayDoc
    $ addHeader "Bound Repl Variables"
    $ vcat $ text <$> HM.keys (cs_variables s)
  pure s
cShowVars vs' s = do
  let vs            = words vs'
      findResult v  = HM.findWithDefault mempty v $ cs_variables s
      mkDoc v e     = addHeader (v ++ " Bound to") $ text e
      docs          = fmap (\v -> mkDoc v (findResult v)) vs
  mapM_ displayDoc docs
  pure s

cClear :: CmdRunner CState
cClear "" s = pure s{ cs_variables = mempty }
cClear xs s = pure s{ cs_variables = HM.filterWithKey go (cs_variables s) }
  where
    to_remove = words xs
    go k _    = k `notElem` to_remove

{-
cDefine :: CmdRunner CState
cDefine =
  withLastExpr $ \ e s -> do
    let !e' = asSrcExpr e
    pure  s{ cs_definitions = cs_definitions s ++ [e'] }

cClear :: CmdRunner CState
cClear _ s = pure s{ cs_definitions = [] }

cDisplay :: CmdRunner CState
cDisplay _ s = do
  let (prel, _) = fPrelude (cs_flags s)
  putStrLn "prelude:"; display prel
  putStrLn "definitions:"
  mapM_ display $ cs_definitions s
  pure s
-}

--------------------------------------------------------
--
--         Evaluation and verification
--
--------------------------------------------------------

{-
cEval :: CmdRunner CState
cEval
  = withLastExpr $ \ e s ->
    tryIt (pure s) (updateLastExpr s) $
    do { putStrLn ("\n\n------- Prep'd ---------")
       ; let core_expr, prepd_expr :: Core.Expr
             core_expr  = asCore e
             prepd_expr = prep core_expr
       ; putStrLn (prettyShow prepd_expr)

       ; putStrLn ("\n\n------- Evaluate ---------")
       ; let eval_it = normalize (fEvalSteps (cs_flags s)) (everywhere TRS2024.runtimeRules)

       ; core_result <- showEvalResult (fTraceEval $ cs_flags s) "Evaluation" (eval_it prepd_expr)

       ; pure (RulesCore core_result) }


cVerify :: CmdRunner CState
cVerify
  = withLastExpr $ \ e s ->
    tryIt (pure s) (updateLastExpr s) $
    do { putStrLn ("\n\n------- Prep'd ---------")
       ; let verify_it = normalize (fEvalSteps (cs_flags s))
                              (everywhere Verifier.verificationRules)
       ; let core_expr, prepd_expr :: Core.Expr
             core_expr  = asCore e
             prepd_expr = prep core_expr
       ; putStrLn (prettyShow prepd_expr)

       ; putStrLn ("\n\n------- Verify ---------")
       ; e' <- showEvalResult (fTraceVerify $ cs_flags s) "Verification" (verify_it prepd_expr)

       ; pure (RulesCore e') }


showEvalResult :: Bool -> String -> (NormResult, Traced Core.Expr) -> IO Core.Expr
showEvalResult False _ (_, (e' :<-- _))
  = do { putStrLn (prettyShow e')
       ; return e' }
showEvalResult _ what (res, tr@(e' :<-- _))
  = do { putStrLn (what ++ " " ++ showNormResult res)
       ; display tr
       ; return e' }


cTransform :: Bool                    -- True <=> display the result
           -> (SomeExpr -> SomeExpr)  -- How to transform
           -> CmdRunner CState
cTransform display_result tr =
  withLastExpr $ \ e s ->
    tryIt (pure s) (updateLastExpr s) $ do
    do { e' <- evaluate (tr e)
       ; when display_result $ display e'
       ; pure e' }


cPcore :: CmdRunner CState
cPcore line s =
  tryIt (pure s) (updateLastExpr s) $ do
    let prog = parseDie (Desugared <$> pCoreFile) "<interactive>" line
    display prog
    pure prog


cPDesugar :: CmdRunner CState
cPDesugar c s = do
  let flg = (flags s){ fSimplify = True, fSplit = False, fAssumeVerified = True, fKeepIf = True,
                       fPrelude = either error id $ findPrelude "miniprelude" }
  putStrLn $ "Desugar for execution, prettyfied: rules=" ++ show (fDesugar flg) ++ ", prelude=" ++ fst (fPrelude flg)
  cTransform True (Desugared . dropDollar . desugar flg . asSrcExpr) c s

cDesugarVerify :: CmdRunner CState
cDesugarVerify c s = do
  let aflg = flags s
      -- This is a hack to avoid the default prelude for verification.
      prel = if isVerifyPrelude (fPrelude aflg) then fPrelude aflg else either error id $ findPrelude "verifyprelude"
      flg = aflg{ fVerify = True, fSplit = False, fAssumeVerified = False, fPrelude = prel }
  putStrLn $ "Desugar for verification: rules=" ++ show (fDesugar flg) ++ ", prelude=" ++ fst (fPrelude flg)
  cTransform True (Desugared . desugar flg . asSrcExpr) c s

cPDesugarVerify :: CmdRunner CState
cPDesugarVerify c s = do
  let aflg = flags s
      -- This is a hack to avoid the default prelude for verification.
      prel = if isVerifyPrelude (fPrelude aflg) then fPrelude aflg else either error id $ findPrelude "verifyprelude"
      flg = aflg{ fVerify = True, fSplit = False, fAssumeVerified = False, fPrelude = prel, fSimplify = True }
  putStrLn $ "Desugar for verification: rules=" ++ show (fDesugar flg) ++ ", prelude=" ++ fst (fPrelude flg)
  cTransform True (Desugared . dropDollar . desugar flg . asSrcExpr) c s

cPreprocess :: CmdRunner CState
cPreprocess c s = cTransform True (Desugared . pre . asCore (flags s)) c s
  where pre = trsToCore . preProcess sys (ruleEnv sys) . coreToTrs
        sys = esystem s

systemDescr :: ESystem -> String
systemDescr s = sname s ++ ": " ++ description s


isVerifyPrelude :: (PreludeName, SrcExpr) -> Bool
isVerifyPrelude (pn, _) = "verify" `isPrefixOf` pn

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
        display $ snd $ head $ toList trc
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
  cTransform True (Cored . eval flg . simpCore . asCore (flags s) . Parsed . addDefs . asSrcExpr) c s

-}

--------------------------------------------------------
--
--         Monad utilities
--
--------------------------------------------------------


tryIt :: (SomeException -> IO b) -> (a -> IO b) -> IO a -> IO b
tryIt recover success_cont try_me = do
  e <- try try_me
  case e of
    Left exn -> print exn >> recover exn
    Right a  -> success_cont a
