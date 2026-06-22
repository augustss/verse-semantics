{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Main(main) where
--import Debug.Trace

import Core.Traced(getTerm, displayTraceV)
import FrontEnd.Expr as Src
import FrontEnd.Desugar
import FrontEnd.Flags( Flags(..), defaultFlags )

import qualified Parser.Verse   as LP
--import FrontEnd.Error


import qualified Red

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
  main_flags :: MainFlags <- mainArgs
  let cs1 = c_state theCommandSet

--  let msys = fmap (either error id . findSystem) (rulesys flags)
--  let cs2 = case msys of
--              Nothing -> cs1
--              Just sys -> cs1{ esystem = sys }
--      cs3 = cs2{ flags = adjustFlags msys (flags cs2) }

  cs3 <- return cs1 -- setPrelude (mf_preludeName main_flags) cs1
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
  , mf_desugar     :: !Bool
  , mf_simplify    :: !Bool
  , mf_fileNames   :: ![FilePath]          -- input files
  }

mainFlags :: OA.Parser MainFlags
mainFlags = MainFlags
  <$> OA.switch
      ( OA.long "wsl"
      <> OA.help "Add extra NL to compensate for WSL bug" )
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
  = do { when (not (fQuiet (cs_flags s))) $
           display e
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
      , Cmd "let NAME [EXPR]"      "Store the result of EXPR as NAME"      cDefine
      , Cmd "display [NAME]"       "Show the current variables"            cShowVars
      , Cmd "delete  [NAME]"       "Remove the variable[s] [NAME0 NAME1]"  cClear
      , Cmd "read FILE"            "Parse a file"                          cRead

      , Cmd "essential [EXPR]"     "Desugar [last] expression to Essential" (runGetterSrc getEssential)

      , Cmd "red [EXPR]"           "Reduce [last] expression"                cRed
      , Cmd "parse [EXPR]"         "Parse [last] expression"                 cParseLine

      ]

  -- c_exec :: CmdRunner deals with a command not starting with colon
  , c_exec = cRed
  , c_help   = helpMsg
  , c_greet  = "Verse read-eval-print loop.\nUse :help for help, and :quit to quit."
  , c_bye    = "Bye!"
  , c_prompt = "> "
  , c_state  = CState { cs_lastExpr = Nothing
                      , cs_lastFile = Nothing
                      , cs_definitions = []
                      , cs_flags = defaultFlags{fSplit=True, fNoFuelStop=True, fSimplify=True}
                      , cs_esystem = ESystemPlaceHolder
                      , cs_variables = mempty
                      }
  , c_history = Just ".versei"
  , c_nl = False
  }

helpMsg :: String
helpMsg = "\
\Enter an expression to evaluated, or a command starting with ':'.\n\
\Type ':' to see all the commands.\n\
\Many commands operate on the last printed expression.\n\
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
  printf "  %-12s= %d\n" "verbosity" (fTraceVerbosity (cs_flags s))
  pure s

-- Set fRewriteSteps
cSet True l s | Just l' <- stripPrefix "steps=" l
              , Just n <- readMaybe l'
  = pure $ s{ cs_flags = (cs_flags s){fRewriteSteps = n} }

-- Set fTraceVerbosity
cSet True l s | Just l' <- stripPrefix "verbosity=" l
              , Just n <- readMaybe l'
  = pure $ s{ cs_flags = (cs_flags s){fTraceVerbosity = n} }
-- Set fTraceVerbosity
cSet True l s | Just l' <- stripPrefix "trace-verbosity=" l
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
  ,("quiet",       (fQuiet,        \ b s -> s{fQuiet=b}))
  ,("match-first", (fMatchFirst,   \ b s -> s{fMatchFirst=b}))
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
--    traceM $ "cParseLine " ++ show prog
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

runGetter :: a -> (Flags -> SrcExpr -> DsM a) -> CmdRunner CState
runGetter err_result getter
  = getInputExpr $ \ e s ->
    let flags = cs_flags s
    in tryIt (\_exc -> pure s) (\_ -> pure s)
             (runD flags err_result (getter flags e))

getEssential :: Flags -> SrcExpr -> DsM SrcEssential
getEssential flg e_parsed
  = do { e_ess  <- sDesugarExpr e_parsed
       ; when (not $ fQuiet flg) $
           printWithHdr "Essential" (pPrint e_ess)
       ; return e_ess }

cRed :: CmdRunner CState
cRed
  = getInputExpr $ \e s ->
    tryIt (\_ -> pure s) (\_ -> pure s) $
    do { let flags = cs_flags s
       ; e_ds <- runD flags undefined $ getEssential flags e
       ; -- XXX setJustMatching
       ; let (top_cxt, top_blk) = Red.initialBlk e_ds
       ; m_blk <-
           if fMatchFirst flags then do
             let { tr_mtch = Red.runTraced (fRewriteSteps flags) (Red.setJustMatching top_cxt) top_blk }
             displayDoc $ addHeader "Match reduction trace" $ text ""
             displayTraceV (fTraceVerbosity flags) tr_mtch
             return $ getTerm tr_mtch
           else
             return top_blk
       ; let tr_res = Red.runTraced (fRewriteSteps flags) top_cxt m_blk
             res = getTerm tr_res
             res_string = prettyShow res
       ; when (fTraceEval flags) $ do
           displayDoc $ addHeader "Reduction trace" $ text ""
           displayTraceV (fTraceVerbosity flags) tr_res
       ; if fQuiet flags then
           displayDoc $ text res_string
         else
           displayDoc $ addHeader "Reduced" $ text res_string
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
