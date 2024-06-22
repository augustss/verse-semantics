{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Simple Gofer/Hugs/ghci style command interpreter.

module Epic.Repl(
    CommandSet(..),
    Cmd(..),
    CmdRunner,
    runCommands
 ) where

import Data.Char
import Data.List
import Text.Printf
import Control.Monad( when )
import Control.Monad.Trans( liftIO )

import System.Console.Haskeline as HL

data CommandSet s = CommandSet
  { c_commands :: [Cmd s]
  , c_exec     :: CmdRunner s   -- Use this if the input line does
                                -- not match any of the c_commands
  , c_help     :: String
  , c_greet    :: String
  , c_bye      :: String
  , c_prompt   :: String
  , c_state    :: s
  , c_history  :: Maybe FilePath
  , c_nl       :: !Bool
  }

data Cmd s = Cmd  -- Describes a single command
  { cmd_string :: String    -- The command name
  , cmd_help   :: String    -- Help text for the command
  , cmd_exec   :: CmdRunner s     -- Use this to run the command
  }

type CmdRunner s = String -> s -> IO s
  -- Run the command with this input argument string
  -- (The string does not include the command name itself.)

runCommands :: forall s . CommandSet s -> IO ()
-- Run a REPl driven by the given CommandSet
runCommands CommandSet{..} = do
  putStrLn c_greet
  let
    commands = [Cmd "help" "Print this message" help
               ,Cmd "quit" "Quit program"       undefined] ++
               c_commands
    rpl = REPL { repl_init = pure (c_prompt, c_state)
               , repl_eval = eval
               , repl_exit = const $ putStrLn c_bye
               , repl_hist = c_history
               , repl_nl   = c_nl }

    help :: CmdRunner s
    help _ s = do
      putStrLn c_help
      let l = maximum (map (\ (Cmd c _ _) -> length c) commands)
      mapM_ (\ (Cmd c h _) -> printf ":%-*s  %s\n" l c h) commands
      pure s

    eval :: s -> String -> IO (Bool, s)
    eval s line =
      case dropWhile isSpace line of
        ':' : line' | null line'' -> pure (False, s)
                    | otherwise -> do
                      let (w, rest) = span isAlpha line''
                      case filter (\ c -> w `isPrefixOf` cmd_string c) commands of
                        [] -> do
                          putStrLn "Cannot parse command.  Use :help to get help."
                          pure (False, s)
                        Cmd { cmd_string = "quit" } : _ -> pure (True, s)
                        Cmd { cmd_exec = run } : _ -> do
                          s' <- run (dropWhile isSpace rest) s
                          pure (False, s')
                where line'' = dropWhile isSpace line'
        "" ->
          pure (False, s)
        line' -> do
          s' <- c_exec line' s
          pure (False, s')
  repl rpl

_testCommand :: CommandSet Int
_testCommand = CommandSet
  { c_commands = [Cmd "hello" "Greet" $ \ s i -> do putStrLn ("Hello " ++ s); pure i
                 ,Cmd "increment" "Increment state" $ \ _ i -> pure (i+1)
                 ]
  , c_exec = \ s i -> do putStrLn $ "Command=" ++ show s ++ " state=" ++ show i; pure i
  , c_help = "Available commands"
  , c_greet = "Hello tester!"
  , c_bye = "Bye then!"
  , c_prompt = "> "
  , c_state = 1
  , c_history = Nothing
  , c_nl = False
  }

--------------------------------------------------------
--
--            The main REPL
--
--------------------------------------------------------

data REPL s = REPL {
    repl_init :: IO (String, s),                        -- prompt and initial state
    repl_eval :: s -> String -> IO (Bool, s),           -- quit flag and new state
    repl_exit :: s -> IO (),
    repl_hist :: Maybe FilePath,
    repl_nl   :: !Bool                                  -- extra NL to compensate for WSL bug
    }

repl :: REPL s -> IO ()
repl p = do
    (prompt, state) <- repl_init p
    let loop s = do
            mline <- getInputLine prompt
            when (repl_nl p) $
              liftIO $ putStrLn ""
            case mline of
                Nothing -> loop s
                Just line -> do
                     (quit, s') <- liftIO $ repl_eval p s line
                     if quit then
                         liftIO $ repl_exit p s'
                      else
                         loop s'
        settings = HL.defaultSettings { HL.historyFile = repl_hist p }
    HL.runInputT settings (loop state)
