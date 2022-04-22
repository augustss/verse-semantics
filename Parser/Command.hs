{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll #-}
-- Simple Gofer/Hugs/ghci style command interpreter.
module Command(Command(..), Cmd(..), Run, runCommand) where
import Data.Char
import Data.List
import Text.Printf
import REPL

type Run s = String -> s -> IO s

data Cmd s = Cmd
  { cmd_string :: String
  , cmd_help :: String
  , cmd_exec :: Run s
  }

data Command s = Command
  { c_commands :: [Cmd s]
  , c_exec     :: Run s
  , c_help     :: String
  , c_greet    :: String
  , c_bye      :: String
  , c_prompt   :: String
  , c_state    :: s
  }

runCommand :: forall s . Command s -> IO ()
runCommand Command{..} = do
  putStrLn c_greet
  let
    commands = [Cmd "help" "Print this message" help
               ,Cmd "quit" "Quit program"       undefined] ++
               c_commands
    rpl = REPL { repl_init = pure (c_prompt, c_state)
               , repl_eval = eval
               , repl_exit = const $ putStrLn c_bye }
    help :: Run s
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
                      case filter (\ (Cmd c _ _) -> w `isPrefixOf` c) commands of
                        [] -> do
                          putStrLn "Cannot parse command.  Use :help to get help."
                          pure (False, s)
                        Cmd "quit" _ _ : _ -> pure (True, s)
                        Cmd _ _ run : _ -> do
                          s' <- run (dropWhile isSpace rest) s
                          pure (False, s')
                where line'' = dropWhile isSpace line'
        "" ->
          pure (False, s)
        line' -> do
          s' <- c_exec line' s
          pure (False, s')
  repl rpl

_testCommand :: Command Int
_testCommand = Command
  { c_commands = [Cmd "hello" "Greet" $ \ s i -> do putStrLn ("Hello " ++ s); pure i
                 ,Cmd "increment" "Increment state" $ \ _ i -> pure (i+1)
                 ]
  , c_exec = \ s i -> do putStrLn $ "Command=" ++ show s ++ " state=" ++ show i; pure i
  , c_help = "Available commands"
  , c_greet = "Hello tester!"
  , c_bye = "Bye then!"
  , c_prompt = "> "
  , c_state = 1
  }

