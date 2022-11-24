module VerseRepl.REPL(REPL(..), repl) where
import Control.Monad.Trans
import System.Console.Haskeline

data REPL s = REPL {
    repl_init :: IO (String, s),                        -- prompt and initial state
    repl_eval :: s -> String -> IO (Bool, s),           -- quit flag and new state
    repl_exit :: s -> IO (),
    repl_hist :: Maybe FilePath
    }

repl :: REPL s -> IO ()
repl p = do
    (prompt, state) <- repl_init p
    let loop s = do
            mline <- getInputLine prompt
            case mline of
                Nothing -> loop s
                Just line -> do
                     (quit, s') <- liftIO $ repl_eval p s line
                     if quit then
                         liftIO $ repl_exit p s'
                      else
                         loop s'
        settings = defaultSettings { historyFile = repl_hist p }
    runInputT settings (loop state)
