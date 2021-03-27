module MonadApp where

import Ivo

import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans (lift)
import Data.HashMap.Strict (HashMap)
import Data.Text qualified as T
import Data.Text.IO (hPutStrLn)
import System.Console.Haskeline
  ( InputT, runInputT, defaultSettings
  , outputStrLn, handleInterrupt, withInterrupt
  )
import System.IO (stderr)

type AppM = ExceptT Text (StateT AppState (InputT IO))

runAppM :: InterpreterOpts -> AppM () -> IO ()
runAppM opts =
  runInputT defaultSettings . justDie . flip evalStateT appState . handleErrors
  where
    appState :: AppState
    appState = defaultAppState { interpreterOpts = opts }

    -- | Immediately quit the program when interrupted
    -- without performing any additional actions.
    -- (Without this, it will print an extra newline for some reason.)
    justDie :: (MonadIO m, MonadMask m) => InputT m () -> InputT m ()
    justDie = handleInterrupt (pure ()) . withInterrupt

handleErrors :: AppM () -> StateT AppState (InputT IO) ()
handleErrors m = do
  result <- runExceptT m
  case result of
    Left err -> liftIO $ hPutStrLn stderr err
    Right _ -> pure ()

liftInput :: InputT IO a -> AppM a
liftInput = lift . lift

outputTextLn :: MonadIO m => Text -> InputT m ()
outputTextLn = outputStrLn . T.unpack

outputStderrLn :: Text -> AppM ()
outputStderrLn = liftIO . hPutStrLn stderr

data AppState = AppState
  { interpreterOpts :: InterpreterOpts
  , definitions     :: HashMap Text CheckExpr
  }

defaultAppState :: AppState
defaultAppState = AppState
  { interpreterOpts = defaultInterpreterOpts
  , definitions = mempty
  }

data InterpreterOpts = InterpreterOpts
  { traceOpts     :: TraceOpts
  , printTypeOpts :: PrintTypeOpts
  }

defaultInterpreterOpts :: InterpreterOpts
defaultInterpreterOpts = InterpreterOpts
  { traceOpts     = TraceOff
  , printTypeOpts = PrintOff
  }

data TraceOpts
  -- | Print the entire expression in traces.
  = TraceGlobal
  -- | Print only the modified part of the expression in traces.
  | TraceLocal
  -- | Do not trace evaluation.
  | TraceOff

data PrintTypeOpts
  -- | Do not print the inferred type of any expressions.
  = PrintOff
  -- | Print the inferred type of top-level declarations.
  | PrintDecls
  -- | Print the inferred type of top-level expressions.
  | PrintExprs
  -- | Print to STDERR the inferred types of
  -- both top-level declarations and top-level expressions.
  | PrintBoth
  deriving Eq

shouldPrintType :: Bool -> InterpreterOpts -> Bool
shouldPrintType isDecl opts = case printTypeOpts opts of
  PrintBoth  -> True
  PrintDecls -> isDecl
  PrintExprs -> not isDecl
  PrintOff   -> False
