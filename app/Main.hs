module Main (main) where

import Ivo

import Control.Exception (IOException, catch)
import Data.Maybe (isJust)
import Control.Monad (when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Loops (whileJust_)
import Control.Monad.State (MonadState, StateT, evalStateT, gets, modify)
import Control.Monad.Trans (lift)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import System.Console.Haskeline
  ( InputT, runInputT, defaultSettings
  , outputStrLn, getInputLine, handleInterrupt, withInterrupt
  )
import Text.Parsec
import Text.Parsec.Text (Parser)

outputTextLn :: MonadIO m => Text -> InputT m ()
outputTextLn = outputStrLn . T.unpack

-- | Immediately quit the program when interrupted
-- without performing any additional actions.
-- (Without this, it will print an extra newline for some reason.)
justDie :: (MonadIO m, MonadMask m) => InputT m () -> InputT m ()
justDie = handleInterrupt (pure ()) . withInterrupt

data AppState = AppState
  { traceOptions :: TraceOptions
  , checkOptions :: CheckOptions
  , definitions  :: HashMap Text CheckExpr
  }

data TraceOptions
  -- | Print the entire expression in traces.
  = TraceGlobal
  -- | Print only the modified part of the expression in traces.
  | TraceLocal
  -- | Do not trace evaluation.
  | TraceOff

data CheckOptions = CheckOptions
  -- | Require that an expression typechecks to run it.
  { shouldTypecheck :: Bool
  -- | Print the inferred type of an expressions.
  , shouldPrintType :: CheckPrintOptions
  }

data CheckPrintOptions = PrintAlways | PrintDecls | PrintOff
  deriving Eq

shouldPrintTypeErrorsQ :: Bool -> CheckOptions -> Bool
shouldPrintTypeErrorsQ isDecl opts
  =  shouldTypecheck opts
  || shouldPrintTypeQ isDecl opts

shouldPrintTypeQ :: Bool -> CheckOptions -> Bool
shouldPrintTypeQ isDecl opts
  =  shouldPrintType opts == PrintAlways
  || shouldPrintType opts == PrintDecls && isDecl

defaultAppState :: AppState
defaultAppState = AppState
  { traceOptions = TraceOff
  , checkOptions = CheckOptions
    { shouldTypecheck = True
    , shouldPrintType = PrintDecls
    }
  , definitions = HM.empty
  }

data Command
  = Trace TraceOptions
  | Check CheckOptions
  | Load FilePath
  | Clear

commandParser :: Parser Command
commandParser = do
  char ':'
  trace <|> check <|> load <|> clear
  where
    trace = Trace <$> do
      try $ string "trace "
      try traceOff <|> try traceLocal <|> try traceGlobal
    traceOff = TraceOff <$ string "off"
    traceLocal = TraceLocal <$ string "local"
    traceGlobal = TraceGlobal <$ string "global"

    check = Check <$> do
      try $ string "check "
      spaces
      tc <- (True <$ try (string "on ")) <|> (False <$ try (string "off "))
      spaces
      pr <- try printAlways <|> try printDecls <|> try printOff
      pure $ CheckOptions tc pr
    printAlways = PrintAlways <$ string "always"
    printDecls = PrintDecls <$ string "decls"
    printOff = PrintOff <$ string "off"

    load = Load <$> do
      try $ string "load "
      spaces
      filename <- many1 (noneOf " ")
      spaces
      pure filename

    clear = Clear <$ try (string "clear")

class MonadState AppState m => MonadApp m where
  parsed        :: Either ParseError a -> m a
  typecheckDecl :: Maybe Type -> Text -> CheckExpr -> m (Maybe Scheme)
  typecheckExpr ::                       CheckExpr -> m (Maybe Scheme)
  execute       :: CheckExpr -> m EvalExpr

type AppM = ExceptT Text (StateT AppState (InputT IO))

liftInput :: InputT IO a -> AppM a
liftInput = lift . lift

instance MonadApp AppM where
  parsed (Left err) = throwError $ T.pack $ show err
  parsed (Right ok) = pure ok

  typecheckDecl ty = typecheck ty    . Just
  typecheckExpr    = typecheck Nothing Nothing

  execute checkExpr = do
    defs <- gets definitions
    let expr = check2eval $ substitute defs checkExpr
    traceOpts <- gets traceOptions
    case traceOpts of
      TraceOff -> do
        let value = eval expr
        liftInput $ outputTextLn $ unparseEval value
        pure value
      TraceLocal -> do
        let (value, trace) = evalTrace expr
        liftInput $ mapM_ (outputTextLn . unparseEval) trace
        pure value
      TraceGlobal -> do
        let (value, trace) = evalTraceGlobal expr
        liftInput $ mapM_ (outputTextLn . unparseEval) trace
        pure value

typecheck :: Maybe Type -> Maybe Text -> CheckExpr -> AppM (Maybe Scheme)
typecheck tann decl expr = do
  defs <- gets definitions
  let type_ = maybe infer check tann $ substitute defs expr
  checkOpts <- gets checkOptions
  if shouldTypecheck checkOpts
    then case type_ of
      Left err -> throwError $ "Typecheck error: " <> err
      Right t -> do
        printType checkOpts t
        pure $ Just t
    else do
      case type_ of
        Left err ->
          when (shouldPrintTypeErrorsQ isDecl checkOpts) $
            liftInput $ outputStrLn $ "Typecheck error: " <> T.unpack err
        Right t -> printType checkOpts t

      pure Nothing
  where
    isDecl = isJust decl

    printType opts t =
      when (shouldPrintTypeQ isDecl opts) $
        liftInput $ outputTextLn $ prefix <> unparseScheme t

    prefix = case decl of
      Just name -> name <> " : "
      Nothing -> ": "

define :: MonadApp m => Text -> CheckExpr -> m ()
define name expr = modify \appState ->
  let expr' = substitute (definitions appState) expr
  in appState { definitions = HM.insert name expr' $ definitions appState }

runDeclOrExpr :: MonadApp m => DeclOrExprAST -> m ()
runDeclOrExpr (Left (name, ty, exprAST)) = do
  defs <- gets definitions
  let expr = substitute defs $ ast2check exprAST
  _ <- typecheckDecl ty name expr
  define name expr
runDeclOrExpr (Right exprAST) = do
  defs <- gets definitions
  let expr = substitute defs $ ast2check exprAST
  _ <- typecheckExpr expr
  _ <- execute expr
  pure ()

runProgram :: MonadApp m => ProgramAST -> m ()
runProgram = mapM_ runDeclOrExpr

runCommand :: forall m. (MonadApp m, MonadIO m, MonadError Text m) => Command -> m ()
runCommand (Trace traceOpts) = modify \app -> app { traceOptions = traceOpts }
runCommand (Check checkOpts) = modify \app -> app { checkOptions = checkOpts }
runCommand Clear = modify \app -> app { definitions = HM.empty }
runCommand (Load filePath) = do
  input <- safeReadFile
  program <- parsed $ parse programParser filePath input
  runProgram program
  where
    safeReadFile :: m Text
    safeReadFile = liftEither =<< liftIO (
      (Right . T.pack <$> readFile filePath)
      `catch` handleException)

    handleException :: IOException -> IO (Either Text Text)
    handleException = pure . Left . T.pack . show

parseCommandOrDeclOrExpr :: MonadApp m => Text -> m (Either Command DeclOrExprAST)
parseCommandOrDeclOrExpr input = parsed $ parse commandOrDeclOrExprParser "input" input
  where
    commandOrDeclOrExprParser =
      (Left <$> try commandParser) <|> (Right <$> declOrExprParser) <* spaces <* eof

main :: IO ()
main = runInputT defaultSettings $ justDie $ flip evalStateT defaultAppState $
  whileJust_ (fmap T.pack <$> lift (getInputLine ">> ")) \inputText ->
    handleErrors do
      input <- parseCommandOrDeclOrExpr inputText
      either runCommand runDeclOrExpr input
  where
    handleErrors :: ExceptT Text (StateT AppState (InputT IO)) () -> StateT AppState (InputT IO) ()
    handleErrors m = do
      result <- runExceptT m
      case result of
        Left err -> lift $ outputTextLn err
        Right _ -> pure ()
