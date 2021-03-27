module Main (main) where

import Command
import Flags
import MonadApp

import Ivo

import Control.Exception (IOException, catch)
import Control.Monad (when, zipWithM)
import Control.Monad.Except (throwError, liftEither)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (whileJust_)
import Control.Monad.State (gets, modify')
import Control.Monad.Trans (lift)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (isJust, fromJust)
import Data.Text qualified as T
import Data.Text.IO (readFile)
import Prelude hiding (readFile)
import System.Console.Haskeline (getInputLine)

main :: IO ()
main = do
  action <- getAction
  case action of
    PrintHelp -> putStrLn usageMessage
    PrintVersion -> putStrLn "Ivo 0.1.0.0"
    Interpreter ProgramOpts { pInterpreterOpts, loadFiles, mainFile } -> do
      -- read the file contents first so we can print errors right away
      mMainContents <- mapM readArgFile mainFile
      filesContents <- mapM readArgFile loadFiles
      runAppM pInterpreterOpts do
        -- then parse
        mMain <- mapM (parseProgramHandleErrors $ fromJust mainFile) mMainContents
        case mMain of
          Just mainAST
            | any (\(name, _, _) -> name == "main") mainAST -> pure ()
            | otherwise -> throwError "File passed to `-c` does not contain a main function."
          Nothing -> pure ()

        files <- zipWithM parseProgramHandleErrors loadFiles filesContents
        -- and only finally interpret
        mapM_ loadFile files
        maybe repl runProgram mMain

  where
    -- | When reading the file contents of a file passed as an argument,
    -- we want to print usage information and exit if the file can't be opened.
    readArgFile :: FilePath -> IO Text
    readArgFile file = readFile file `catch` handleException
      where
        handleException :: IOException -> IO a
        handleException _ = ioError $ userError $
          "Could not open file: " ++ file ++ "\n" ++ usageMessage

repl :: AppM ()
repl = lift $ whileJust_ (fmap T.pack <$> lift (getInputLine ">> ")) \inputText ->
  handleErrors do
    input <- parseCommandOrTopLevel inputText
    either runCommand runTopLevel input

parseCommandOrTopLevel :: Text -> AppM (Either Command TopLevelAST)
parseCommandOrTopLevel input = do
  mCmd <- liftParseError $ parseCommand input
  case mCmd of
    Nothing -> Right <$> liftParseError (parseTopLevel input)
    Just cmd -> pure $ Left cmd

parseProgramHandleErrors :: FilePath -> Text -> AppM ProgramAST
parseProgramHandleErrors filename = liftParseError . parse programParser filename

liftParseError :: Either ParseError a -> AppM a
liftParseError result = case result of
  Left err -> throwError $ T.pack $ show err
  Right x -> pure x

runProgram :: ProgramAST -> AppM ()
runProgram program = do
  loadFile program
  runDeclOrExpr (Right (Var "main"))

loadFile :: ProgramAST -> AppM ()
loadFile = mapM_ (\(name, ty, e) -> define name ty $ ast2check e)

runTopLevel :: TopLevelAST -> AppM ()
runTopLevel = mapM_ runDeclOrExpr

runDeclOrExpr :: Either Declaration AST -> AppM ()
runDeclOrExpr (Left (name, ty, exprAST)) = do
  defs <- gets definitions
  let expr = substitute defs $ ast2check exprAST
  _ <- typecheckDecl ty name expr
  define name ty expr
runDeclOrExpr (Right exprAST) = do
  defs <- gets definitions
  let expr = substitute defs $ ast2check exprAST
  _ <- typecheckExpr expr
  value <- execute expr
  liftInput $ outputTextLn $ unparseEval value
  pure ()

typecheckDecl :: Maybe Type -> Text -> CheckExpr -> AppM Scheme
typecheckDecl ty = typecheck ty . Just

typecheckExpr :: CheckExpr-> AppM Scheme
typecheckExpr = typecheck Nothing Nothing

typecheck :: Maybe Type -> Maybe Text -> CheckExpr -> AppM Scheme
typecheck tann decl expr = do
  defs <- gets definitions
  let type_ = maybe infer check tann $ substitute defs expr
  case type_ of
    Left err -> throwError $ "Typecheck error: " <> err
    Right t -> do
      printTypeB <- gets $ shouldPrintType isDecl . interpreterOpts
      when printTypeB $ outputStderrLn $ prefix <> unparseScheme t
      pure t
  where
    isDecl = isJust decl

    prefix = case decl of
      Just name -> name <> " : "
      Nothing -> ": "

execute :: CheckExpr -> AppM EvalExpr
execute checkExpr = do
  defs <- gets definitions
  let expr = check2eval $ substitute defs checkExpr
  traceOpts <- gets (traceOpts . interpreterOpts)
  case traceOpts of
    TraceOff -> do
      let value = eval expr
      pure value
    TraceLocal -> do
      let (value, trace) = evalTrace expr
      mapM_ (outputStderrLn . unparseEval) trace
      pure value
    TraceGlobal -> do
      let (value, trace) = evalTraceGlobal expr
      mapM_ (outputStderrLn . unparseEval) trace
      pure value

define :: Text -> Maybe Type -> CheckExpr -> AppM ()
define name ty expr = do
  _ <- typecheckDecl ty name expr
  modify' \appState ->
    let expr' = substitute (definitions appState) expr
    in appState { definitions = HM.insert name expr' $ definitions appState }

modifyInterpreterOpts :: (InterpreterOpts -> InterpreterOpts) -> AppM ()
modifyInterpreterOpts f =
  modify' \app -> app { interpreterOpts = f (interpreterOpts app) }

runCommand :: Command -> AppM ()
runCommand (Trace traceOpts) =
  modifyInterpreterOpts \opts -> opts { traceOpts }
runCommand (PrintType printTypeOpts) =
  modifyInterpreterOpts \opts -> opts { printTypeOpts }
runCommand Clear = modify' \app -> app { definitions = HM.empty }
runCommand (Load filePath) = do
  input <- safeReadFile
  program <- liftParseError $ parse programParser filePath input
  runProgram program
  where
    safeReadFile :: AppM Text
    safeReadFile = liftEither =<< liftIO (
      (Right <$> readFile filePath)
      `catch` handleException)

    handleException :: IOException -> IO (Either Text Text)
    handleException = pure . Left . T.pack . show
