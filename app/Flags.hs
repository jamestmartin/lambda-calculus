module Flags where

import MonadApp

import Data.Foldable (foldlM)
import Data.Maybe (isJust)
import System.Console.GetOpt
  ( OptDescr (Option), ArgDescr (NoArg, OptArg, ReqArg), ArgOrder (Permute)
  , getOpt, usageInfo
  )
import System.Environment (getArgs)

data Flag
  = FlagHelp
  | FlagVersion
  | FlagProgram String
  | FlagPrintTypes (Maybe String)
  | FlagTrace (Maybe String)

flagDescrs :: [OptDescr Flag]
flagDescrs =
  [ Option ['h'] ["help"]        (NoArg  FlagHelp)
    "Print to STDOUT this help message and exit."
  , Option ['V'] ["version"]     (NoArg  FlagVersion)
    "Print to STDOUT the Ivo version number and exit."
  , Option ['c'] []              (ReqArg FlagProgram    "FILE")
    "After loading all other files, execute the `main` function of this file and exit."
  , Option ['T'] ["print-types"] (OptArg FlagPrintTypes "decls|exprs|both")
    "Print to STDERR the inferred types for top-level declarations and/or expressions."
  , Option ['t'] ["trace"]       (OptArg FlagTrace      "local|global")
    "Print to STDERR each evaluation step, either the analyzed portion of, or the entire, expression."
  ]

data ProgramAction
  = PrintHelp
  | PrintVersion
  | Interpreter ProgramOpts

defaultProgramAction :: ProgramAction
defaultProgramAction = Interpreter defaultProgramOpts

data ProgramOpts = ProgramOpts
  { pInterpreterOpts :: InterpreterOpts
  -- | The files which will be interpreted.
  , loadFiles        :: [FilePath]
  -- | The file whose `main` function will be executed.
  , mainFile         :: Maybe FilePath
  }

defaultProgramOpts :: ProgramOpts
defaultProgramOpts = ProgramOpts
  { pInterpreterOpts = defaultInterpreterOpts
  , loadFiles        = []
  , mainFile         = Nothing
  }

-- | Take all the flags and files passed to the program,
-- and determine what action the program should take based on them.
foldFlags :: [Flag] -> [String] -> Either String ProgramAction
foldFlags flags files =
  foldlM applyFlag (Interpreter (defaultProgramOpts { loadFiles = files })) flags
  where
    applyFlag :: ProgramAction -> Flag -> Either String ProgramAction
    applyFlag PrintHelp _        = Right PrintHelp
    applyFlag _         FlagHelp = Right PrintHelp
    applyFlag PrintVersion _     = Right PrintVersion
    applyFlag _      FlagVersion = Right PrintVersion
    applyFlag (Interpreter opts) flag = Interpreter <$> applyInterpreterFlag flag
      where
        applyInterpreterFlag :: Flag -> Either String ProgramOpts
        applyInterpreterFlag (FlagProgram file)
          | isJust (mainFile opts) = Left "-c can only be specified once"
          | otherwise              = Right $ opts { mainFile = Just file }
        applyInterpreterFlag (FlagPrintTypes mOpt) = case readPrintTypeOpt mOpt of
          Left opt -> Left $ "unknown --print-types option: " ++ opt ++ "\n"
          Right opt -> Right
            opts { pInterpreterOpts = (pInterpreterOpts opts) { printTypeOpts = opt } }
        applyInterpreterFlag (FlagTrace mOpt) = case readTraceOpt mOpt of
          Left opt -> Left $ "unknown --trace option: " ++ opt ++ "\n"
          Right opt -> Right
            opts { pInterpreterOpts = (pInterpreterOpts opts) { traceOpts = opt } }
        applyInterpreterFlag _ = error "Illegal flag"

    readPrintTypeOpt :: Maybe String -> Either String PrintTypeOpts
    readPrintTypeOpt Nothing        = Right PrintDecls
    readPrintTypeOpt (Just "decls") = Right PrintDecls
    readPrintTypeOpt (Just "exprs") = Right PrintExprs
    readPrintTypeOpt (Just "both")  = Right PrintBoth
    readPrintTypeOpt (Just opt)     = Left  opt

    readTraceOpt :: Maybe String -> Either String TraceOpts
    readTraceOpt Nothing         = Right TraceLocal
    readTraceOpt (Just "local")  = Right TraceLocal
    readTraceOpt (Just "global") = Right TraceGlobal
    readTraceOpt (Just opt)      = Left  opt

-- | Given a list of arguments as raw strings, determine what the program should do.
decideAction :: [String] -> Either [String] ProgramAction
decideAction args = case getOpt Permute flagDescrs args of
  (flags, files, errs) -> case foldFlags flags files of
    Left err            -> Left $ err : errs
    Right action
      | not (null errs) -> Left errs
      | otherwise       -> Right action

usageMessage :: String
usageMessage = usageInfo "Usage: ivo [OPTION...] [files...]" flagDescrs

-- | Read the program's arguments and decide what to do.
getAction :: IO ProgramAction
getAction = do
  mAction <- decideAction <$> getArgs
  case mAction of
    Left errs -> ioError $ userError $ concat errs ++ usageMessage
    Right action -> pure action
