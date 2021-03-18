module Main (main) where

import LambdaCalculus

import Control.Monad (forever)
import Data.Text (pack)
import Data.Text.IO
import Prelude hiding (putStr, putStrLn, getLine)
import System.IO (hFlush, stdout)

prompt :: Text -> IO Text
prompt text = do
  putStr text
  hFlush stdout
  getLine

main :: IO ()
main = forever $ parseCheck <$> prompt ">> " >>= \case
  Left parseError -> putStrLn $ "Parse error: " <> pack (show parseError)
  -- TODO: Support choosing which version to use at runtime.
  Right expr -> do
    either putStrLn (putStrLn . (": " <>) . unparseScheme) $ infer expr
    putStrLn $ unparseEval $ eval $ check2eval expr
    --mapM_ (putStrLn . unparseEval) $ snd $ traceEval $ check2eval expr
