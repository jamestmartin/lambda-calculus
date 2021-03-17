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
main = forever $ parseEval <$> prompt ">> " >>= \case
  Left parseError -> putStrLn $ "Parse error: " <> pack (show parseError)
  -- TODO: Support choosing which version to use at runtime.
  Right expr -> putStrLn $ unparseEval $ eval expr
  --Right expr -> mapM_ (putStrLn . unparseEval) $ snd $ traceEval expr
