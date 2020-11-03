{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad (forever)
import Data.Text
import qualified Data.Text.IO as TIO
import LambdaCalculus (eagerEval)
import LambdaCalculus.Parser (parseExpression)
import System.IO (hFlush, stdout)

prompt :: Text -> IO Text
prompt text = do
  TIO.putStr text
  hFlush stdout
  TIO.getLine

main :: IO ()
main = forever $ parseExpression <$> prompt ">> " >>= \case
  Left parseError -> putStrLn $ "Parse error: " ++ show parseError
  Right expr -> print $ eagerEval expr
