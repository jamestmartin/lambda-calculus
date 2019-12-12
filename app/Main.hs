{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad (forever)
import LambdaCalculus.Expression (lazyEval)
import LambdaCalculus.Parser (parseExpression)
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

main :: IO ()
main = forever $ parseExpression <$> prompt ">> " >>= \case
  Left parseError -> putStrLn $ "Parse error: " ++ show parseError
  Right expr -> print $ lazyEval expr
