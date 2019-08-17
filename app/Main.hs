{-# LANGUAGE BlockArguments, LambdaCase #-}
module Main where

import Control.Monad (forever)
import System.IO (hFlush, stdout)
import UntypedLambdaCalculus (eval)
import UntypedLambdaCalculus.Parser (parseExpr)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

main :: IO ()
main = forever $ parseExpr "stdin" <$> prompt ">> " >>= \case
  Left parseError -> putStrLn $ "Parse error: " ++ show parseError
  Right expr -> print $ eval expr
