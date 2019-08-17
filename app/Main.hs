module Main where

import Control.Monad (forever)
import System.IO (hFlush, stdout)
import Text.Parsec (parse)
import UntypedLambdaCalculus (eval)
import UntypedLambdaCalculus.Parser (expr)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

main :: IO ()
main = forever $ do
  input <- expr "stdin" <$> prompt ">> "
  case input of
    Left parseError -> putStrLn $ "Parse error: " ++ show parseError
    Right ast -> print $ eval ast
