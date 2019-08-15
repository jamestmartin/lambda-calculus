module Main where

import Control.Monad (forever)
import System.IO (hFlush, stdout)
import Text.Parsec (parse)
import UntypedLambdaCalculus (eval, canonym)
import UntypedLambdaCalculus.Parser (expr)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

-- (\D F I. D (F I)) (\x. x x) (\f. f (f y)) (\x. x)
main :: IO ()
main = forever $ do
  expr <- parse expr "stdin" <$> prompt ">> "
  case expr of
    Left parseError -> putStrLn $ "Parse error: " ++ show parseError
    Right expr -> do
      print $ eval [] $ canonym expr
  
