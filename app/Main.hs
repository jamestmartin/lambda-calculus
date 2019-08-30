module Main where

import Control.Monad (forever)
import Data.Type.Nat (Nat (Z))
import System.IO (hFlush, stdout)
import LambdaCalculus.Evaluation.Optimal (eval)
import LambdaCalculus.Parser (parse)
import LambdaCalculus.Representation (convert)
import LambdaCalculus.Representation.Dependent.ReverseDeBruijn (Expression)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

main :: IO ()
main = forever $ parse "stdin" <$> prompt ">> " >>= \case
  Left parseError -> putStrLn $ "Parse error: " ++ show parseError
  Right expr -> do print expr; print $ eval (convert expr :: Expression 'Z)
