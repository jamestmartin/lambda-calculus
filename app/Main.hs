module Main (main) where

import Control.Monad (forever)
import Data.Text
import Data.Text.IO qualified as TIO
import LambdaCalculus (eval)
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
  Right expr -> print $ eval expr
