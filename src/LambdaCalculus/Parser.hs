module LambdaCalculus.Parser (parseExpression) where

import Control.Applicative ((*>))
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import LambdaCalculus.Expression
import Text.Parsec hiding (spaces)
import Text.Parsec.Text

spaces :: Parser ()
spaces = void $ many1 space

variableName :: Parser Text
variableName = do
  notFollowedBy anyKeyword
  T.pack <$> many1 letter
  where anyKeyword = choice $ map keyword keywords
        keywords = ["let", "in"]

-- | Match an exact string which is not just a substring
-- of a larger variable name.
keyword :: Text -> Parser ()
keyword kwd = try $ do
  void $ string (T.unpack kwd)
  notFollowedBy letter

variable :: Parser Expression
variable = Variable <$> variableName

application :: Parser Expression
application = foldl1 Application <$> sepEndBy1 applicationTerm spaces
  where applicationTerm :: Parser Expression
        applicationTerm = variable <|> abstraction <|> let_ <|> grouping
          where grouping :: Parser Expression
                grouping = between (char '(') (char ')') expression

abstraction :: Parser Expression
abstraction = do
  char '\\'
  optional spaces
  names <- sepEndBy1 variableName spaces
  char '.'
  optional spaces
  body <- expression
  pure $ foldr Abstraction body names

let_ :: Parser Expression
let_ = do
  keyword "let"
  name <- between spaces (optional spaces) variableName
  char '='
  value <- expression
  keyword "in"
  body <- expression
  pure $ Application (Abstraction name body) value

expression :: Parser Expression
expression = optional spaces *> (abstraction <|> let_ <|> application <|> variable) <* optional spaces

parseExpression :: Text -> Either ParseError Expression
parseExpression = parse (expression <* eof) "input"
