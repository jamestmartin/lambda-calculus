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
variableName = T.pack <$> many1 letter

variable :: Parser Expression
variable = Variable <$> variableName

application :: Parser Expression
application = foldl1 Application <$> sepBy1 applicationTerm spaces
  where applicationTerm :: Parser Expression
        applicationTerm = variable <|> abstraction <|> grouping
          where grouping :: Parser Expression
                grouping = between (char '(') (char ')') expression

abstraction :: Parser Expression
abstraction = do
  char '^'
  names <- sepBy1 variableName spaces
  char '.'
  body <- expression
  pure $ foldr Abstraction body names

expression :: Parser Expression
expression = abstraction <|> application <|> variable

parseExpression :: Text -> Either ParseError Expression
parseExpression code = parse (expression <* eof) "input" code
