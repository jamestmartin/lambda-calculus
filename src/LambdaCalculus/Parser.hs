module LambdaCalculus.Parser (parseExpression) where

import Control.Applicative ((*>))
import Control.Monad (void)
import LambdaCalculus.Expression (Expression (Variable, Application, Abstraction))
import Text.Parsec hiding (spaces)
import Text.Parsec.String

spaces :: Parser ()
spaces = void $ many1 space

variableName :: Parser String
variableName = many1 letter

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

parseExpression :: String -> Either ParseError Expression
parseExpression = parse (expression <* eof) "input"
