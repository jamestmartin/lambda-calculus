module LambdaCalculus.Parser (parseExpression) where

import Control.Applicative ((*>))
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import LambdaCalculus.Expression
import Text.Parsec hiding (spaces)
import Text.Parsec.Text

keywords :: [Text]
keywords = ["let", "in"]

-- | A keyword is an exact string which is not part of an identifier.
keyword :: Text -> Parser ()
keyword kwd = do
    void $ string (T.unpack kwd)
    notFollowedBy letter

-- | An identifier is a sequence of letters which is not a keyword.
identifier :: Parser Text
identifier = do
    notFollowedBy anyKeyword
    T.pack <$> many1 letter
  where anyKeyword = choice $ map (try . keyword) keywords

variable :: Parser Expression
variable = Variable <$> identifier

spaces :: Parser ()
spaces = skipMany1 space

application :: Parser Expression
application = foldl1 Application <$> sepEndBy1 applicationTerm spaces
  where applicationTerm :: Parser Expression
        applicationTerm = abstraction <|> grouping <|> let_ <|> variable
          where grouping :: Parser Expression
                grouping = between (char '(') (char ')') expression

abstraction :: Parser Expression
abstraction = do
    char '\\' <|> char 'λ' ; optional spaces
    names <- sepEndBy1 identifier spaces
    char '.'
    body <- expression
    pure $ foldr Abstraction body names

let_ :: Parser Expression
let_ = do
    try (keyword "let") ; spaces
    defs <- sepBy1 definition (char ';' *> optional spaces)
    keyword "in"
    body <- expression
    pure $ foldr (uncurry letExpr) body defs
  where definition :: Parser (Text, Expression)
        definition = do
            name <- identifier ; optional spaces
            char '='
            value <- expression
            pure (name, value)
        letExpr :: Text -> Expression -> Expression -> Expression
        letExpr name value body = Application (Abstraction name body) value

expression :: Parser Expression
expression = optional spaces *> (abstraction <|> let_ <|> application <|> variable) <* optional spaces

parseExpression :: Text -> Either ParseError Expression
parseExpression = parse (expression <* eof) "input"
