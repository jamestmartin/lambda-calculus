module LambdaCalculus.Parser
    ( parseAST, parseExpression
    ) where

import Control.Applicative ((*>))
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import LambdaCalculus.Expression (Expression, ast2expr)
import qualified LambdaCalculus.Expression as Expr
import LambdaCalculus.Parser.AbstractSyntax
import Text.Parsec hiding (spaces)
import Text.Parsec.Text
import TextShow

keywords :: [Text]
keywords = ["let", "in"]

-- | A keyword is an exact string which is not part of an identifier.
keyword :: Text -> Parser ()
keyword kwd = do
    void $ string (T.unpack kwd)
    notFollowedBy letter

-- | An identifier is a sequence of letters which is not a keyword.
identifier :: Parser Identifier
identifier = do
    notFollowedBy anyKeyword
    T.pack <$> many1 letter
  where anyKeyword = choice $ map (try . keyword) keywords

variable :: Parser AbstractSyntax
variable = Variable <$> identifier

spaces :: Parser ()
spaces = skipMany1 space

application :: Parser AbstractSyntax
application = Application <$> sepEndBy1 applicationTerm spaces
  where applicationTerm :: Parser AbstractSyntax
        applicationTerm = abstraction <|> grouping <|> let_ <|> variable
          where grouping :: Parser AbstractSyntax
                grouping = between (char '(') (char ')') expression

abstraction :: Parser AbstractSyntax
abstraction = do
    char '\\' <|> char 'λ' ; optional spaces
    names <- sepEndBy1 identifier spaces
    char '.'
    Abstraction names <$> expression

let_ :: Parser AbstractSyntax
let_ = do
    try (keyword "let") ; spaces
    defs <- sepBy1 definition (char ';' *> optional spaces)
    keyword "in"
    Let defs <$> expression
  where definition :: Parser Definition
        definition = do
            name <- identifier ; optional spaces
            char '='
            value <- expression
            pure (name, value)

expression :: Parser AbstractSyntax
expression = optional spaces *> (abstraction <|> let_ <|> application <|> variable) <* optional spaces

parseAST :: Text -> Either ParseError AbstractSyntax
parseAST = parse (expression <* eof) "input"

parseExpression :: Text -> Either ParseError Expression
parseExpression = fmap ast2expr . parseAST
