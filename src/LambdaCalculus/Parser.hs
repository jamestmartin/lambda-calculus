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
import Text.Parsec hiding (label, token)
import Text.Parsec qualified
import Text.Parsec.Text (Parser)
import TextShow

label :: String -> Parser a -> Parser a
label = flip Text.Parsec.label

token :: Char -> Parser ()
token ch = label [ch] $ char ch *> spaces

keywords :: [Text]
keywords = ["let", "in"]

-- | A keyword is an exact string which is not part of an identifier.
keyword :: Text -> Parser ()
keyword kwd = label (T.unpack kwd) $ do
    try do
      string $ T.unpack kwd
      notFollowedBy letter
    spaces

-- | An identifier is a sequence of letters which is not a keyword.
identifier :: Parser Identifier
identifier = label "identifier" $ do
    notFollowedBy anyKeyword
    T.pack <$> (many1 letter <* spaces)
  where anyKeyword = choice $ map keyword keywords

variable :: Parser AbstractSyntax
variable = label "variable" $ Variable <$> identifier

many2 :: Parser a -> Parser [a]
many2 p = (:) <$> p <*> many1 p

grouping :: Parser AbstractSyntax
grouping = label "grouping" $ between (token '(') (token ')') expression

application :: Parser AbstractSyntax
application = Application <$> many2 applicationTerm
  where applicationTerm :: Parser AbstractSyntax
        applicationTerm = abstraction <|> let_ <|> grouping <|> variable

abstraction :: Parser AbstractSyntax
abstraction = label "lambda abstraction" $ Abstraction <$> between lambda (token '.') (many1 identifier) <*> expression
    where lambda = label "lambda" $ (char '\\' <|> char 'λ') *> spaces

let_ :: Parser AbstractSyntax
let_ = Let <$> between (keyword "let") (keyword "in") definitions <*> expression
  where definitions :: Parser [Definition]
        definitions = flip sepBy1 (token ';') do
            name <- identifier
            token '='
            value <- expression
            pure (name, value)

expression :: Parser AbstractSyntax
expression = label "expression" $ abstraction <|> let_ <|> try application <|> grouping <|> variable

parseAST :: Text -> Either ParseError AbstractSyntax
parseAST = parse (spaces *> expression <* eof) "input"

parseExpression :: Text -> Either ParseError Expression
parseExpression = fmap ast2expr . parseAST
