module LambdaCalculus.Syntax.Parser
  ( ParseError
  , parseAST
  ) where

import LambdaCalculus.Syntax.Base

import Data.List.NonEmpty (fromList)
import Data.Text qualified as T
import Text.Parsec hiding (label, token)
import Text.Parsec qualified
import Text.Parsec.Text (Parser)

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
    _ <- string $ T.unpack kwd
    notFollowedBy letter
  spaces

-- | An identifier is a sequence of letters which is not a keyword.
identifier :: Parser Text
identifier = label "identifier" $ do
    notFollowedBy anyKeyword
    T.pack <$> (many1 letter <* spaces)
  where anyKeyword = choice $ map keyword keywords

variable :: Parser AST
variable = label "variable" $ Var <$> identifier

many1' :: Parser a -> Parser (NonEmpty a)
many1' p = fromList <$> many1 p

many2 :: Parser a -> Parser (a, NonEmpty a)
many2 p = (,) <$> p <*> many1' p

grouping :: Parser AST
grouping = label "grouping" $ between (token '(') (token ')') expression

application :: Parser AST
application = uncurry App <$> many2 applicationTerm
  where applicationTerm = abstraction <|> let_ <|> grouping <|> variable

abstraction :: Parser AST
abstraction = label "lambda abstraction" $ Abs <$> between lambda (token '.') (many1' identifier) <*> expression
  where lambda = label "lambda" $ (char '\\' <|> char 'λ') *> spaces

let_ :: Parser AST
let_ = Let <$> between (keyword "let") (keyword "in") (fromList <$> definitions) <*> expression
  where
    definitions :: Parser [Def Parse]
    definitions = flip sepBy1 (token ';') do
      name <- identifier
      token '='
      value <- expression
      pure (name, value)

expression :: Parser AST
expression = label "expression" $ abstraction <|> let_ <|> try application <|> grouping <|> variable

parseAST :: Text -> Either ParseError AST
parseAST = parse (spaces *> expression <* eof) "input"
