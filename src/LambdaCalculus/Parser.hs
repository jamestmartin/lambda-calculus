module LambdaCalculus.Parser (parse) where

import Control.Applicative (liftA2)
import Control.Monad (void)
import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import LambdaCalculus.Representation.AbstractSyntax

-- | Parse a keyword, unambiguously not a variable name.
keyword :: String -> Parser ()
keyword kwd = try $ do
  void $ string kwd
  -- TODO: rephrase this in terms of `extension`
  notFollowedBy alphaNum

-- | The extension of a variable name.
-- The first letter of a variable name must be a letter,
-- but the rest of the variable name may be more general.
extension :: Parser String
extension = many alphaNum

-- | A variable name, e.g. `x`, `foo`, `f2`, `fooBar27`.
name :: Parser String
name = do
  notFollowedBy anyKeyword
  liftA2 (:) letter extension
  where
    anyKeyword = choice $ map keyword keywords
      where
        -- | Keywords that are forbidden from use as variable names.
        keywords = ["let", "in"]

-- | A variable expression.
variable :: Parser Expression
variable = Variable <$> name

-- | A lambda abstraction.
abstraction :: Parser Expression
abstraction = do
  char 'λ' <|> char '\\'    ; spaces
  variables <- variableList ; spaces
  char '.'                  ; spaces
  body <- expression
  return $ Abstraction variables body
  where variableList :: Parser [String]
        variableList = name `sepBy` spaces

-- | A function application.
application :: Parser Expression
application = Application <$> applicationTerm `sepEndBy` spaces
  where
    -- | An application term is any expression which consumes input,
    -- i.e. anything other than an ungrouped application.
    applicationTerm :: Parser Expression
    applicationTerm = let_ <|> variable <|> abstraction <|> grouping
      where
        -- | An expression grouped by parentheses.
        grouping :: Parser Expression
        grouping = between (char '(' >> spaces) (spaces >> char ')') expression

-- | A `let` expression.
let_ :: Parser Expression
let_ = do
  keyword "let"       ; spaces
  variable <- name    ; spaces
  char '='            ; spaces
  value <- expression ; spaces
  string "in"         ; spaces
  body <- expression
  return $ Let variable value body

-- | Any expression.
expression :: Parser Expression
expression = application

-- | Parse a lambda calculus expression.
parse :: SourceName -> String -> Either ParseError Expression
parse = Parsec.parse (expression <* eof)
