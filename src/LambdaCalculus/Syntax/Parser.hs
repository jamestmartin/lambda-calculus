module LambdaCalculus.Syntax.Parser
  ( ParseError, parse
  , DeclOrExprAST, ProgramAST
  , parseAST, parseDeclOrExpr, parseProgram
  , astParser, declOrExprParser, programParser
  ) where

import LambdaCalculus.Syntax.Base

import Data.List.NonEmpty (fromList)
import Data.Text qualified as T
import Prelude hiding (succ, either)
import Text.Parsec hiding (label, token, spaces)
import Text.Parsec qualified
import Text.Parsec.Text (Parser)

spaces :: Parser ()
spaces = Text.Parsec.spaces >> optional (try (comment >> spaces))
  where
    comment, lineComment, blockComment :: Parser ()
    comment = blockComment <|> lineComment
    lineComment = label "line comment" $ do
      _ <- try (string "//")
      _ <- many1 (noneOf "\n")
      pure ()
    blockComment = label "block comment" $ do
      _ <- try (string "/*")
      _ <- many1 $ notFollowedBy (string "*/") >> anyChar
      _ <- string "*/"
      pure ()

label :: String -> Parser a -> Parser a
label = flip Text.Parsec.label

token :: Char -> Parser ()
token ch = label [ch] $ char ch *> spaces

keywords :: [Text]
keywords = ["let", "in", "Left", "Right", "S", "Z", "Char"]

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
grouping = label "grouping" $ between (token '(') (token ')') ambiguous

application :: Parser AST
application = uncurry App <$> many2 block

abstraction :: Parser AST
abstraction = label "lambda abstraction" $ Abs <$> between lambda (token '.') (many1' identifier) <*> ambiguous
  where lambda = label "lambda" $ (char '\\' <|> char 'λ') *> spaces

definition :: Parser (Def Parse)
definition = do
  name <- identifier
  token '='
  value <- ambiguous
  pure (name, value)

let_ :: Parser AST
let_ = letrecstar <|> letstar
  where
    letrecstar = LetRecP <$> between (try (keyword "letrec")) (keyword "in") definition <*> ambiguous
    letstar = Let <$> between (keyword "let") (keyword "in") definitions <*> ambiguous

    definitions :: Parser (NonEmpty (Def Parse))
    definitions = fromList <$> sepBy1 definition (token ';')

ctr :: Parser AST
ctr = pair <|> unit <|> either <|> nat <|> list <|> str
  where
    unit, pairCtr, tuple, either, left, right,
      zero, succ, natLit, consCtr, cons, charCtr, charLit, strLit :: Parser AST
    unit = Ctr CUnit [] <$ keyword "()"
    pair = pairCtr <|> tuple
    pairCtr = Ctr CPair [] <$ keyword "(,)"
    tuple = try $ between (token '(') (token ')') do
      e1 <- ambiguous
      token ','
      e2 <- ambiguous
      pure $ Ctr CPair [e1, e2]
    either = left <|> right
    left = Ctr CLeft [] <$ keyword "Left"
    right = Ctr CRight [] <$ keyword "Right"
    nat = zero <|> succ <|> natLit
    zero = Ctr CZero [] <$ keyword "Z"
    succ = Ctr CSucc [] <$ keyword "S"
    natLit = (PNat . read <$> many1 digit) <* spaces
    list = cons <|> consCtr <|> listLit
    consCtr = Ctr CCons [] <$ keyword "(::)"
    cons = try $ between (token '(') (token ')') do
      e1 <- ambiguous
      keyword "::"
      e2 <- ambiguous
      pure $ Ctr CCons [e1, e2]
    listLit = fmap PList $ between (token '[') (token ']') $ sepEndBy ambiguous (token ',')
    str = charCtr <|> charLit <|> strLit
    charCtr = Ctr CChar [] <$ keyword "Char"
    charLit = fmap PChar $ char '\'' *> anyChar <* spaces
    strLit = fmap (PStr . T.pack) $ between (token '"') (token '"') $ many (noneOf "\"")

pat :: Parser (Pat Parse)
pat = label "case alternate" $ do
  (c, ns) <- label "pattern" $
    pair <|> unit <|> left <|> right <|> zero <|> succ <|> nil <|> cons <|> char'
  keyword "->"
  e <- ambiguous
  pure $ Pat c ns e
  where
    pair = try $ between (token '(') (token ')') do
      e1 <- identifier
      token ','
      e2 <- identifier
      pure (CPair, [e1, e2])
    unit = (CUnit, []) <$ keyword "()"
    left = do
      keyword "Left"
      e <- identifier
      pure (CLeft, [e])
    right = do
      keyword "Right"
      e <- identifier
      pure (CRight, [e])
    zero = (CZero, []) <$ keyword "Z"
    succ = do
      keyword "S"
      e <- identifier
      pure (CSucc, [e])
    nil = (CNil, []) <$ keyword "[]"
    cons = try $ between (token '(') (token ')') do
      e1 <- identifier
      keyword "::"
      e2 <- identifier
      pure (CCons, [e1, e2])
    char' = do
      keyword "Char"
      e <- identifier
      pure (CChar, [e])

case_ :: Parser AST
case_ = label "case patterns" $ do
  token '{'
  pats <- sepEndBy pat (token ';')
  token '}'
  pure $ Case pats

hole :: Parser AST
hole = label "hole" $ HoleP <$ token '_'

-- | Guaranteed to consume a finite amount of input
finite :: Parser AST
finite = label "finite expression" $ variable <|> hole <|> ctr <|> case_ <|> grouping

-- | Guaranteed to consume input, but may continue until it reaches a terminator
block :: Parser AST
block = label "block expression" $ abstraction <|> let_ <|> finite

-- | Not guaranteed to consume input at all, may continue until it reaches a terminator
ambiguous :: Parser AST
ambiguous = label "any expression" $ try application <|> block

astParser :: Parser AST
astParser = ambiguous

parseAST :: Text -> Either ParseError AST
parseAST = parse (spaces *> ambiguous <* eof) "input"

type Declaration = (Text, AST)

declaration :: Parser Declaration
declaration = notFollowedBy (try let_) >> (declrec <|> decl)
  where
    declrec = do
      try $ keyword "letrec"
      (name, expr) <- definition
      pure (name, LetRecP (name, expr) (Var name))
    decl = do
      keyword "let"
      definition

-- | A program is a series of declarations and expressions to execute.
type ProgramAST = [DeclOrExprAST]
type DeclOrExprAST = Either Declaration AST

declOrExprParser :: Parser DeclOrExprAST
declOrExprParser = try (Left <$> declaration) <|> (Right <$> ambiguous)

programParser :: Parser ProgramAST
programParser = spaces *> sepEndBy declOrExprParser (token ';') <* eof

parseDeclOrExpr :: Text -> Either ParseError DeclOrExprAST
parseDeclOrExpr = parse declOrExprParser "input"

parseProgram :: Text -> Either ParseError ProgramAST
parseProgram = parse programParser "input"
