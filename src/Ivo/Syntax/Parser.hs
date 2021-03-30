module Ivo.Syntax.Parser
  ( ParseError, parse
  , Declaration, TopLevelAST, ProgramAST
  , parseAST, parseTopLevel, parseProgram
  , typeParser, schemeParser, astParser, topLevelParser, programParser
  ) where

import Ivo.Syntax.Base

import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (fromList)
import Data.Text qualified as T
import Prelude hiding (succ, either)
import Text.Parsec hiding (label, token, spaces)
import Text.Parsec qualified
import Text.Parsec.Expr
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
keywords = ["let", "in", "Left", "Right", "S", "Z", "forall", "Char", "Void", "Unit", "Nat", "Char"]

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

tvariable :: Parser Type
tvariable = label "variable" $ TVar <$> identifier

many1' :: Parser a -> Parser (NonEmpty a)
many1' p = fromList <$> many1 p

many2 :: Parser a -> Parser (a, NonEmpty a)
many2 p = (,) <$> p <*> many1' p

grouping :: Parser AST
grouping = label "grouping" $ between (token '(') (token ')') ambiguous

tgrouping :: Parser Type
tgrouping = label "grouping" $ between (token '(') (token ')') tambiguous

application :: Parser AST
application = label "application" $ uncurry App <$> many2 block

tapplication :: Parser Type
tapplication = label "application" $ uncurry tapp' <$> many2 tblock
  where tapp' t1 (t2 :| ts) = tapp (t1 : t2 : ts)

abstraction :: Parser AST
abstraction = label "lambda abstraction" $ Abs <$> between lambda (token '.') (many1' identifier) <*> ambiguous
  where lambda = label "lambda" $ (char '\\' <|> char 'λ') *> spaces

definition :: Parser (Def Parse)
definition = label "definition" $ do
  name <- identifier
  token '='
  value <- ambiguous
  pure (name, value)

let_ :: Parser AST
let_ = label "let expression" $
  Let <$> between (keyword "let") (keyword "in") definitions <*> ambiguous
  where
    definitions :: Parser (NonEmpty (Def Parse))
    definitions = fromList <$> sepBy1 definition (token ';')

ctr :: Parser AST
ctr = label "data constructor" $ pair <|> unit <|> either <|> nat <|> list <|> str
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

ann :: Parser AST
ann = label "type annotation" $ do
  e <- block
  token ':'
  t <- tambiguous
  pure (Ann () e t)

hole :: Parser AST
hole = label "hole" $ HoleP <$ token '_'

tlist :: Parser Type
tlist = between (token '[') (token ']') $ ((TApp TList <$> tambiguous) <|> pure TList)

tinfix :: Parser Type
tinfix = buildExpressionParser ttable tblock
  where
    ttable :: [[Operator Text () Identity Type]]
    ttable = [ [Infix (binop TAbs  <$ arrSym)    AssocRight]
             , [Infix (binop TProd <$ token '*') AssocRight]
             , [Infix (binop TSum  <$ token '+') AssocRight]
             ]

    arrSym :: Parser ()
    arrSym = token '→' <|> keyword "->"

    binop :: Type -> Type -> Type -> Type
    binop c t1 t2 = TApp (TApp c t1) t2

tctr :: Parser Type
tctr = tlist <|> tunit <|> tvoid <|> tnat <|> tchar
  where
    tunit = TUnit <$ (keyword "Unit" <|> keyword "⊤")
    tvoid = TVoid <$ (keyword "Void" <|> keyword "⊥")
    tnat  = TNat  <$ (keyword "Nat"  <|> keyword "ℕ")
    tchar = TChar <$  keyword "Char"

tfinite :: Parser Type
tfinite = tvariable <|> tlist <|> tctr <|> tgrouping

tblock :: Parser Type
tblock = tfinite

tambiguous :: Parser Type
tambiguous = try tinfix <|> try tapplication <|> tblock

tforall :: Parser Scheme
tforall = do
  keyword "forall" <|> token '∀'
  names <- many1 (identifier <* spaces)
  token '.'
  ty <- tambiguous
  pure $ TForall names ty

scheme :: Parser Scheme
scheme = tforall <|> (TForall [] <$> tambiguous)

-- | Guaranteed to consume a finite amount of input
finite :: Parser AST
finite = label "finite expression" $ variable <|> hole <|> ctr <|> case_ <|> grouping

-- | Guaranteed to consume input, but may continue until it reaches a terminator
block :: Parser AST
block = label "block expression" $ abstraction <|> let_ <|> finite

-- | Not guaranteed to consume input at all, may continue until it reaches a terminator
ambiguous :: Parser AST
ambiguous = label "any expression" $ try ann <|> try application <|> block

typeParser :: Parser Type
typeParser = tambiguous

schemeParser :: Parser Scheme
schemeParser = scheme

astParser :: Parser AST
astParser = ambiguous

parseAST :: Text -> Either ParseError AST
parseAST = parse (spaces *> ambiguous <* eof) "input"

type Declaration = (Text, Maybe Type, AST)

definitionAnn :: Parser Declaration
definitionAnn = do
  name <- identifier
  ty <- optionMaybe $ token ':' *> tambiguous
  token '='
  e <- ambiguous
  pure (name, ty, e)

declaration :: Parser Declaration
declaration = notFollowedBy (try let_) >> do
  keyword "let"
  definitionAnn

-- | A program is a series of declarations and expressions to execute.
type ProgramAST = [Declaration]
type TopLevelAST = [Either Declaration AST]

topLevel :: Parser (Either Declaration AST)
topLevel = try (Left <$> declaration) <|> (Right <$> ambiguous)

topLevelParser :: Parser TopLevelAST
topLevelParser = spaces *> sepEndBy topLevel (token ';') <* eof

shebang :: Parser ()
shebang = do
  try $ keyword "#!"
  skipMany (noneOf "\n")
  spaces

programParser :: Parser ProgramAST
programParser = shebang *> sepEndBy declaration (token ';') <* eof

parseTopLevel :: Text -> Either ParseError TopLevelAST
parseTopLevel = parse topLevelParser "input"

parseProgram :: Text -> Either ParseError ProgramAST
parseProgram = parse programParser "input"
