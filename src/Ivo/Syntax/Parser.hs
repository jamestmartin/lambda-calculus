module Ivo.Syntax.Parser
  ( ParseError, parse
  , fileParser, scopeParser, defParser, exprParser
  ) where

import Ivo.Syntax.Base

import Data.Char (isSeparator)
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty (..), fromList)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Parsec hiding (spaces, label, sepBy, sepBy1, sepEndBy, sepEndBy1)
import Text.Parsec qualified as P
import Text.Parsec.Char ()
import Text.Parsec.Text (Parser)

fileParser :: Parser (Scope Text)
fileParser = do
  shebang
  scope

shebang :: Parser ()
shebang = do
  _ <- try $ string "#!"
  skipMany (noneOf "\n")
  spaces

scopeParser, scope :: Parser (Scope Text)
scopeParser = scope

scope = Scope <$> flexBraces def

defParser, def, basicDef, basicDecl :: Parser (Def Text)
defParser = def

def = try basicDef <|> basicDecl

basicDef = do
  n <- name
  tokEquals
  body <- ambiguous
  pure $ BasicDef n body

basicDecl = do
  names <- many1' name
  tokCol
  ty <- ambiguous
  pure $ BasicDecl names ty

exprParser, ambiguous, block, finite :: Parser (Expr Text)
exprParser = ambiguous

-- | Syntax which may recurse before consuming input.
ambiguous = try app <|> try arrow <|> try ann <|> block

-- | Syntax which consumes input before recursing.
block = let_ <|> lam <|> case_ <|> forall <|> finite

-- | Syntax which consumes input both before and after recursing.
finite = hole <|> lit <|> var <|> parens ambiguous

var, lit, app, let_, lam, case_, forall, arrow, ann, hole :: Parser (Expr Text)

var = Var <$> name

lit = Lit <$> literal ambiguous

app = uncurry App <$> many2' ambiguous

let_ = do
  kwdLet
  defs <- flexSepBy1' (tok ";") def
  kwdIn
  body <- ambiguous
  pure $ Let defs body

lam = do
  kwdLam
  args <- many1' name
  kwdArrow
  body <- ambiguous
  pure $ Lam args body

case_ = do
  kwdCase
  arg <- ambiguous
  branches <- caseBranches
  pure $ Case arg branches

forall = do
  kwdForall
  names <- many1' name
  kwdFatArrow
  ty <- ambiguous
  pure $ Forall names ty

arrow = do
  arg <- block
  kwdArrow
  ret <- ambiguous
  pure $ Arrow arg ret

ann = do
  expr <- block
  kwdCol
  ty <- ambiguous
  pure $ Ann expr ty

hole = Hole <$ kwdHole

caseBranches :: Parser (CaseBranches Text)
caseBranches = CaseBranches <$> flexBraces caseBranch

caseBranch :: Parser (Pattern Text, Expr Text)
caseBranch = do
  pat <- pattern_
  kwdArrow
  body <- ambiguous
  pure (pat, body)

pattern_, patVar, patLit, patIrrelevant, patApp :: Parser (Pattern Text)

pattern_ = patVar <|> patLit <|> patIrrelevant <|> patApp

patVar = PatVar <$> name

patLit = PatLit <$> literal pattern_

patIrrelevant = Irrelevant <$ kwdHole

patApp = do
  ctr <- name
  args <- many1 pattern_
  pure $ PatApp ctr args

literal :: Parser r -> Parser (Literal r)
literal p = litInt <|> litChar <|> litStr <|> litList p

litInt, litChar, litStr :: Parser (Literal r)

litInt = do
  sign <- ("-" <$ tokNegative) <|> ("" <$ tokPositive) <|> pure ""
  digits <- many1 $ optional (char '_') *> digit
  pure $ LitInt $ read $ sign ++ digits

litChar = do
  close <- matchingSingleQuote <$> tokOpenSingleQuote
  body <- noneOf (close : "\n")
  tok $ T.singleton close
  pure $ LitChar body

litStr = do
  close <- matchingQuote <$> tokOpenQuote
  body <- many1 $ satisfy \c -> c /= close && not (isSeparator c)
  tok $ T.singleton close
  pure $ LitStr $ T.pack body

litList :: Parser r -> Parser (Literal r)
litList p = LitList <$> flexBrackets p

comment :: Parser ()
comment = do
  tok ";;"
  skipMany1 $ noneOf "\n"
  spaces

name :: Parser Text
name = do
  notFollowedBy (anyKwd <|> void litInt)
  n <- many1 legalChar
  spaces
  pure $ T.pack n

legalChar :: Parser Char
legalChar = satisfy isLegalChar

isLegalChar :: Char -> Bool
isLegalChar c = not (isSeparator c) && c `notElem` forbidden
  where
    forbidden :: String
    forbidden = "\"“”'‘’(){};"

kwdLet, kwdIn, kwdLam, kwdCase, kwdForall, kwdFatArrow, kwdArrow,
  kwdCol, kwdHole :: Parser ()
kwdLet = kwd "let"
kwdIn = kwd "in"
kwdLam = kwd "λ" <|> kwd "\\"
kwdCase = kwd "case"
kwdForall = kwd "∀" <|> kwd "forall"
kwdFatArrow = kwd "⇒" <|> kwd "=>"
kwdArrow = kwd "→" <|> kwd "->"
kwdCol = kwd ":"
kwdHole = kwd "_"

kwd :: Text -> Parser ()
kwd txt = try do
  _ <- string $ T.unpack txt
  notFollowedBy legalChar

anyKwd :: Parser ()
anyKwd = kwdLet <|> kwdIn <|> kwdLam <|> kwdCase <|> kwdForall <|> kwdFatArrow
  <|> kwdArrow <|> kwdCol <|> kwdHole

tokEquals, tokCol, tokNegative, tokPositive :: Parser ()

tokEquals = void $ char '=' -- U+003D EQUALS SIGN

tokCol = void $ char ':' -- U+003A COLON

tokNegative = void $
      char '-' -- U+002D HYPHEN-MINUS
  <|> char '⁻' -- U+207B SUPERSCRIPT MINUS

tokPositive = void $
      char '+' -- U+002B PLUS SIGN
  <|> char '⁺' -- U+207A SUPERSCRIPT PLUS SIGN

tokOpenQuote, tokOpenSingleQuote :: Parser Char

tokOpenQuote =
      char '"' -- U+0022 QUOTATION MARK
  <|> char '“' -- U+201C LEFT DOUBLE QUOTATION MARK

tokOpenSingleQuote =
      char '\'' -- U+0027 APOSTROPHE
  <|> char '‘'  -- U+2018 LEFT SINGLE QUOTATION MARK

matchingQuote, matchingSingleQuote :: Char -> Char

matchingQuote = \case
  '"' -> '"' -- U+0022 QUOTATION MARK (again)
  '“' -> '”' -- U+201D RIGHT DOUBLE QUOTATION MARK
  _ -> error "no matching double quote"

matchingSingleQuote = \case
  '\'' -> '\'' -- U+0027 APOSTROPHE (again)
  '‘'  -> '’'  -- U+2019 RIGHT SINGLE QUOTATION MARK
  _ -> error "no matching single quote"

tok :: Text -> Parser ()
tok txt = do
  _ <- string $ T.unpack txt
  spaces

sepEndBy, sepEndBy1 :: Parser delim -> Parser a -> Parser [a]
sepEndBy = flip P.sepEndBy
sepEndBy1 = flip P.sepEndBy1

flexSepBy :: Parser delim -> Parser a -> Parser [a]
flexSepBy delim p = do
  optional delim
  sepEndBy delim p

flexSepBy1 :: Parser delim -> Parser a -> Parser [a]
flexSepBy1 delim p = do
  optional delim
  sepEndBy1 delim p

flexSepBy1' :: Parser delim -> Parser a -> Parser (NonEmpty a)
flexSepBy1' delim p = fromList <$> flexSepBy1 delim p

parens :: Parser a -> Parser a
parens = between (tok "(") (tok ")")

flexBrackets :: Parser a -> Parser [a]
flexBrackets = between (kwd "[") (kwd "]") . flexSepBy (tok ";")

flexBraces :: Parser a -> Parser [a]
flexBraces = between (tok "{") (tok "}") . flexSepBy (tok ";")

many1' :: Parser a -> Parser (NonEmpty a)
many1' p = (:|) <$> p <*> many p

many2' :: Parser a -> Parser (a, NonEmpty a)
many2' p = (,) <$> p <*> many1' p

spaces :: Parser ()
spaces = P.spaces >> optional comment
