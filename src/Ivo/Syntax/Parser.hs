module Ivo.Syntax.Parser
  ( ParseError, parse
  , programParser, replParser
  ) where

import Ivo.Syntax.Base

import Data.List.NonEmpty (fromList)
import Data.Text qualified as T
import Prelude hiding (succ, either)
import Text.Parsec hiding (label, token, spaces, tokens, anyToken)
import Text.Parsec qualified
import Text.Parsec.Text (Parser)

-- | Parse the contents of a file containing Ivo source code,
-- with an optional shebang.
--
-- See 'Scope'.
programParser :: Parser Scope
programParser = do
  optional shebang
  scope

-- | Parse REPL input (which consists of top-level statements and expressions).
--
-- See 'TopLevel' and 'Expr'.
replParser :: Parser [Either TopLevel Expr]
replParser = sepEndBy ((Left <$> topLevel) <|> (Right <$> ambiguous)) (token ";")

---
--- This is a very long module which does not even document the
--- syntactic constructs which it is implementing.
--- On the other hand, 'Ivo.Syntax.Base' goes into great detail'
--- about the syntax while hardly containing any actual code.
--- Thus, to assist with navigation, the order
--- in which parsers are defined in this module
--- is the same as the order in which they are described in 'Ivo.Syntax.Base'.
--- I would recommend keeping both files open at once while working on this code
--- so you can cross-reference the informal description of the syntax
--- with the parsers.
---

shebang :: Parser ()
shebang = label "shebang" do
  try $ keyword "#!"
  skipMany (noneOf "\n")
  spaces

-- | 'Scope'
scope :: Parser Scope
scope = Scope <$> many (topLevel <* token ";")

topLevel, tlOpen, tlDef, tlData, tlAxiom, tlSig :: Parser TopLevel

-- | 'TopLevel'
topLevel = tlOpen <|> tlDef <|> tlData <|> tlAxiom <|> tlSig

-- | 'Open'
tlOpen  = label "open statement" $
  Open <$> publicOrPrivate
       <*> ambiguous
       <*> openUsing
       <*> openHiding
       <*> openRenaming
  where
    openUsing, openHiding :: Parser [(Text, Maybe (Maybe [Text]))]
    openRenaming :: Parser [(Text, Text)]

    openUsing = option [] do
      keyword "using"
      flexibleParens openId

    openHiding = option [] do
      keyword "hiding"
      flexibleParens openId

    openId = do
      name <- identifier
      ctrs <- optionMaybe $ openAll <|> openCtrs
      pure (name, ctrs)

    openRenaming = option [] do
      keyword "renaming"
      flexibleParens $ cobetween identifier tkArr identifier

    openAll, openCtrs :: Parser (Maybe [Text])

    openAll  = Nothing <$ keyword ".."

    openCtrs = optionMaybe $ flexibleParens identifier

-- | 'Define'
tlDef   = Define  <$> publicOrPrivate <*> definition

-- | 'TLData'
tlData  = TLData  <$> data_

-- | 'TLAxiom'
tlAxiom = TLAxiom <$> axiom

-- | 'TLSig'
tlSig   = TLSig   <$> sig

-- | 'PublicOrPrivate'
publicOrPrivate :: Parser PublicOrPrivate
publicOrPrivate = (Public <$ keyword "pub") <|> pure Private

-- | 'Definition'
definition :: Parser Definition
definition = label "definition" do
  name <- identifier
  ty <- optionMaybe typeSig
  token "="
  expr <- ambiguous
  pure $ Definition name ty expr

ambiguous, block, finite :: Parser Expr
-- | 'Expr's which are not guaranteed to consume input before recursing.
ambiguous = label "expression" $
  exAccess <|> exArrow <|> exAnn <|> exAppI <|> exApp <|> exTypeApp

-- | 'Expr's with a terminal symbol on its left.
block = label "block expression" $
  exData <|> exAxiom <|> exForall <|> exTypeLam <|> exBlockLam <|>
  exLet <|> exBlockCase <|> finite

-- | 'Expr's with a terminal symbol on both the left /and/ right.
finite = label "finite expression" $
  exSig <|> exMod <|> exHole <|> exFiniteCase <|> exFiniteLam <|>
  exLit <|> exVar <|> exGroup

exAccess, exArrow, exAnn, exAppI, exApp, exTypeApp,
  exData, exAxiom, exForall, exTypeLam, exBlockLam, exFiniteLam, exLet,
  exSig, exMod, exHole, exBlockCase, exFiniteCase, exLit, exVar, exGroup
  :: Parser Expr

exGroup = label "parenthetical expression" $ betweenParens ambiguous

typeSig, kindSig, modSig :: Parser Type

typeSig = label "type signature" $ keyword ":" *> ambiguous

kindSig = label "kind signature" typeSig

modSig = label "signature" typeSig

-- | 'Data'
exData = Data <$> data_

data_ :: Parser Expr
data_ = label "data expression" do
  keyword "data"
  ambiguous

-- | 'Axiom'
exAxiom = Axiom <$> axiom

axiom :: Parser Expr
axiom = label "axiom expression" do
  keyword "axiom"
  ambiguous

-- | 'SigE'
exSig = SigE <$> sig

-- | 'Mod'
exMod = label "module expression" do
  keyword "mod"
  params <- sigParams
  sig <- optionMaybe modSig
  body <- flexibleBraces topLevel
  pure $ Mod params sig body

-- | 'Access'
exAccess = label "field access" $ try do
  expr <- block
  tkColCol
  name <- identifier
  pure $ Access expr name

-- | @∷@
tkColCol :: Parser ()
tkColCol = label "∷" $ token "∷" <|> token "::"

-- | 'Forall'
exForall = label "∀ type" $ do
  tkForall
  uncurry Forall <$> typeBinder

-- | @∀@
tkForall :: Parser ()
tkForall = label "∀" $ keyword "∀" <|> keyword "forall"

-- | Everything about @forall@ and @^@ is the same
-- except for the token and whether a type or expr
-- (which are also syntactically the same object)
-- comes next.
--
-- This factors out all of that stuff they have in common.
typeBinder :: Parser (NonEmpty (Either (NonEmpty Text, Kind) Text), Expr)
typeBinder = do
  vars <- many1' typeBinder'
  tkArr
  ty <- ambiguous
  pure (vars, ty)
  where
    typeBinder', kindedBinder, plainBinder
      :: Parser (Either (NonEmpty Text, Kind) Text)

    typeBinder' = kindedBinder <|> plainBinder

    kindedBinder = betweenParens $ do
      names <- many1' identifier
      kind <- kindSig
      pure $ Left (names, kind)

    plainBinder = Right <$> identifier

-- | 'Arrow'
exArrow = label "→ type" $ try do
  (names, argTy) <- arrowArgs
  tkArr
  rTy <- ambiguous
  pure $ Arrow names argTy rTy

  where
    arrowArgs, namedArgs, typeArg :: Parser ([Text], Type)

    arrowArgs = namedArgs <|> typeArg

    namedArgs = betweenParens do
      names <- many1 identifier
      ty <- kindSig
      pure (names, ty)

    typeArg = ([],) <$> block

-- | @→@
tkArr :: Parser ()
tkArr = label "→" $ keyword "→" <|> keyword "->"

-- | 'Ann'
exAnn = label "annotated expression" $ try do
  expr <- block
  ty <- typeSig
  pure $ Ann expr ty

-- | 'Hole'
exHole = label "hole" $ Hole <$ keyword "_"

-- | 'TypeLam'
exTypeLam = label "type Λ" $ do
  tkBigLam
  uncurry TypeLam <$> typeBinder

-- | @Λ@
tkBigLam :: Parser ()
tkBigLam = label "Λ" $ keyword "Λ" <|> keyword "^"

-- | 'TypeApp'
exTypeApp = label "type application" $ try do
  expr <- block
  token "@"
  arg <- ambiguous
  pure $ TypeApp expr arg

-- | 'Lam' with only one undelimited case branch
exBlockLam = label "λ expression" $ try do
  args <- many pattern_
  tkArr
  body <- ambiguous
  pure $ LamBlock args body

-- | 'Lam' with delimited case branches
exFiniteLam = label "λ-case expression" $ try do
  simpleArgs <- many pattern_
  body <- caseBranches
  pure $ Lam simpleArgs body

tkLam :: Parser ()
tkLam = label "λ" $ keyword "λ" <|> keyword "\\"

-- | 'App'
exApp = label "application" $ try do
  ef      <- finite
  exs     <- many finite
  exfinal <- block
  pure $ App ef (snoc' exs exfinal)

-- | 'AppI'
exAppI = label "infix application" $
  fail "infix expressions not yet supported"

-- | 'Let'
exLet = label "let expression" do
  keyword "let"
  decls <- flexibleSepBy1 (token ";") topLevel
  keyword "in"
  body <- ambiguous
  pure $ Let decls body

-- | 'Case' with only one undelimited case branch
exBlockCase = label "case expression" $ try do
  keyword "case"
  exprs <- flexibleSepBy (keyword "|") ambiguous
  body <- caseBranches
  pure $ Case exprs body

-- | 'Case'
exFiniteCase = label "case-of expression" $ try do
  keyword "case"
  exprs <- flexibleSepBy1 (keyword "|") ambiguous
  keyword "of"
  pats <- flexibleSepBy1 (keyword "|") pattern_
  tkArr
  body <- ambiguous
  pure $ CaseBlock exprs pats body

-- | 'Lit'
exLit = label "literal" $ Lit <$> lit ambiguous

-- | 'Var'
exVar = label "variable" $ Var <$> identifier

-- | 'Sig'
sig, sigADT, sigGADT, sigSig :: Parser Sig
sig = label "signature" $ sigADT <|> sigGADT <|> sigSig

sigParams :: Parser [Either (NonEmpty Text, Kind) Text]
sigParams = label "signature parameters" $ many $ annParam <|> unannParam
  where
    annParam, unannParam :: Parser (Either (NonEmpty Text, Kind) Text)

    annParam = betweenParens do
      names <- many1' identifier
      kind <- kindSig
      pure $ Left (names, kind)

    unannParam = Right <$> identifier

-- | 'ADT'
sigADT = label "adt" do
  keyword "adt"
  name <- identifier
  params <- sigParams
  kind <- optionMaybe kindSig
  (codataQ, body) <- betweenBraces $
    emptyData <|> emptyCodata <|> dataCtrs <|> codataElims
  pure $ SigQs params $ ADT codataQ name kind body
  where
    emptyData, emptyCodata, dataCtrs, codataElims
      :: Parser (Bool, [(Text, [Type])])

    emptyData   = try $ (False, []) <$ keyword "+"
    emptyCodata = try $ (True,  []) <$ keyword "&"

    dataCtrs = try $ (False,) <$> flexibleSepBy1 (keyword "+") do
      name <- identifier
      ctrs <- many $ notFollowedBy (keyword "+") >> finite
      pure (name, ctrs)

    codataElims = try $ (True,) <$> flexibleSepBy1 (keyword "&") do
      name <- identifier
      elims <- many $ notFollowedBy (keyword "&") >> finite
      pure (name, elims)

-- | 'GADT'
sigGADT = label "gadt" do
  keyword "gadt"
  name <- identifier
  params <- sigParams
  kind <- optionMaybe kindSig
  ctrs <- flexibleBraces declaration
  pure $ SigQs params $ GADT name kind ctrs

-- | 'Sig'
sigSig = label "sig" do
  keyword "sig"
  params <- sigParams
  ctrs <- flexibleBraces $ (Left <$> declaration) <|> (Right <$> topLevel)
  pure $ SigQs params $ Sig ctrs

declaration :: Parser (NonEmpty Text, Type)
declaration = label "declaration" $ (,) <$> many1' identifier <*> typeSig

pattern_, finitePattern, patVar, patIrr, patImp, patApp,
  patAppI, patView, patLit, patGroup :: Parser Pattern

-- | 'Pattern'
pattern_ = label "pattern" $
  patApp <|> patAppI <|> patView <|> finitePattern

finitePattern =
  patIrr <|> patImp <|> patLit <|> patVar <|> patGroup

patGroup = betweenParens pattern_

-- | 'PatVar'
patVar = label "pattern variable" $ PatVar <$> identifier <*> optionMaybe typeSig

-- | 'Irrelevant'
patIrr = label "irrelevant pattern" $ Irrelevant <$ keyword "_"

-- | 'Impossible'
patImp = label "impossible pattern" $ Impossible <$ keyword "!"

-- | 'PatApp'
patApp = label "pattern application" $ do
  name <- identifier
  pats <- many1 finitePattern
  pure $ PatApp name pats

-- FIXME: Implement infix pattern application
-- | 'PatAppI'
patAppI = label "infix pattern application" $
  fail "infix pattern application not yet implemented"

-- | 'View'
patView = label "view pattern" $ try $ View <$> block <*> (tkArr *> pattern_)

-- | 'PatLit'
patLit = label "literal pattern" $ PatLit <$> lit pattern_

caseBranches :: Parser [CaseBranch]
caseBranches = label "case branches" $ flexibleBraces caseBranch

caseBranch :: Parser CaseBranch
caseRefinement, caseGuards, casePlain :: [Pattern] -> Parser CaseBranch

-- | 'CaseBranch'
caseBranch = label "case branch" do
  pats <- many1 pattern_
  caseRefinement pats <|> caseGuards pats <|> casePlain pats

-- FIXME: Implement case refinement
-- | 'Refinement'
caseRefinement pats = label "case refinement" $
  fail "case refinement not implemented"

-- FIXME: Implement case guards
-- | 'Guards'
caseGuards pats = label "guard clauses" $
  fail "case guards not yet implemented"

-- | 'PlainCase'
casePlain pats = do
  tkArr
  PlainCase pats <$> ambiguous

-- FIXME: Implement guard clauses
-- | 'Guard'
guard :: Parser Guard
guard = label "guard clause" $
  fail "guard clauses not yet implemented"

lit, litList            :: Parser a -> Parser (Lit a)
litInt, litChar, litStr :: Parser (Lit a)

-- | 'Lit'
lit m = label "literal" $ litInt <|> litChar <|> litStr <|> litList m

-- | 'LitInt'
litInt = label "integer literal" $ LitInt . read <$> try do
  sign <- optionMaybe $ char '+' <|> char '-'
  digits <- many1 digit
  pure $ maybe digits (: digits) sign

-- | 'LitChar'
litChar = label "character literal" $
  fmap LitChar $ between (char '\'') (char '\'') $ noneOf "\\'\n"

-- | 'LitStr'
litStr = label "string literal" $ fmap (LitStr . T.pack) $
  between (char '"') (char '"') $ many1 $ noneOf "\\\"\n"

-- | 'LitList'
litList m = label "list literal" $ LitList <$> flexibleBrackets m

-- | 'FlexibleSeparatorSyntax'
flexibleSepBy :: Parser a -> Parser b -> Parser [b]
flexibleSepBy delim m = do
  optional delim
  sepEndBy m delim

-- | 'FlexibleSeparatorSyntax'
flexibleSepBy1 :: Parser a -> Parser b -> Parser [b]
flexibleSepBy1 delim m = do
  optional delim
  sepEndBy1 m delim

flexibleParens, flexibleBrackets, flexibleBraces :: Parser a -> Parser [a]

-- | 'FlexibleSeparatorSyntax', with the @;@ separator between @(@ @)@
flexibleParens = betweenParens . flexibleSepBy (token ";")

-- | 'FlexibleSeparatorSyntax', with the @;@ separator between @[@ @]@
flexibleBrackets = betweenBrackets . flexibleSepBy (token ";")

-- | 'FlexibleSeparatorSyntax', with the @;@ separator between @{@ @}@
flexibleBraces = betweenBraces . flexibleSepBy (token ";")

---
--- Helper parsers which aren't explicitly specified by the AST
---

-- | An identifier is a sequence of characters with no token which is not a keyword.
identifier :: Parser Text
identifier = label "identifier" $ try do
    notFollowedBy anyKeyword
    name <- T.pack <$> (notFollowedBy anyToken *> many1 anyChar <* spaces)
    if any ((`elem` keywords) . T.unpack) $ T.splitOn "_" name
      then fail "identifier contained keywords"
      else pure name

token :: String -> Parser ()
token tk = label tk $ string tk *> spaces

anyToken :: Parser ()
anyToken = label "any token" $ choice $ map token tokens

-- | Tokens which are reserved by syntax
-- and may not appear /anywhere/ in an identifier.
tokens :: [String]
tokens =
  [ "//", "/*", "*/"
  , "(", ")", "{", "}"
  , ";", "∷", "::", "@"
  , " ", "\r", "\n", "\t"
  , "```"
  ]

-- | A keyword is an exact string which is not part of an identifier.
keyword :: String -> Parser ()
keyword kwd = label kwd $ do
  try do
    _ <- string kwd
    notFollowedBy (notFollowedBy anyToken >> anyChar)
  spaces

anyKeyword :: Parser ()
anyKeyword = label "any keyword" $ choice $ map keyword keywords

-- | Keywords are reserved by syntax and are not allowed to be used as identifiers
-- or as a part of a `_`-separated identifier.
--
-- However, they may be used as /part/ of an identifier.
keywords :: [String]
keywords =
  [ "open", "using", "hiding", "renaming", "..", "pub"
  , "data", "axiom", "adt", "gadt", "sig", "mod"
  , "forall"
  , "let", "in", "case"
  , "[", "]"
  , ":", "∀", "Λ", "^", "λ", "\\", "→", "->"
  , "|", "?", "!", "_", "..."
  ]

spaces :: Parser ()
spaces = label "whitespace or comments" $
  Text.Parsec.spaces >> optional (try (comment >> spaces))

comment, lineComment, blockComment :: Parser ()

comment = label "comment" $ lineComment <|> blockComment

lineComment = label "line comment" $ do
  _ <- try (string "//")
  _ <- many1 (noneOf "\n")
  pure ()

blockComment = label "block comment" $ do
  _ <- try (string "/*")
  _ <- many1 $ notFollowedBy (string "*/") >> anyChar
  _ <- string "*/"
  pure ()

-- | The opposite of `between`: ignore the middle, keep the sides
cobetween :: Parser a -> Parser b -> Parser c -> Parser (a, c)
cobetween a b c = do
  x <- a
  _ <- b
  y <- c
  pure (x, y)

-- the parens/brackets/braces word choice is totally meaningless
betweenParens, betweenBrackets, betweenBraces :: Parser a -> Parser a

-- | Between @(@ @)@
betweenParens = between (token "(") (token ")")

-- | Between @[@ @]@
betweenBrackets = between (keyword "[") (keyword "]")

-- | Between @{@ @}@
betweenBraces = between (keyword "{") (keyword "}")

many1' :: Parser a -> Parser (NonEmpty a)
many1' = fmap fromList . many1

snoc' :: [a] -> a -> NonEmpty a
snoc' xs x = fromList $ xs ++ [x]

label :: String -> Parser a -> Parser a
label = flip Text.Parsec.label
