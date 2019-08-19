{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable, DataKinds #-}
module UntypedLambdaCalculus.Parser (parseExpr) where

import Control.Applicative (liftA2)
import Control.Monad.Reader (local, asks)
import Data.List (foldl', elemIndex)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Text.Parsec (SourceName, ParseError, (<|>), many, sepBy, letter, alphaNum, char, between, spaces, parse)
import Text.Parsec.String (Parser)
import UntypedLambdaCalculus (Expr (Free, Var, Lam, App, Nil), ReaderAlg, cataReader)

data Ast = AstVar String
         | AstLam [String] Ast
         | AstApp [Ast]

makeBaseFunctor ''Ast

-- | A variable name.
name :: Parser String
name = liftA2 (:) letter $ many alphaNum

-- | A variable expression.
var :: Parser Ast
var = AstVar <$> name

-- | Run parser between parentheses.
parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

-- | A lambda expression.
lam :: Parser Ast
lam = do
  vars <- between (char '\\') (char '.') $ name `sepBy` spaces
  spaces
  body <- app
  return $ AstLam vars body

-- | An application expression.
app :: Parser Ast
app = AstApp <$> safeExpr `sepBy` spaces

-- | An expression, but where applications must be surrounded by parentheses,
-- | to avoid ambiguity (infinite recursion on `app` in the case where the first
-- | expression in the application is also an `app`, consuming no input).
safeExpr :: Parser Ast
safeExpr = var <|> lam <|> parens (lam <|> app)

toExpr :: Ast -> Expr
toExpr = cataReader alg []
  where
    alg :: ReaderAlg AstF [String] Expr
    alg (AstVarF varName) = do
      bindingSite <- asks (elemIndex varName)
      return $ case bindingSite of
        Just index -> Var index
        Nothing    -> Free varName
    alg (AstLamF vars body) = foldr (\v e -> Lam v <$> local (v :) e) body vars
    alg (AstAppF [e]) = e
    alg (AstAppF es) = foldl' App Nil <$> sequenceA es

-- | Since applications do not require parentheses and can contain only a single item,
-- | the `app` parser is sufficient to parse any expression at all.
parseExpr :: SourceName -> String -> Either ParseError Expr
parseExpr sourceName code = toExpr <$> parse app sourceName code
