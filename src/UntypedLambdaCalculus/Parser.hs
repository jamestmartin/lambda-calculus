{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable, DataKinds #-}
module UntypedLambdaCalculus.Parser (parseExpr) where

import Control.Applicative (liftA2)
import Control.Monad.Reader (local, asks)
import Data.List (foldl1', elemIndex)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Text.Parsec (SourceName, ParseError, (<|>), many, sepBy1, letter, alphaNum, char, between, spaces, parse)
import Text.Parsec.String (Parser)
import UntypedLambdaCalculus (Expr (Free, Var, Lam, App), ReaderAlg, cataReader)

data Ast = AstVar String
         | AstLam String Ast
         | AstApp Ast Ast

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
  vars <- between (char '\\') (char '.') $ name `sepBy1` spaces
  spaces
  body <- app
  return $ foldr AstLam body vars

-- | An application expression.
app :: Parser Ast
app = foldl1' AstApp <$> safeExpr `sepBy1` spaces

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
    alg (AstLamF varName body) = Lam varName <$> local (varName :) body
    alg (AstAppF f x) = App <$> f <*>x

-- | Since applications do not require parentheses and can contain only a single item,
-- | the `app` parser is sufficient to parse any expression at all.
parseExpr :: SourceName -> String -> Either ParseError Expr
parseExpr sourceName code = toExpr <$> parse app sourceName code
