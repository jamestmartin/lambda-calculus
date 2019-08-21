{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable, DataKinds #-}
module UntypedLambdaCalculus.Parser (parseExpr) where

import Control.Applicative (liftA2)
import Control.Monad.Reader (local, asks)
import Data.List (foldl', elemIndex)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Text.Parsec (SourceName, ParseError, (<|>), many, sepBy, letter, alphaNum, char, between, spaces, parse, string)
import Text.Parsec.String (Parser)
import UntypedLambdaCalculus (Expr (Free, Var, Lam, App), ReaderAlg, cataReader)

data Ast = AstVar String
         | AstLam [String] Ast
         | AstApp [Ast]
         | AstLet String Ast Ast

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
app = AstApp <$> consumesInput `sepBy` spaces

let_ :: Parser Ast
let_ = do
  string "let "
  bound <- name
  string " = "
  -- we can't allow raw `app` or `lam` here
  -- because they will consume the `in` as a variable.
  val <- let_ <|> var <|> parens app
  char ' '
  spaces
  string "in "
  body <- app
  return $ AstLet bound val body

-- | An expression, but where applications must be surrounded by parentheses,
-- | to avoid ambiguity (infinite recursion on `app` in the case where the first
-- | expression in the application is also an `app`, consuming no input).
consumesInput :: Parser Ast
consumesInput = let_ <|> var <|> lam <|> parens app

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
    alg (AstAppF es) = foldl' App (Lam "x" (Var 0)) <$> sequenceA es
    alg (AstLetF var val body) = do
      body' <- local (var :) body
      App (Lam var body') <$> val

-- | Since applications do not require parentheses and can contain only a single item,
-- | the `app` parser is sufficient to parse any expression at all.
parseExpr :: SourceName -> String -> Either ParseError Expr
parseExpr sourceName code = toExpr <$> parse app sourceName code
