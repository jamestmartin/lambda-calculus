{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module UntypedLambdaCalculus.Parser ( Ast (AstVar, AstLam, AstApp)
                                    , AstF (AstVarF, AstLamF, AstAppF)
                                    , expr
                                    ) where

import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.TH (makeBaseFunctor,)
import Data.List (foldl1')
import Text.Parsec
import Text.Parsec.String

-- | The abstract syntax tree of lambda calculus.
data Ast = AstVar String
         | AstLam String Ast
         | AstApp Ast Ast

makeBaseFunctor ''Ast

instance Show Ast where
  show = cata alg
    where alg (AstVarF v)   = v
          alg (AstLamF v e) = "(\\" ++ v ++ ". " ++ e ++ ")"
          alg (AstAppF f x) = "(" ++ f ++ " " ++ x ++ ")"

-- | A variable name.
name :: Parser String
name = do
  c <- letter
  cs <- many alphaNum
  return $ c : cs

-- | A variable expression.
var :: Parser Ast
var = AstVar <$> name

-- | Run parser between parentheses.
parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

-- | A lambda expression.
lam :: Parser Ast
lam = do
  char '\\'
  vars <- name `sepBy1` spaces
  char '.'
  spaces
  body <- expr
  return $ foldr AstLam body vars

-- | An application expression.
app :: Parser Ast
app = foldl1' AstApp <$> sepBy1 safeExpr spaces

-- | An expression, but where applications must be surrounded by parentheses,
-- | to avoid ambiguity (infinite recursion on `app` in the case where the first
-- | expression in the application is also an `app`, consuming no input).
safeExpr :: Parser Ast
safeExpr = var <|> lam <|> parens (lam <|> app)

-- | Since applications do not require parentheses and can contain only a single item,
-- | the `app` parser is sufficient to parse any expression at all.
expr :: Parser Ast
expr = app
