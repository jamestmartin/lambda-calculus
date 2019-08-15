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

data Ast = AstVar String
         | AstLam String Ast
         | AstApp Ast Ast

makeBaseFunctor ''Ast

instance Show Ast where
  show = cata alg
    where alg (AstVarF v)   = v
          alg (AstLamF v e) = "(\\" ++ v ++ ". " ++ e ++ ")"
          alg (AstAppF f x) = "(" ++ f ++ " " ++ x ++ ")"

name :: Parser String
name = do
  c <- letter
  cs <- many alphaNum
  return $ c : cs

var :: Parser Ast
var = AstVar <$> name

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

lam :: Parser Ast
lam = do
  char '\\'
  vars <- name `sepBy1` spaces
  char '.'
  spaces
  body <- expr
  return $ foldr AstLam body vars

safeExpr :: Parser Ast
safeExpr = var <|> parens (lam <|> expr)

expr :: Parser Ast
expr = foldl1' AstApp <$> sepBy1 safeExpr spaces
