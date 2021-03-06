{-# LANGUAGE DeriveGeneric #-}
module LambdaCalculus.Expression
  ( Expression (Variable, Application, Abstraction)
  , ast2expr, expr2ast
  , pattern Lets, pattern Abstractions, pattern Applications
  , viewLet, viewAbstraction, viewApplication
  , basicShow
  ) where

-- The definition of Expression is in its own file because:
-- * Expression and AbstractSyntax should not be in the same file
-- * Expression's `show` definition depends on AbstractSyntax's show definition,
--   which means that `ast2expr` and `expr2ast` can't be in AbstractSyntax
--   because of mutually recursive modules
-- * I don't want to clutter the module focusing on the actual evaluation
--   with all of these irrelevant conversion operators.

import Data.Bifunctor (first, second)
import Data.List.NonEmpty (NonEmpty ((:|)), fromList, toList)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LambdaCalculus.Parser.AbstractSyntax (AbstractSyntax)
import LambdaCalculus.Parser.AbstractSyntax qualified as AST
import TextShow (Builder, fromText, TextShow, showb, showt)

data Expression
  = Variable Text
  -- | Function application: `(f x)`.
  | Application Expression Expression
  -- | Lambda abstraction: `(λx. e)`.
  | Abstraction Text Expression
  deriving (Eq, Generic)

-- | A naive implementation of 'show', which does not take advantage of any syntactic sugar
-- and always emits optional parentheses.
basicShow :: Expression -> Builder
basicShow (Variable var) = fromText var
basicShow (Application ef ex) = "(" <> showb ef <> " " <> showb ex <> ")"
basicShow (Abstraction var body) = "(λ" <> fromText var <> ". " <> showb body <> ")"

-- | Convert from an abstract syntax tree to an expression.
ast2expr :: AbstractSyntax -> Expression
ast2expr (AST.Variable name) = Variable name
ast2expr (AST.Application (x :| [])) = ast2expr x
ast2expr (AST.Application xs) = foldl1 Application $ map ast2expr (toList xs)
ast2expr (AST.Abstraction names body) = foldr Abstraction (ast2expr body) names
ast2expr (AST.Let defs body) = foldr (uncurry letExpr . second ast2expr) (ast2expr body) defs
  where
    letExpr :: Text -> Expression -> Expression -> Expression
    letExpr name val body' = Application (Abstraction name body') val

-- | View nested applications of abstractions as a list.
pattern Lets :: [(Text, Expression)] -> Expression -> Expression
pattern Lets defs body <- (viewLet -> (defs@(_:_), body))

viewLet :: Expression -> ([(Text, Expression)], Expression)
viewLet (Application (Abstraction var body) x) = first ((var, x) :) (viewLet body)
viewLet x = ([], x)

-- | View nested abstractions as a list.
pattern Abstractions :: [Text] -> Expression -> Expression
pattern Abstractions names body <- (viewAbstraction -> (names@(_:_), body))

viewAbstraction :: Expression -> ([Text], Expression)
viewAbstraction (Abstraction name body) = first (name :) (viewAbstraction body)
viewAbstraction x = ([], x)

-- | View left-nested applications as a list.
pattern Applications :: [Expression] -> Expression
pattern Applications exprs <- (viewApplication -> (exprs@(_:_:_)))

{-# COMPLETE Abstractions, Applications, Variable :: Expression #-}

viewApplication :: Expression -> [Expression]
viewApplication (Application ef ex) = ex : viewApplication ef
viewApplication x = [x]

-- | Convert from an expression to an abstract syntax tree.
--
-- This function will use let, and applications and abstractions of multiple values when possible.
expr2ast :: Expression -> AbstractSyntax
expr2ast (Lets defs body) = AST.Let (fromList $ map (second expr2ast) defs) $ expr2ast body
expr2ast (Abstractions names body) = AST.Abstraction (fromList names) $ expr2ast body
expr2ast (Applications exprs) = AST.Application $ fromList $ map expr2ast $ reverse exprs
expr2ast (Variable name) = AST.Variable name

instance TextShow Expression where
  showb = showb . expr2ast

instance Show Expression where
  show = T.unpack . showt
