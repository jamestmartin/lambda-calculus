module LambdaCalculus.Expression
  ( Expression (..), ExpressionF (..)
  , ast2expr, expr2ast
  , pattern Lets, pattern Abstractions, pattern Applications
  , viewLet, viewAbstraction, viewApplication
  ) where

-- The definition of Expression is in its own file because:
-- * Expression and AbstractSyntax should not be in the same file
-- * Expression's `show` definition depends on AbstractSyntax's show definition,
--   which means that `ast2expr` and `expr2ast` can't be in AbstractSyntax
--   because of mutually recursive modules
-- * I don't want to clutter the module focusing on the actual evaluation
--   with all of these irrelevant conversion operators.

import Data.Bifunctor (first)
import Data.Functor.Foldable (ana, cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List (foldl1')
import Data.List.NonEmpty (NonEmpty ((:|)), fromList, toList)
import Data.Text (Text)
import Data.Text qualified as T
import LambdaCalculus.Parser.AbstractSyntax (AbstractSyntax)
import LambdaCalculus.Parser.AbstractSyntax qualified as AST
import TextShow (TextShow, showb, showt)

data Expression
  = Variable Text
  -- | Function application: `(f x)`.
  | Application Expression Expression
  -- | Lambda abstraction: `(λx. e)`.
  | Abstraction Text Expression
  -- | A continuation. This is identical to a lambda abstraction,
  -- with the exception that it performs the side-effect of
  -- deleting the current continuation.
  --
  -- Continuations do not have any corresponding surface-level syntax.
  | Continuation Expression
  deriving Eq

makeBaseFunctor ''Expression

-- | Convert from an abstract syntax tree to an expression.
ast2expr :: AbstractSyntax -> Expression
ast2expr = cata \case
  AST.VariableF name -> Variable name
  AST.ApplicationF es -> case es of
    x :| [] -> x
    xs -> foldl1' Application (toList xs)
  AST.AbstractionF names body -> foldr Abstraction body (toList names)
  AST.LetF defs body ->
    let letExpr name val body' = Application (Abstraction name body') val
    in foldr (uncurry letExpr) body defs

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
pattern Applications exprs <- (viewApplication -> exprs@(_:_:_))

{-# COMPLETE Abstractions, Applications, Continuation, Variable :: Expression #-}

viewApplication :: Expression -> [Expression]
viewApplication (Application ef ex) = viewApplication ef ++ [ex]
viewApplication x = [x]

-- | Convert from an expression to an abstract syntax tree.
--
-- This function will use let, and applications and abstractions of multiple values when possible.
expr2ast :: Expression -> AbstractSyntax
expr2ast = ana \case
  Lets defs body -> AST.LetF (fromList defs) body
  Abstractions names body -> AST.AbstractionF (fromList names) body
  Applications exprs -> AST.ApplicationF $ fromList exprs
  Continuation body -> AST.AbstractionF ("!" :| []) body
  Variable name -> AST.VariableF name

instance TextShow Expression where
  showb = showb . expr2ast

instance Show Expression where
  show = T.unpack . showt
