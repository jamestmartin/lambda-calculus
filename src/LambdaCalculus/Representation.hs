module LambdaCalculus.Representation where

import Data.Functor.Foldable (cata)
import Data.HashSet (HashSet, singleton, union, delete)
import LambdaCalculus.Representation.Standard

-- | `expr` is a representation of a /closed/ lambda calculus expression.
class IsExpr expr where
  -- | Convert an expression to the standard representation.
  toStandard :: expr -> Expression

  -- | Convert an expression from the standard representation.
  fromStandard :: Expression -> expr

  -- | Convert an expression from one representation to another.
  convert :: IsExpr repr => expr -> repr
  convert = fromStandard . toStandard

  -- | Retrieve the free variables in an expression.
  freeVariables :: expr -> HashSet String
  freeVariables = freeVariables . toStandard

instance IsExpr Expression where
  toStandard    = id
  fromStandard  = id
  convert       = fromStandard
  freeVariables = cata \case
    VariableF name         -> singleton name
    AbstractionF name body -> name `delete` body
    ApplicationF fe xe     -> fe `union` xe
