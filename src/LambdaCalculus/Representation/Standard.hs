module LambdaCalculus.Representation.Standard where

import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)

data Expression = Variable String
                | Abstraction String Expression
                | Application Expression Expression

makeBaseFunctor ''Expression

instance Show Expression where
  -- For a more sophisticated printing mechanism,
  -- consider converting to 'LambdaCalculus.Representation.AbstractSyntax.Expression' first.
  show = cata \case
    VariableF name -> name
    AbstractionF name body -> "(λ" ++ name ++ ". " ++ body ++ ")"
    ApplicationF fe xe -> "(" ++ fe ++ " " ++ xe ++ ")"
