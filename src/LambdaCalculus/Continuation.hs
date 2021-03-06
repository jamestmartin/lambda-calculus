module LambdaCalculus.Continuation
  ( Continuation, continue, continue1
  , ContinuationCrumb (ApplyTo, AppliedTo, AbstractedOver)
  ) where

import Data.List (foldl')
import Data.Text (Text)
import LambdaCalculus.Expression

data ContinuationCrumb
  -- | The one-hole context of a function application: `(_ e)`
  = ApplyTo Expression
  -- | The one-hole context of the argument to a function application: `(f _)`
  | AppliedTo Expression
  -- | The one-hole context of the body of a lambda abstraction: `(λx. _)`
  | AbstractedOver Text

type Continuation = [ContinuationCrumb]

continue1 :: Expression -> ContinuationCrumb -> Expression
continue1 e (ApplyTo x) = Application e x
continue1 e (AppliedTo x) = Application x e
continue1 e (AbstractedOver name) = Abstraction name e

continue :: Expression -> Continuation -> Expression
continue = foldl' continue1
