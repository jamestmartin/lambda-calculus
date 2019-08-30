module LambdaCalculus.Combinators where

import LambdaCalculus.Representation (IsExpr, fromStandard)
import LambdaCalculus.Representation.Standard

-- | The `I` combinator, representing the identify function `λx. x`.
i :: IsExpr expr => expr
i = fromStandard $ Abstraction "x" $ Variable "x"
