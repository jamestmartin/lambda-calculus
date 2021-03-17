module LambdaCalculus
  ( module LambdaCalculus.Evaluator
  , module LambdaCalculus.Expression
  , module LambdaCalculus.Syntax
  , parseEval, unparseEval
  ) where

import LambdaCalculus.Evaluator
import LambdaCalculus.Expression
import LambdaCalculus.Syntax

parseEval :: Text -> Either ParseError EvalExpr
parseEval = fmap ast2eval . parseAST

unparseEval :: EvalExpr -> Text
unparseEval = unparseAST . simplify . eval2ast
