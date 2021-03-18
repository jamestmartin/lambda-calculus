module LambdaCalculus
  ( module LambdaCalculus.Evaluator
  , module LambdaCalculus.Expression
  , module LambdaCalculus.Syntax
  , module LambdaCalculus.Types
  , parseCheck, parseEval, unparseCheck, unparseEval
  ) where

import LambdaCalculus.Evaluator
import LambdaCalculus.Expression
import LambdaCalculus.Syntax
import LambdaCalculus.Types

parseCheck :: Text -> Either ParseError CheckExpr
parseCheck = fmap ast2check . parseAST

parseEval :: Text -> Either ParseError EvalExpr
parseEval = fmap ast2eval . parseAST

unparseCheck :: CheckExpr -> Text
unparseCheck = unparseAST . simplify . check2ast

unparseEval :: EvalExpr -> Text
unparseEval = unparseAST . simplify . eval2ast
