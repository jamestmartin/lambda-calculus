module Ivo
  ( module Ivo.Evaluator
  , module Ivo.Expression
  , module Ivo.Syntax
  , module Ivo.Types
  , parseCheck, parseEval, unparseCheck, unparseEval
  ) where

import Ivo.Evaluator
import Ivo.Expression
import Ivo.Syntax
import Ivo.Types

parseCheck :: Text -> Either ParseError CheckExpr
parseCheck = fmap ast2check . parseAST

parseEval :: Text -> Either ParseError EvalExpr
parseEval = fmap ast2eval . parseAST

unparseCheck :: CheckExpr -> Text
unparseCheck = unparseAST . simplify . check2ast

unparseEval :: EvalExpr -> Text
unparseEval = unparseAST . simplify . eval2ast
