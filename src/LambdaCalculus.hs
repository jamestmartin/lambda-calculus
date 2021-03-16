module LambdaCalculus
  ( module LambdaCalculus.Expression
  , eval
  ) where

import Control.Monad.State (State, evalState, modify', state, put, gets)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Text (Text)
import Data.Text qualified as T
import LambdaCalculus.Continuation
import LambdaCalculus.Expression (Expression (..), foldExpr)

-- | Free variables are variables which are present in an expression but not bound by any abstraction.
freeVariables :: Expression -> HashSet Text
freeVariables = foldExpr HS.singleton HS.union HS.delete

-- FIXME
quickHack :: Expression -> Expression
quickHack (Continuation name body) = Abstraction name body
quickHack e = e

-- | Substitution is the process of replacing all free occurrences of a variable in one expression with another expression.
substitute :: Text -> Expression -> Expression -> Expression
substitute var1 value unmodified@(Variable var2)
    | var1 == var2 = value
  | otherwise = unmodified
substitute var value (Application ef ex)
  = Application (substitute var value ef) (substitute var value ex)
substitute var1 value unmodified@(quickHack -> Abstraction var2 body)
  | var1 == var2 = unmodified
  | otherwise = constructor var2' $ substitute var1 value $ alphaConvert var2 var2' body
  where
    constructor = case unmodified of
      Abstraction _ _ -> Abstraction
      Continuation _ _ -> Continuation
      _ -> error "impossible"

    var2' :: Text
    var2' = escapeName (freeVariables value) var2

    alphaConvert :: Text -> Text -> Expression -> Expression
    alphaConvert oldName newName expr = substitute oldName (Variable newName) expr
    -- | Generate a new name which isn't present in the set, based on the old name.
    escapeName :: HashSet Text -> Text -> Text
    escapeName env name = fromJust $ find (not . free) names
      where names :: [Text]
            names = name : map (`T.snoc` '\'') names

            free :: Text -> Bool
            free = (`HS.member` env)
substitute _ _ _ = error "impossible"

type EvaluatorM a = State Continuation a
type Evaluator = Expression -> EvaluatorM Expression

isReducible :: Expression -> Bool
isReducible (Application (quickHack -> (Abstraction _ _)) _) = True
isReducible (Application (Variable "callcc") _) = True
isReducible (Application ef ex) = isReducible ef || isReducible ex
isReducible _ = False

push :: ContinuationCrumb -> EvaluatorM ()
push c = modify' (c :)

pop :: EvaluatorM (Maybe ContinuationCrumb)
pop = state \case
  [] -> (Nothing, [])
  (crumb:k) -> (Just crumb, k)

ret :: Expression -> EvaluatorM Expression
ret e = pop >>= maybe (pure e) (evaluator . continue1 e)

-- | A call-by-value expression evaluator.
evaluator :: Evaluator
evaluator unmodified@(Application ef ex)
  -- First reduce the argument...
  | isReducible ex = do
      push (AppliedTo ef)
      evaluator ex
  -- then reduce the function...
  | isReducible ef = do
      push (ApplyTo ex)
      evaluator ef
  | otherwise = case ef of
      -- perform beta reduction if possible...
      Abstraction name body ->
        evaluator $ substitute name ex body
      -- perform continuation calls if possible...
      Continuation name body -> do
        put []
        evaluator $ substitute name ex body
      -- capture the current continuation if requested...
      Variable "callcc" -> do
        -- Don't worry about variable capture here for now.
        k <- gets $ continue (Variable "!")
        evaluator (Application ex (Continuation "!" k))
      -- otherwise the value is irreducible and we can continue evaluation.
      _ -> ret unmodified
-- Neither abstractions nor variables are reducible.
evaluator e = ret e

eval :: Expression -> Expression
eval = flip evalState [] . evaluator
