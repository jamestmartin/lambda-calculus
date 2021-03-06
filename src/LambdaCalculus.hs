module LambdaCalculus
  ( module LambdaCalculus.Expression
  , eval
  ) where

import Control.Monad.State (State, evalState, modify', state, put, get)
import Data.List (elemIndex, find)
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

-- | Bound variables are variables which are bound by any abstraction in an expression.
boundVariables :: Expression -> HashSet Text
boundVariables = foldExpr (const HS.empty) HS.union HS.insert

-- | Return True if the given variable is free in the given expression.
freeIn :: Text -> Expression -> Bool
freeIn var = foldExpr (== var) (&&) (\name body -> (name == var) || body)

-- | A closed expression is an expression with no free variables.
-- Closed expressions are also known as combinators and are equivalent to terms in combinatory logic.
closed :: Expression -> Bool
closed = HS.null . freeVariables

-- | Alpha-equivalent terms differ only by the names of bound variables,
-- i.e. one can be converted to the other using only alpha-conversion.
alphaEquivalent :: Expression -> Expression -> Bool
alphaEquivalent = alphaEquivalent' [] []
  where
    alphaEquivalent' :: [Text] -> [Text] -> Expression -> Expression -> Bool
    alphaEquivalent' ctx1 ctx2 (Variable v1) (Variable v2)
      -- Two variables are alpha-equivalent if they are bound in the same location.
      = bindingSite ctx1 v1 == bindingSite ctx2 v2
    alphaEquivalent' ctx1 ctx2 (Application ef1 ex1) (Application ef2 ex2)
      -- Two applications are alpha-equivalent if their components are alpha-equivalent.
      = alphaEquivalent' ctx1 ctx2 ef1 ef2
      && alphaEquivalent' ctx1 ctx2 ex1 ex2
    alphaEquivalent' ctx1 ctx2 (Abstraction v1 b1) (Abstraction v2 b2)
      -- Two abstractions are alpha-equivalent if their bodies are alpha-equivalent.
      = alphaEquivalent' (v1 : ctx1) (v2 : ctx2) b1 b2
    alphaEquivalent' ctx1 ctx2 (Continuation v1 b1) (Continuation v2 b2)
      = alphaEquivalent' (v1 : ctx1) (v2 : ctx2) b1 b2
    alphaEquivalent' _ _ _ _ = False

    -- | The binding site of a variable is either the index of its binder
    -- or, if it is unbound, the name of the free variable.
    bindingSite :: [Text] -> Text -> Either Text Int
    bindingSite ctx var = maybeToRight var $ var `elemIndex` ctx
      where maybeToRight :: b -> Maybe a -> Either b a
            maybeToRight default_ = maybe (Left default_) Right

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

-- | Returns True if the top-level expression is reducible by beta-reduction.
betaRedex :: Expression -> Bool
betaRedex (Application (quickHack -> (Abstraction _ _)) _) = True
betaRedex _ = False

-- | Returns True if the top-level expression is reducible by eta-reduction.
etaRedex :: Expression -> Bool
etaRedex (Abstraction var1 (Application ef (Variable var2)))
  = var1 /= var2 || var1 `freeIn` ef
etaRedex _ = False

-- | In an expression in normal form, all reductions that can be applied have been applied.
-- This is the result of applying eager evaluation.
normal :: Expression -> Bool
-- The expression is beta-reducible.
normal (Application (quickHack -> (Abstraction _ _)) _) = False
-- The expression is eta-reducible.
normal (quickHack -> (Abstraction var1 (Application fe (Variable var2))))
  = var1 /= var2 || var1 `freeIn` fe
normal (Application ef ex) = normal ef && normal ex
normal _ = True

-- | In an expression in weak head normal form, reductions to the function have been applied,
-- but not all reductions to the parameter have been applied.
-- This is the result of applying lazy evaluation.
whnf :: Expression -> Bool
whnf (Application (quickHack -> (Abstraction _ _)) _) = False
whnf (quickHack -> (Abstraction var1 (Application fe (Variable var2))))
  = var1 /= var2 || var1 `freeIn` fe
whnf (Application ef _) = whnf ef
whnf _ = True

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
        k <- continue (Variable "!") <$> get
        evaluator (Application ex (Continuation "!" k))
      -- otherwise the value is irreducible and we can continue evaluation.
      _ -> ret unmodified
-- Neither abstractions nor variables are reducible.
evaluator e = ret e

eval :: Expression -> Expression
eval = flip evalState [] . evaluator
