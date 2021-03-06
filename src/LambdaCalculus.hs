module LambdaCalculus
  ( module LambdaCalculus.Expression
  , eagerEval, lazyEval
  ) where

import Data.List (elemIndex, find)
import Data.Maybe (fromJust)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Text (Text)
import Data.Text qualified as T
import LambdaCalculus.Expression (Expression (..))

-- | Free variables are variables which are present in an expression but not bound by any abstraction.
freeVariables :: Expression -> HashSet Text
freeVariables (Variable variable) = HS.singleton variable
freeVariables (Application ef ex) = freeVariables ef `HS.union` freeVariables ex
freeVariables (Abstraction variable body) = HS.delete variable $ freeVariables body

-- | Return True if the given variable is free in the given expression.
freeIn :: Text -> Expression -> Bool
freeIn var1 (Variable var2) = var1 == var2
freeIn var (Application ef ex) = var `freeIn` ef && var `freeIn` ex
freeIn var1 (Abstraction var2 body) = var1 == var2 || var1 `freeIn` body

-- | Bound variables are variables which are bound by any abstraction in an expression.
boundVariables :: Expression -> HashSet Text
boundVariables (Variable _) = HS.empty
boundVariables (Application ef ex) = boundVariables ef `HS.union` boundVariables ex
boundVariables (Abstraction variable body) = HS.insert variable $ boundVariables body

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
    alphaEquivalent' _ _ _ _ = False

    -- | The binding site of a variable is either the index of its binder
    -- or, if it is unbound, the name of the free variable.
    bindingSite :: [Text] -> Text -> Either Text Int
    bindingSite ctx var = maybeToRight var $ var `elemIndex` ctx
      where maybeToRight :: b -> Maybe a -> Either b a
            maybeToRight default_ = maybe (Left default_) Right

-- | Substitution is the process of replacing all free occurrences of a variable in one expression with another expression.
substitute :: Text -> Expression -> Expression -> Expression
substitute var1 value unmodified@(Variable var2)
    | var1 == var2 = value
  | otherwise = unmodified
substitute var value (Application ef ex)
  = Application (substitute var value ef) (substitute var value ex)
substitute var1 value unmodified@(Abstraction var2 body)
  | var1 == var2 = unmodified
  | otherwise = Abstraction var2' $ substitute var1 value $ alphaConvert var2 var2' body
  where
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

-- | Returns True if the top-level expression is reducible by beta-reduction.
betaRedex :: Expression -> Bool
betaRedex (Application (Abstraction _ _) _) = True
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
normal (Application (Abstraction _ _) _) = False
-- The expression is eta-reducible.
normal (Abstraction var1 (Application fe (Variable var2)))
  = var1 /= var2 || var1 `freeIn` fe
normal (Application ef ex) = normal ef && normal ex
normal _ = True

-- | In an expression in weak head normal form, reductions to the function have been applied,
-- but not all reductions to the parameter have been applied.
-- This is the result of applying lazy evaluation.
whnf :: Expression -> Bool
whnf (Application (Abstraction _ _) _) = False
whnf (Abstraction var1 (Application fe (Variable var2)))
  = var1 /= var2 || var1 `freeIn` fe
whnf (Application ef _) = whnf ef
whnf _ = True

eval :: (Expression -> Expression) -> Expression -> Expression
eval strategy = eval'
  where
    eval' :: Expression -> Expression
    eval' (Application ef ex) =
      case ef' of
        -- Beta-reduction
        Abstraction var body -> eval' $ substitute var ex' body
        _ -> Application ef' ex'
      where
        ef' = eval' ef
        ex' = strategy ex
    eval' unmodified@(Abstraction var1 (Application ef (Variable var2)))
      -- Eta-reduction
      | var1 == var2 && not (var1 `freeIn` ef) = eval' ef
      | otherwise = unmodified
    eval' x = x

-- | Reduce an expression to normal form.
eagerEval :: Expression -> Expression
eagerEval = eval eagerEval

-- | Reduce an expression to weak head normal form.
lazyEval :: Expression -> Expression
lazyEval = eval id
