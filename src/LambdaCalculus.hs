module LambdaCalculus
  ( module LambdaCalculus.Expression
  , eval, traceEval
  ) where

import Control.Monad.Except (MonadError, ExceptT, throwError, runExceptT)
import Control.Monad.State (MonadState, State, evalState,  modify', state, put, gets)
import Control.Monad.Writer (runWriterT, tell)
import Data.Functor.Foldable (cata, para, project, embed)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Stream (Stream)
import Data.Stream qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void, absurd)
import LambdaCalculus.Continuation
import LambdaCalculus.Expression (Expression (..), ExpressionF (..))

-- | Free variables are variables which are present in an expression but not bound by any abstraction.
freeVariables :: Expression -> HashSet Text
freeVariables = cata \case
  VariableF name -> HS.singleton name
  ApplicationF e1 e2 -> HS.union e1 e2
  AbstractionF n e -> HS.delete n e
  ContinuationF e -> HS.delete "!" e

-- | Bound variables are variables which are bound by any form of abstraction in an expression.
boundVariables :: Expression -> HashSet Text
boundVariables = cata \case
  VariableF _ -> HS.empty
  ApplicationF e1 e2 -> HS.union e1 e2
  AbstractionF n e -> HS.insert n e
  ContinuationF e -> HS.insert "!" e

-- | Variables that occur anywhere in an experession, bound or free.
usedVariables :: Expression -> HashSet Text
usedVariables x = HS.union (freeVariables x) (boundVariables x)

-- | Generate a stream of new variables which are not in the set of provided variables.
freshVariables :: HashSet Text -> Stream Text
freshVariables ctx = S.filter (not . flip HS.member ctx) $ S.fromList $ fmap T.pack $ (:) <$> ['a'..'z'] <*> map show [0 :: Int ..]

-- | Create a new variable which is not used anywhere else.
fresh :: State (Stream Text) Text
fresh = state project

-- | Substitution is the process of replacing all free occurrences of a variable in one expression with another expression.
substitute :: Text -> Expression -> Expression -> Expression
substitute var val = unsafeSubstitute var val . alphaConvert (usedVariables val)

-- | Rename the bound variables in `e` so they do not overlap any variables used in `ctx`.
--
-- This is used as part of substitution when substituting `val` with free variables `ctx` into `e`,
-- because it prevents any of the binders in `e` from accidentally capturing a free variable in `ctx`.
alphaConvert :: HashSet Text -> Expression -> Expression
alphaConvert ctx e_ = evalState (rename e_) $ freshVariables $ HS.union (usedVariables e_) ctx
  where
    rename :: Expression -> State (Stream Text) Expression
    rename = cata \case
      VariableF var -> pure $ Variable var
      ApplicationF ef ex -> Application <$> ef <*> ex
      ContinuationF e -> Continuation <$> e
      AbstractionF n e
        | HS.member n ctx -> do
            n' <- fresh
            Abstraction n' . unsafeSubstitute n (Variable n') <$> e
        | otherwise -> Abstraction n <$> e

-- | Substitution with the assumption that no free variables in the value are bound in the expression.
unsafeSubstitute :: Text -> Expression -> Expression -> Expression
unsafeSubstitute var val = para \case
  e'
    | VariableF var2 <- e', var == var2 -> val
    | ApplicationF (_, ef) (_, ex) <- e' -> Application ef ex
    | ContinuationF (_, e) <- e', var /= "!" -> Continuation e
    | AbstractionF var2 (_, e) <- e', var /= var2 -> Abstraction var2 e
    | otherwise -> embed $ fmap fst e'

isReducible :: Expression -> Bool
isReducible = snd . cata \case
  ApplicationF ctr args -> eliminator ctr [args]
  VariableF "callcc" -> constructor
  AbstractionF _ _ -> constructor
  ContinuationF _ -> constructor
  VariableF _ -> constant
  where
    -- | Constants are irreducible in any context.
    constant = (False, False)
    -- | Constructors are reducible if an eliminator is applied to them.
    constructor = (True, False)
    -- | Eliminators are reducible if they are applied to a constructor or their arguments are reducible.
    eliminator ctr args = (False, fst ctr || snd ctr || any snd args)

push :: MonadState Continuation m => ContinuationCrumb -> m ()
push c = modify' (c :)

pop :: MonadState Continuation m => m (Maybe ContinuationCrumb)
pop = state \case
  [] -> (Nothing, [])
  (crumb:k) -> (Just crumb, k)

ret :: (MonadError Expression m, MonadState Continuation m) => Expression -> m Expression
ret e = pop >>= maybe (throwError e) (pure . continue1 e)

-- | Iteratively perform an action forever (or at least until it performs a control flow effect).
iterateM_ :: Monad m => (a -> m a) -> a -> m b
iterateM_ m = m' where m' x = m x >>= m'

fromLeft :: Either a Void -> a
fromLeft (Left x) = x
fromLeft (Right x) = absurd x

-- | Iteratively call an action until it 'throws' a return value.
loop :: Monad m => (a -> ExceptT b m a) -> a -> m b
loop f = fmap fromLeft . runExceptT . iterateM_ f

-- | A call-by-value expression evaluator.
evaluatorStep :: (MonadError Expression m, MonadState Continuation m) => Expression -> m Expression
evaluatorStep = \case
  unmodified@(Application ef ex)
    -- First reduce the argument...
    | isReducible ex -> do
        push (AppliedTo ef)
        pure ex
    -- then reduce the function...
    | isReducible ef -> do
        push (ApplyTo ex)
        pure ef
    | otherwise -> case ef of
        -- perform beta reduction if possible...
        Abstraction name body ->
          pure $ substitute name ex body
        -- perform continuation calls if possible...
        Continuation body -> do
          put []
          pure $ substitute "!" ex body
        -- capture the current continuation if requested...
        Variable "callcc" -> do
          -- Don't worry about variable capture here for now.
          k <- gets $ continue (Variable "!")
          pure $ Application ex (Continuation k)
        -- otherwise the value is irreducible and we can continue evaluation.
        _ -> ret unmodified
  -- Neither abstractions nor variables are reducible.
  e -> ret e

eval :: Expression -> Expression
eval = flip evalState [] . loop evaluatorStep

traceEval :: Expression -> (Expression, [Expression])
traceEval = flip evalState [] . runWriterT . loop \e -> do
  -- You can also use `gets (continue e)` to print the *entire* expression each step.
  -- This is a trade-off because it becomes much harder to pick out what changed from the rest of the expression.
  tell [e]
  evaluatorStep e
