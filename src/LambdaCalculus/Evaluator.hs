module LambdaCalculus.Evaluator
  ( Expr (..), Ctr (..), Pat, ExprF (..), PatF (..), VoidF, UnitF (..), Text
  , Eval, EvalExpr, EvalX, EvalXF (..)
  , pattern AppFE, pattern CtrE, pattern CtrFE
  , pattern ContE, pattern ContFE, pattern CallCCE, pattern CallCCFE
  , eval, evalTrace, evalTraceGlobal
  ) where

import LambdaCalculus.Evaluator.Base
import LambdaCalculus.Evaluator.Continuation

import Control.Monad.Except (MonadError, ExceptT, throwError, runExceptT)
import Control.Monad.Loops (iterateM_)
import Control.Monad.State (MonadState, evalState, modify', state, put, gets)
import Control.Monad.Writer (runWriterT, tell)
import Data.HashMap.Strict qualified as HM
import Data.Void (Void, absurd)

isReducible :: EvalExpr -> Bool
-- Applications of function type constructors
isReducible (App (Abs _ _) _) = True
isReducible (App (ContE _) _) = True
isReducible (App  CallCCE  _) = True
-- Pattern matching of data
isReducible (App (Case _) ex) = isData ex || isReducible ex
-- Reducible subexpressions
isReducible (App ef       ex) = isReducible ef || isReducible ex
isReducible _                 = False

lookupPat :: Ctr -> [Pat phase] -> Pat phase
lookupPat ctr = foldr lookupCtr' (error "Constructor not found")
  where
    lookupCtr' p@(Pat ctr' _ _) p'
      | ctr == ctr' = p
      | otherwise = p'

isData :: EvalExpr -> Bool
isData (CtrE _) = True
isData (App ef _) = isData ef
isData _ = False

toData :: EvalExpr -> (Ctr, [EvalExpr])
toData (CtrE ctr) = (ctr, [])
toData (App ef ex) = (++ [ex]) <$> toData ef
toData _ = error "Matched expression is not data"

push :: MonadState Continuation m => ContinuationCrumb -> m ()
push c = modify' (c :)

pop :: MonadState Continuation m => m (Maybe ContinuationCrumb)
pop = state \case
  [] -> (Nothing, [])
  (crumb:k) -> (Just crumb, k)

ret :: (MonadError EvalExpr m, MonadState Continuation m) => EvalExpr -> m EvalExpr
ret e = pop >>= maybe (throwError e) (pure . continue1 e)

fromLeft :: Either a Void -> a
fromLeft (Left x) = x
fromLeft (Right x) = absurd x

-- | Iteratively call an action until it 'throws' a return value.
loop :: Monad m => (a -> ExceptT b m a) -> a -> m b
loop f = fmap fromLeft . runExceptT . iterateM_ f

-- | A call-by-value expression evaluator.
evaluatorStep :: (MonadError EvalExpr m, MonadState Continuation m) => EvalExpr -> m EvalExpr
evaluatorStep = \case
  unmodified@(App ef ex)
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
        Abs name body ->
          pure $ substitute1 name ex body
        Case pats ->
          if isData ex
          then do
            let (ctr, xs) = toData ex
            let Pat _ ns e = lookupPat ctr pats
            pure $ substitute (HM.fromList $ zip ns xs) e
          else ret unmodified
        -- perform continuation calls if possible...
        ContE body -> do
          put []
          pure $ substitute1 "!" ex body
        -- capture the current continuation if requested...
        CallCCE -> do
          k <- gets $ continue (Var "!")
          pure $ App ex (ContE k)
        -- otherwise the value is irreducible and we can continue evaluation.
        _ -> ret unmodified
  -- Neither abstractions, constructors nor variables are reducible.
  e -> ret e

eval :: EvalExpr -> EvalExpr
eval = flip evalState [] . loop evaluatorStep

-- | Trace each evaluation step.
evalTrace :: EvalExpr -> (EvalExpr, [EvalExpr])
evalTrace = flip evalState [] . runWriterT . loop \e -> do
  tell [e]
  evaluatorStep e

-- | Trace each evaluation step, including the *entire* continuation of each step.
evalTraceGlobal :: EvalExpr -> (EvalExpr, [EvalExpr])
evalTraceGlobal = flip evalState [] . runWriterT . loop \e -> do
  e' <- gets (continue e)
  tell [e']
  evaluatorStep e
