module LambdaCalculus.Evaluator
  ( Expr (..), Ctr (..), Pat, ExprF (..), PatF (..), VoidF, UnitF (..), Text
  , Eval, EvalExpr, EvalX, EvalXF (..)
  , pattern AppFE, pattern CtrE, pattern CtrFE
  , pattern Cont, pattern ContF, pattern CallCC, pattern CallCCF
  , eval, traceEval, substitute, alphaConvert
  ) where

import LambdaCalculus.Evaluator.Base
import LambdaCalculus.Evaluator.Continuation

import Control.Monad (forM)
import Control.Monad.Except (MonadError, ExceptT, throwError, runExceptT)
import Control.Monad.State (MonadState, State, evalState,  modify', state, put, gets)
import Control.Monad.Writer (runWriterT, tell)
import Data.Foldable (fold)
import Data.Functor.Foldable (cata, para, embed)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Stream qualified as S
import Data.Text qualified as T
import Data.Void (Void, absurd)

-- | Free variables are variables which are present in an expression but not bound by any abstraction.
freeVars :: EvalExpr -> HashSet Text
freeVars = cata \case
  VarF n   -> HS.singleton n
  AbsF n e -> HS.delete  n  e
  ContF e  -> HS.delete "!" e
  CaseF ps -> foldMap (\(Pat _ ns e) -> HS.difference e (HS.fromList ns)) ps
  e -> fold e

-- | Bound variables are variables which are bound by any form of abstraction in an expression.
boundVars :: EvalExpr -> HashSet Text
boundVars = cata \case
  AbsF n e -> HS.insert  n  e
  ContF e  -> HS.insert "!" e
  CaseF ps -> foldMap (\(Pat _ ns e) -> HS.union (HS.fromList ns) e) ps
  e -> fold e

-- | Vars that occur anywhere in an experession, bound or free.
usedVars :: EvalExpr -> HashSet Text
usedVars x = HS.union (freeVars x) (boundVars x)

-- | Substitution is the process of replacing all free occurrences of a variable in one expression with another expression.
substitute :: Text -> EvalExpr -> EvalExpr -> EvalExpr
substitute var val = unsafeSubstitute var val . alphaConvert (freeVars val)

-- | Substitution is only safe if the bound variables in the body
-- are disjoint from the free variables in the argument;
-- this function makes an expression body safe for substitution
-- by replacing the bound variables in the body
-- with completely new variables which do not occur in either expression
-- (without changing any *free* variables in the body, of course).
alphaConvert :: HashSet Text -> EvalExpr -> EvalExpr
alphaConvert ctx e_ = evalState (alphaConverter e_) $ HS.union ctx (usedVars e_)
  where
    alphaConverter :: EvalExpr -> State (HashSet Text) EvalExpr
    alphaConverter = cata \case
      e
        | AbsF n e' <- e, n `HS.member` ctx -> do
            n' <- fresh n
            e'' <- e'
            pure $ Abs n' $ replace n n' e''
        -- | TODO: Only replace the names that *have* to be replaced.
        | CaseF ps <- e, any (any (`HS.member` ctx) . patNames) ps ->
            Case <$> forM ps \(Pat ctr ns e') -> do
              ns' <- mapM fresh ns
              e'' <- e'
              pure $ Pat ctr ns' $ foldr (uncurry replace) e'' (zip ns ns')
        | otherwise -> embed <$> sequenceA e

    -- | Create a new name which is not used anywhere else.
    fresh :: Text -> State (HashSet Text) Text
    fresh n = state \ctx' ->
      let n' = S.head $ S.filter (not . (`HS.member` ctx')) names
      in (n', HS.insert n' ctx')
      where names = S.iterate (`T.snoc` '\'') n

-- | Replace a name with an entirely new name in all contexts.
-- This will only give correct results if
-- the new name does not occur anywhere in the expression.
replace :: Text -> Text -> EvalExpr -> EvalExpr
replace name name' = cata \case
  e
    | VarF name2    <- e, name == name2 -> Var name'
    | AbsF name2 e' <- e, name == name2 -> Abs name' e'
    | CaseF ps <- e -> Case $ flip map ps \(Pat ctr ns e') -> Pat ctr (replace' ns) e'
    | otherwise -> embed e
  where
    replace' = map \case
        n
          | n == name -> name'
          | otherwise -> n

-- | Substitution which does *not* avoid variable capture;
-- it only gives the correct result if the bound variables in the body
-- are disjoint from the free variables in the argument.
unsafeSubstitute :: Text -> EvalExpr -> EvalExpr -> EvalExpr
unsafeSubstitute var val = para \case
  e'
    | VarF  var2   <- e', var == var2 -> val
    | AbsF  var2 _ <- e', var == var2 -> unmodified e'
    | ContF      _ <- e', var == "!"  -> unmodified e'
    | CaseF ps <- e' -> Case $ flip map ps \(Pat ctr ns (unmod, sub)) ->
        Pat ctr ns if var `elem` ns then unmod else sub
    | otherwise -> substituted e'
  where
    substituted, unmodified :: EvalExprF (EvalExpr, EvalExpr) -> EvalExpr
    substituted = embed . fmap snd
    unmodified  = embed . fmap fst

isReducible :: EvalExpr -> Bool
isReducible = snd . cata \case
  AppFE ctr args -> active ctr [args]
  AbsF _ _       -> passive
  ContF _        -> passive
  CaseF _        -> passive
  CallCCF        -> passive
  CtrFE _        -> constant
  VarF _         -> constant
  where
    -- | Constants are irreducible in any context.
    constant = (False, False)
    -- | Passive expressions are reducible only if an active expression is applied to them.
    passive = (True, False)
    -- | Active expressions are reducible if they are applied to a constructor or their arguments are reducible.
    active ctr args = (False, fst ctr || snd ctr || any snd args)

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
          pure $ substitute name ex body
        Case pats
          | isData ex -> do
              let (ctr, xs) = toData ex
              let Pat _ ns e = lookupPat ctr pats
              pure $ foldr (uncurry substitute) e (zip ns xs)
          | otherwise -> ret unmodified
        -- perform continuation calls if possible...
        Cont body -> do
          put []
          pure $ substitute "!" ex body
        -- capture the current continuation if requested...
        CallCC -> do
          k <- gets $ continue (Var "!")
          pure $ App ex (Cont k)
        -- otherwise the value is irreducible and we can continue evaluation.
        _ -> ret unmodified
  -- Neither abstractions, constructors nor variables are reducible.
  e -> ret e

eval :: EvalExpr -> EvalExpr
eval = flip evalState [] . loop evaluatorStep

traceEval :: EvalExpr -> (EvalExpr, [EvalExpr])
traceEval = flip evalState [] . runWriterT . loop \e -> do
  -- You can also use `gets (continue e)` to print the *entire* expression each step.
  -- This is a trade-off because it becomes much harder to pick out what changed from the rest of the expression.
  e' <- gets (continue e)
  tell [e']
  evaluatorStep e
