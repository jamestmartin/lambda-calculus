module LambdaCalculus.Expression
  ( Expr (..), Ctr (..), Pat, ExprF (..), PatF (..), DefF (..), VoidF, UnitF (..), Text
  , Eval, EvalExpr, EvalX, EvalXF (..), Identity (..)
  , pattern AppFE, pattern CtrE, pattern CtrFE,
    pattern Cont, pattern ContF, pattern CallCC, pattern CallCCF
  , Parse, AST, ASTF, ASTX, ASTXF (..), NonEmptyDefFs (..), NonEmpty (..), simplify
  , pattern LetFP, pattern PNat, pattern PNatF, pattern PList, pattern PListF
  , pattern PChar, pattern PCharF, pattern PStr, pattern PStrF
  , ast2eval, eval2ast
  ) where

import LambdaCalculus.Evaluator.Base
import LambdaCalculus.Evaluator (alphaConvert, substitute)
import LambdaCalculus.Syntax.Base

import Data.Functor.Foldable (cata, hoist)
import Data.HashSet qualified as HS
import Data.List (foldl')
import Data.List.NonEmpty (toList)
import Data.Text (unpack)

-- | Convert from an abstract syntax tree to an evaluator expression.
ast2eval :: AST -> EvalExpr
ast2eval = substitute "callcc" CallCC . cata \case
  VarF name -> Var name
  AppF ef exs -> foldl' App ef $ toList exs
  AbsF ns e -> foldr Abs e $ toList ns
  LetF ds e ->
    let letExpr name val body' = App (Abs name body') val
    in foldr (uncurry letExpr) e $ getNonEmptyDefFs ds
  CtrF ctr es -> foldl' App (CtrE ctr) es
  CaseF ps -> Case ps
  PNatF n -> int2ast n
  PListF es -> mkList es
  PStrF s -> mkList $ map (App (CtrE CChar) . int2ast . fromEnum) $ unpack s
  PCharF c -> App (CtrE CChar) (int2ast $ fromEnum c)
  where
    int2ast :: Int -> EvalExpr
    int2ast 0 = CtrE CZero
    int2ast n = App (CtrE CSucc) (int2ast (n - 1))

    mkList :: [EvalExpr] -> EvalExpr
    mkList = foldr (App . App (CtrE CCons)) (CtrE CNil)

-- | Convert from an evaluator expression to an abstract syntax tree.
eval2ast :: EvalExpr -> AST
-- Because all `ast2eval` replaces all free instances of `callcc`,
-- all instances of `callcc` must be bound;
-- therefore, we are free to alpha convert them,
-- freeing the name `callcc` for us to use for the built-in again.
eval2ast = hoist go . alphaConvert (HS.singleton "callcc")
  where
    go :: EvalExprF r -> ASTF r
    go = \case
      VarF name -> VarF name
      CallCCF -> VarF "callcc"
      AppFE ef ex -> AppF ef (ex :| [])
      AbsF n e -> AbsF (n :| []) e
      CtrFE ctr -> CtrF ctr []
      CaseF ps -> CaseF ps
      ContF e -> AbsF ("!" :| []) e
