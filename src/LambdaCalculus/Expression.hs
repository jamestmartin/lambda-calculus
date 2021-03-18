module LambdaCalculus.Expression
  ( Expr (..), Ctr (..), Pat, ExprF (..), PatF (..), DefF (..), VoidF, UnitF (..), Text
  , substitute, substitute1, rename, free, bound, used
  , Eval, EvalExpr, EvalX, EvalXF (..), Identity (..)
  , pattern AppFE, pattern CtrE, pattern CtrFE,
    pattern ContE, pattern ContFE, pattern CallCCE, pattern CallCCFE
  , Parse, AST, ASTF, ASTX, ASTXF (..), NonEmptyDefFs (..), NonEmpty (..), simplify
  , pattern LetFP, pattern PNat, pattern PNatF, pattern PList, pattern PListF
  , pattern PChar, pattern PCharF, pattern PStr, pattern PStrF, pattern HoleP, pattern HoleFP
  , Check, CheckExpr, CheckExprF, CheckX, CheckXF (..)
  , pattern AppFC, pattern CtrC, pattern CtrFC, pattern CallCCC, pattern CallCCFC
  , pattern FixC, pattern FixFC, pattern HoleC, pattern HoleFC
  , Type (..), TypeF (..), Scheme (..), tapp
  , ast2check, ast2eval, check2eval, check2ast, eval2ast
  , builtins
  ) where

import LambdaCalculus.Evaluator.Base
import LambdaCalculus.Syntax.Base
import LambdaCalculus.Types.Base

import Data.Functor.Foldable (cata, hoist)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (foldl')
import Data.List.NonEmpty (toList)
import Data.Text (unpack)

builtins :: HashMap Text CheckExpr
builtins = HM.fromList [("callcc", CallCCC), ("fix", FixC)]

-- | Convert from an abstract syntax tree to a typechecker expression.
ast2check :: AST -> CheckExpr
ast2check = substitute builtins . cata \case
  VarF name -> Var name
  AppF ef exs -> foldl' App ef $ toList exs
  AbsF ns e -> foldr Abs e $ toList ns
  LetF ds e ->
    let letExpr name val body' = App (Abs name body') val
    in foldr (uncurry letExpr) e $ getNonEmptyDefFs ds
  CtrF ctr es -> foldl' App (CtrC ctr) es
  CaseF ps -> Case ps
  PNatF n -> int2ast n
  PListF es -> mkList es
  PStrF s -> mkList $ map (App (CtrC CChar) . int2ast . fromEnum) $ unpack s
  PCharF c -> App (CtrC CChar) (int2ast $ fromEnum c)
  HoleFP -> HoleC
  where
    int2ast :: Int -> CheckExpr
    int2ast 0 = CtrC CZero
    int2ast n = App (CtrC CSucc) (int2ast (n - 1))

    mkList :: [CheckExpr] -> CheckExpr
    mkList = foldr (App . App (CtrC CCons)) (CtrC CNil)

-- | Convert from a typechecker expression to an evaluator expression.
check2eval :: CheckExpr -> EvalExpr
check2eval = cata \case
  VarF name -> Var name
  AppFC ef ex -> App ef ex
  AbsF n e -> Abs n e
  LetF (Def nx ex) e -> App (Abs nx e) ex
  CtrFC ctr -> CtrE ctr
  CaseF ps -> Case ps
  CallCCFC -> CallCCE
  FixFC -> z
  HoleFC -> omega
  where
    z, omega :: EvalExpr
    z = App omega $ Abs "fix" $ Abs "f" $ Abs "x" $
          App (App (Var "f") (App (App (Var "fix") (Var "fix")) (Var "f"))) (Var "x")
    omega = Abs "x" (App (Var "x") (Var "x"))

-- | Convert from an abstract syntax tree to an evaluator expression.
ast2eval :: AST -> EvalExpr
ast2eval = check2eval . ast2check

-- | Convert from a typechecker expression to an abstract syntax tree.
check2ast :: CheckExpr -> AST
check2ast = hoist go . rename (HM.keysSet builtins)
  where
    go :: CheckExprF r -> ASTF r
    go = \case
      VarF name -> VarF name
      AppFC ef ex -> AppF ef (ex :| [])
      AbsF n e -> AbsF (n :| []) e
      LetF (Def nx ex) e -> LetFP ((nx, ex) :| []) e
      CtrFC ctr -> CtrF ctr []
      CaseF ps -> CaseF ps
      CallCCFC-> VarF "callcc"
      FixFC -> VarF "fix"
      HoleFC -> HoleFP

-- | Convert from an evaluator expression to an abstract syntax tree.
eval2ast :: EvalExpr -> AST
-- Because all `ast2eval` replaces all free instances of `callcc`,
-- all instances of `callcc` must be bound;
-- therefore, we are free to alpha convert them,
-- freeing the name `callcc` for us to use for the built-in again.
eval2ast = hoist go . rename (HM.keysSet builtins)
  where
    go :: EvalExprF r -> ASTF r
    go = \case
      VarF name -> VarF name
      AppFE ef ex -> AppF ef (ex :| [])
      AbsF n e -> AbsF (n :| []) e
      CtrFE ctr -> CtrF ctr []
      CaseF ps -> CaseF ps
      CallCCFE -> VarF "callcc"
      ContFE e -> AbsF ("!" :| []) e
