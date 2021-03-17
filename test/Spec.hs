import LambdaCalculus

import Test.Tasty
import Test.Tasty.HUnit

-- These are terms which have complex reduction steps and
-- are likely to catch bugs in the substitution function, if there are any.
-- However, they don't have any particular computational *meaning*,
-- so the names for them are somewhat arbitrary.

-- This should evaluate to `y y`.
dfi :: EvalExpr
dfi = App d (App f i)
  where
    d = Abs "x" $ App (Var "x") (Var "x")
    f = Abs "f" $ App (Var "f") (App (Var "f") (Var "y"))
    i = Abs "x" $ Var "x"

-- This should evalaute to `y`.
ttttt :: EvalExpr
ttttt = App (App (App f t) (Abs "x" (Var "x"))) (Var "y")
  where
    t = Abs "f" $ Abs "x" $
          App (Var "f") (App (Var "f") (Var "x"))
    f = Abs "T" $ Abs "f" $ Abs "x" $
          App
          (App
            (App (Var "T")
              (App (Var "T")
                (App (Var "T")
                  (App (Var "T")
                    (Var "T")))))
            (Var "f"))
          (Var "x")

-- | A simple divergent expression.
omega :: EvalExpr
omega = App x x
  where x = Abs "x" (App (Var "x") (Var "x"))

cc1 :: EvalExpr
cc1 = App (Var "callcc") (Abs "k" (App omega (App (Var "k") (Var "z"))))

cc2 :: EvalExpr
cc2 = App (Var "y") (App (Var "callcc") (Abs "k" (App (Var "z") (App (Var "k") (Var "x")))))

main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ testGroup "Evaluator tests"
    [ testCase "capture test 1: DFI" $ eval dfi @?= App (Var "y") (Var "y")
    , testCase "capture test 2: ttttt" $ eval ttttt @?= Var "y"
    , testCase "invoking a continuation replaces the current continuation" $ eval cc1 @?= Var "z"
    , testCase "callcc actually captures the current continuation" $ eval cc2 @?= App (Var "y") (Var "x")
    ]
  , testGroup "Parser tests"
    [ testGroup "Unit tests"
      [ testCase "identity" $ parseEval "\\x.x" @?= Right (Abs "x" $ Var "x")
      , testCase "unary application" $ parseEval "(x)" @?= Right (Var "x")
      , testCase "application shorthand" $ parseEval "a b c d" @?= Right (App (App (App (Var "a") (Var "b")) (Var "c")) (Var "d"))
      , testCase "let" $ parseEval "let x = \\y.y in x" @?= Right (App (Abs "x" (Var "x")) (Abs "y" (Var "y")))
      , testCase "multi-let" $ parseEval "let x = y; y = z in x y" @?= Right (App (Abs "x" (App (Abs "y" (App (Var "x") (Var "y"))) (Var "z"))) (Var "y"))
      , testCase "ttttt" $ parseEval "(\\T f x.(T (T (T (T T)))) f x) (\\f x.f (f x)) (\\x.x) y"
          @?= Right ttttt
      , testGroup "Redundant whitespace"
        [ testCase "around variable" $ parseEval " x " @?= Right (Var "x")
        , testCase "around lambda" $ parseEval " \\ x   y . x " @?= Right (Abs "x" $ Abs "y" $ Var "x")
        , testCase "around application" $ parseEval " ( x   (y ) ) " @?= Right (App (Var "x") (Var "y"))
        , testCase "around let" $ parseEval "  let x=(y)in x  " @?= Right (App (Abs "x" (Var "x")) (Var "y"))
        ]
      ]
    ]
  ]
