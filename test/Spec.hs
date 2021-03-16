import LambdaCalculus
import LambdaCalculus.Parser
import Test.Tasty
import Test.Tasty.HUnit

-- These are terms which have complex reduction steps and
-- are likely to catch bugs in the substitution function, if there are any.
-- However, they don't have any particular computational *meaning*,
-- so the names for them are somewhat arbitrary.

-- This should evaluate to `y y`.
dfi :: Expression
dfi = Application d (Application f i)
  where
    d = Abstraction "x" $ Application (Variable "x") (Variable "x")
    f = Abstraction "f" $ Application (Variable "f") (Application (Variable "f") (Variable "y"))
    i = Abstraction "x" $ Variable "x"

-- This should evalaute to `y`.
ttttt :: Expression
ttttt = Application (Application (Application f t) (Abstraction "x" (Variable "x"))) (Variable "y")
  where
    t = Abstraction "f" $ Abstraction "x" $
          Application (Variable "f") (Application (Variable "f") (Variable "x"))
    f = Abstraction "T" $ Abstraction "f" $ Abstraction "x" $
          Application (Application
                        (Application (Variable "T")
                         (Application (Variable "T")
                          (Application (Variable "T")
                           (Application (Variable "T")
                            (Variable "T")))))
                        (Variable "f"))
          (Variable "x")

-- | A simple divergent expression.
omega :: Expression
omega = Application x x
  where x = Abstraction "x" (Application (Variable "x") (Variable "x"))

cc1 :: Expression
cc1 = Application (Variable "callcc") (Abstraction "k" (Application omega (Application (Variable "k") (Variable "z"))))

cc2 :: Expression
cc2 = Application (Variable "y") (Application (Variable "callcc") (Abstraction "k" (Application (Variable "z") (Application (Variable "k") (Variable "x")))))

main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ testGroup "Evaluator tests"
    [ testCase "capture test 1: DFI" $ eval dfi @?= Application (Variable "y") (Variable "y")
    , testCase "capture test 2: ttttt" $ eval ttttt @?= Variable "y"
    , testCase "invoking a continuation replaces the current continuation" $ eval cc1 @?= Variable "z"
    , testCase "callcc actually captures the current continuation" $ eval cc2 @?= Application (Variable "y") (Variable "x")
    ]
  , testGroup "Parser tests"
    [ testGroup "Unit tests"
      [ testCase "identity" $ parseExpression "\\x.x" @?= Right (Abstraction "x" $ Variable "x")
      , testCase "unary application" $ parseExpression "(x)" @?= Right (Variable "x")
      , testCase "application shorthand" $ parseExpression "a b c d" @?= Right (Application (Application (Application (Variable "a") (Variable "b")) (Variable "c")) (Variable "d"))
      , testCase "let" $ parseExpression "let x = \\y.y in x" @?= Right (Application (Abstraction "x" (Variable "x")) (Abstraction "y" (Variable "y")))
      , testCase "multi-let" $ parseExpression "let x = y; y = z in x y" @?= Right (Application (Abstraction "x" (Application (Abstraction "y" (Application (Variable "x") (Variable "y"))) (Variable "z"))) (Variable "y"))
      , testCase "ttttt" $ parseExpression "(\\T f x.(T (T (T (T T)))) f x) (\\f x.f (f x)) (\\x.x) y"
          @?= Right ttttt
      , testGroup "Redundant whitespace"
        [ testCase "around variable" $ parseExpression " x " @?= Right (Variable "x")
        , testCase "around lambda" $ parseExpression " \\ x   y . x " @?= Right (Abstraction "x" $ Abstraction "y" $ Variable "x")
        , testCase "around application" $ parseExpression " ( x   (y ) ) " @?= Right (Application (Variable "x") (Variable "y"))
        , testCase "around let" $ parseExpression "  let x=(y)in x  " @?= Right (Application (Abstraction "x" (Variable "x")) (Variable "y"))
        ]
      ]
    ]
  ]
