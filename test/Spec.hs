import Data.Char (isAlpha)
import qualified Data.Text as T
import Generic.Random (genericArbitraryRec, uniform)
import LambdaCalculus.Expression
import LambdaCalculus.Parser
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import TextShow (showt)

instance Arbitrary Expression where
  arbitrary = genericArbitraryRec uniform

instance Arbitrary T.Text where
  arbitrary = T.pack <$> listOf1 (elements $ ['A'..'Z'] ++ ['a'..'z'])

-- These are terms which have complex reduction steps and
-- are likely to catch bugs in the substitution function, if there are any.
-- However, they don't have any particular computational *meaning*,
-- so the names for them are somewhat arbitrary.

-- This should evaluate to `y y`.
dfi :: Expression
dfi = Application d (Application f i)
  where d = Abstraction "x" $ Application (Variable "x") (Variable "x")
        f = Abstraction "f" $ Application (Variable "f") (Application (Variable "f") (Variable "y"))
        i = Abstraction "x" $ Variable "x"

-- This should evalaute to `y`.
ttttt :: Expression
ttttt = Application (Application (Application f t) (Abstraction "x" (Variable "x"))) (Variable "y")
  where t = Abstraction "f" $ Abstraction "x" $
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

prop_parseExpression_inverse :: Expression -> Bool
prop_parseExpression_inverse expr = Right expr == parseExpression (showt expr)

main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ testGroup "Evaluator tests"
    [ testCase "DFI" $ eagerEval dfi @?= Application (Variable "y") (Variable "y")
    , testCase "ttttt" $ eagerEval ttttt @?= Variable "y"
    ]
  , testGroup "Parser tests"
    [ testGroup "Unit tests"
      [ testCase "identity" $ parseExpression "\\x.x" @?= Right (Abstraction "x" $ Variable "x")
      -- This syntax is forbidden because it interacts poorly with other syntax, e.g. `let x=in` becoming a valid program.
      --, testCase "nullary application" $ parseExpression "()" @?= Right (Abstraction "x" $ Variable "x")
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
    , testProperty "parseExpression is the left inverse of show" prop_parseExpression_inverse
    ]
  ]
