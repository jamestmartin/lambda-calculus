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

dfi :: Expression
dfi = Application d (Application f i)
  where d = Abstraction "x" $ Application (Variable "x") (Variable "x")
        f = Abstraction "f" $ Application (Variable "f") (Application (Variable "f") (Variable "y"))
        i = Abstraction "x" $ Variable "x"

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
      [ testCase "identity" $ parseExpression "^x.x" @?= Right (Abstraction "x" $ Variable "x")
      , testCase "application shorthand" $ parseExpression "a b c d" @?= Right (Application (Application (Application (Variable "a") (Variable "b")) (Variable "c")) (Variable "d"))
      , testCase "ttttt" $ parseExpression "(^T f x.(T (T (T (T T)))) f x) (^f x.f (f x)) (^x.x) y"
          @?= Right ttttt
      ]
    , testProperty "parseExpression is the left inverse of show" prop_parseExpression_inverse
    ]
  ]
