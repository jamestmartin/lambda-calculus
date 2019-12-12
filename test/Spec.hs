import Data.Char (isAlpha)
import Generic.Random (genericArbitraryRec, uniform)
import LambdaCalculus.Expression
import LambdaCalculus.Parser
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

instance Arbitrary Expression where
  arbitrary = genericArbitraryRec uniform

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
prop_parseExpression_inverse expr' = Right expr == parseExpression (show expr)
  where expr = legalizeVariables expr'
        legalizeVariables (Variable var) = Variable $ legalVar var
        legalizeVariables (Application ef ex) = Application (legalizeVariables ef) (legalizeVariables ex)
        legalizeVariables (Abstraction var body) = Abstraction (legalVar var) $ legalizeVariables body
        legalVar var = 'v' : filter isAlpha var


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
