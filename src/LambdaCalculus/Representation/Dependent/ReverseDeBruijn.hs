module LambdaCalculus.Representation.Dependent.ReverseDeBruijn where

import Control.Monad.Reader (Reader, runReader, withReader, asks)
import Data.Type.Equality ((:~:)(Refl))
import Data.Type.Nat
import Data.Vec
import LambdaCalculus.Representation
import qualified LambdaCalculus.Representation.Standard as Std

-- | Expressions are parametrized by the depth of the variable bindings they may access.
-- An expression in which no variables are bound (a closed expression) is represented by `Expression 'Z`.
data Expression :: Nat -> * where
  -- | The body of a lambda abstraction may reference all of the variables
  -- bound in its parent, in addition to a new variable bound by the abstraction.
  Abstraction :: Expression ('S n) -> Expression n
  -- | On the other hand, any sub-expression may choose to simply ignore
  -- the variable bound by the lambda expression,
  -- only referencing the variables bound in its parent instead.
  --
  -- For example, in the constant function `\x. \y. x`,
  -- although the innermost expression *may* access the innermost binding (`y`),
  -- it instead only accesses the outer one, `x`.
  -- Thus the body of the expression would be `Drop Variable`.
  --
  -- Given the lack of any convention for how to write 'Drop',
  -- I have chosen to write it as `?x` where `x` is the body of the drop.
  Drop :: Expression n -> Expression ('S n)
  -- | For this reason (see 'Drop'),
  -- variables only need to access the innermost accessible binding.
  -- To access outer bindings, you must first 'Drop' all of the bindings
  -- in between the variable and the desired binding to access.
  Variable :: Expression ('S n)
  -- | Function application. The left side is the function, and the right side is the argument.
  Application :: Expression n -> Expression n -> Expression n
  -- | A free expression is a symbolic placeholder which reduces to itself.
  Free :: String -> Expression 'Z
  Substitution :: SNat n -> Expression m -> Expression ('S (Plus n m)) -> Expression (Plus n m)

instance SNatI n => Show (Expression n) where
  show expr = show' snat expr
    where show' :: SNat n -> Expression n -> String
          show' (SS n) Variable    = show n
          show' SZ     (Free name) = name
          show' (SS n) (Drop body) = '?' : show' n body
          show' n      (Abstraction body)  = "(\\" ++ show n ++ " " ++ show' (SS n) body ++ ")"
          show' n      (Application fe xe) = "(" ++ show' n fe ++ " " ++ show' n xe ++ ")"

instance IsExpr (Expression 'Z) where 
  fromStandard expr = runReader (fromStandard' expr) VNil
    where fromStandard' :: SNatI n => Std.Expression -> Reader (Vec n String) (Expression n)
          -- TODO: This code is absolutely atrocious.
          -- It is in dire need of cleanup.
          fromStandard' (Std.Variable name) = asks $ makeVar snat SZ
            where makeVar :: SNat n -> SNat m -> Vec n String -> Expression (Plus m n)
                  makeVar SZ     m VNil            = dropEm m $ Free name
                  makeVar (SS n) m (var ::: bound) = case plusSuc m n of
                    Refl
                      | name == var -> dropEm2 n m
                      | otherwise   -> makeVar n (SS m) bound

                  dropEm :: SNat m -> Expression n -> Expression (Plus m n)
                  dropEm SZ     e = e
                  dropEm (SS n) e = Drop $ dropEm n e

                  dropEm2 :: SNat n -> SNat m -> Expression ('S (Plus m n))
                  dropEm2 _ SZ = Variable
                  dropEm2 n (SS m) = Drop $ dropEm2 n m
          fromStandard' (Std.Abstraction name body)
            = fmap Abstraction $ withReader (name :::) $ fromStandard' body
          fromStandard' (Std.Application fe xe)
            = Application <$> fromStandard' fe <*> fromStandard' xe

  -- TODO: Implement this. Important!
  toStandard expr = undefined
