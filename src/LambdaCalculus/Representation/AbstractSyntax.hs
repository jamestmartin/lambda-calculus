module LambdaCalculus.Representation.AbstractSyntax where

import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Functor.Foldable (cata, histo)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List (foldl1')
import LambdaCalculus.Combinators (i)
import LambdaCalculus.Representation
import qualified LambdaCalculus.Representation.Standard as Std

data Expression = Variable String
                | Abstraction [String] Expression
                | Application [Expression]
                -- | `let name = value in body`
                | Let String Expression Expression

makeBaseFunctor ''Expression

instance Show Expression where
  show = histo \case
    VariableF name -> name
    AbstractionF names (body :< _) -> "λ" ++ unwords names ++ ". " ++ body
    -- TODO: this is a weird implementation of re-grouping variables,
    -- to the degree that explicit recursion would probably be more clear.
    -- Clean this up!
    ApplicationF exprs -> unwords $ mapExceptLast regroup regroupApplication exprs
    LetF name (value :< _) (body :< _)
      -> "let " ++ name ++ " = " ++ value ++ " in " ++ body
    where regroup (expr :< AbstractionF _ _) = group expr
          regroup (expr :< LetF _ _ _)       = group expr
          regroup expr                       = regroupApplication expr

          regroupApplication (expr :< ApplicationF _) = group expr
          regroupApplication (expr :< _)              = expr

          group str = "(" ++ str ++ ")"

          -- | Map the first function to all but the last element of the list,
          -- and the last function to only the last element.
          mapExceptLast :: (a -> b) -> (a -> b) -> [a] -> [b]
          -- TODO: express this as a paramorphism
          mapExceptLast _ _     []     = []
          mapExceptLast _ fLast [x]    = [fLast x]
          mapExceptLast f fLast (x:xs) = f x : mapExceptLast f fLast xs

instance IsExpr Expression where
  toStandard = cata \case
    VariableF    name       -> Std.Variable name
    -- We could technically just use `foldl' Std.Application i exprs`,
    -- since that's the justification for allowing non-binary applications in the first place,
    -- but we want expressions using only binary applications
    -- to still generate the same expression,
    -- not just beta-equivalent expressions.
    ApplicationF []         -> i
    ApplicationF [expr]     -> expr
    ApplicationF exprs      -> foldl1' Std.Application exprs
    AbstractionF names body -> foldr Std.Abstraction body names
    LetF name value body    -> Std.Application (Std.Abstraction name body) value

  -- Again with the intent of generating the canonical form for this representation,
  -- we want to convert all left-nested applications into a list application;
  -- similarly, we convert nested abstractions into a list of names,
  -- and abstractions into `let`s when applicable.
  fromStandard = histo \case
    Std.VariableF name -> Variable name
    -- `(\x. e) N` --> `let x = N in e`.
    Std.ApplicationF (_ :< Std.AbstractionF name (body :< _)) (value :< _)
      -> Let name value body
    Std.ApplicationF (Application exprs :< _) (xe :< _)
      -> Application $ exprs ++ [xe]
    Std.ApplicationF (fe                :< _) (xe :< _)
      -> Application [fe, xe]
    Std.AbstractionF name (Abstraction names body :< _)
      -> Abstraction (name : names) body
    Std.AbstractionF name (body :< _)
      -> Abstraction [name]         body
