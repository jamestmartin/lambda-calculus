module LambdaCalculus.Parser.AbstractSyntax
  ( AbstractSyntax (..), Definition, Identifier
  ) where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty ((:|)), fromList, toList)
import Data.Text (Text)
import Data.Text qualified as T
import TextShow (Builder, TextShow, showb, showt, toText, fromText)

-- | The abstract syntax tree reflects the structure of the externally-visible syntax.
--
-- This contains a lot of syntactic sugar when compared with 'LambdaCalculus.Expression'.
-- If this syntactic sugar were used in Expression, then operations like evaluation
-- would become unnecessarily complicated, because the same expression
-- can be represented in terms of multiple abstract syntax trees.
data AbstractSyntax
  = Variable Identifier
  -- There is no technical reason for the AST to forbid nullary applications and so forth.
  -- However the parser rejects them to avoid confusing edge cases like `let x=in`,
  -- so they're forbidden here too so that the syntax tree can't contain data
  -- that the parser would refuse to accept.
  --
  -- As a matter of curiosity, here's why `let x=in` was syntactically valid:
  -- 1. Parentheses in `let` statements are optional, infer them: `let x=()in()`.
  -- 2. Insert optional whitespace: `let x = () in ()`.
  -- 3. Nullary application expands to the identity function because
  --    the identity function is the left identity of function application:
  --    `let x=(\x.x) in \x.x`.
  --
  -- | n-ary function application: `(f x_1 x_2 ... x_n)`.
  | Application (NonEmpty AbstractSyntax)
  -- | Lambda abstraction over n variables: `(λx_1 x_2 ... x_n. e)`
  | Abstraction (NonEmpty Identifier) AbstractSyntax
  -- | Let expressions (syntactic sugar) binding `n` variables:
  --   `let x_1 = e_1; x_2 = e_2; ... x_n = e_n`.
  | Let (NonEmpty Definition) AbstractSyntax
type Definition = (Identifier, AbstractSyntax)
type Identifier = Text

-- I'm surprised this isn't in base somewhere.
unsnoc :: NonEmpty a -> ([a], a)
unsnoc (x :| []) = ([], x)
unsnoc (x :| xs) = first (x :) (unsnoc (fromList xs))

instance TextShow AbstractSyntax where
  showb = unambiguous
    where
      -- Parentheses are often optional to the parser, but not in every context.
      -- The `unambigous` printer is used in contexts where parentheses are optional, and does not include them;
      -- the `ambiguous` printer is used when omitting parentheses could result in an incorrect parse.
      unambiguous, ambiguous :: AbstractSyntax -> Builder
      unambiguous (Variable name) = fromText name
      unambiguous (Application (unsnoc -> (es, final))) = foldr (\e es' -> ambiguous e <> " " <> es') final' es
        where
          final' = case final of
            Application _ -> ambiguous final
            _             -> unambiguous final
      unambiguous (Abstraction names body) = "λ" <> names' <> ". " <> unambiguous body
        where names' = fromText (T.intercalate " " $ toList names)
      unambiguous (Let defs body) = "let " <> defs' <> " in " <> unambiguous body
        where
          defs' = fromText (T.intercalate "; " $ map (toText . showDef) $ toList defs)

          showDef :: Definition -> Builder
          showDef (name, val) = fromText name <> " = " <> unambiguous val

      -- | Adds a grouper if omitting it could result in ambiguous syntax.)
      ambiguous e@(Variable _) = unambiguous e
      ambiguous e = "(" <> unambiguous e <> ")"

instance Show AbstractSyntax where
  show = T.unpack . showt
