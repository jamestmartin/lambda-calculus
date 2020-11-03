module LambdaCalculus.Parser.AbstractSyntax
    ( AbstractSyntax (..), Definition, Identifier
    ) where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as T
import TextShow

-- | The abstract syntax tree reflects the structure of the externally-visible syntax.
--
-- This contains a lot of syntactic sugar when compared with 'LambdaCalculus.Expression'.
-- If this syntactic sugar were used in Expression, then operations like evaluation
-- would become unnecessarily complicated, because the same expression
-- can be represented in terms of multiple abstract syntax trees.
data AbstractSyntax
    = Variable Identifier
    | Application [AbstractSyntax]
    | Abstraction [Identifier] AbstractSyntax
    | Let [Definition] AbstractSyntax
type Definition = (Identifier, AbstractSyntax)
type Identifier = Text

-- I'm surprised this isn't in base somewhere.
unsnoc :: [a] -> ([a], a)
unsnoc [x] = ([], x)
unsnoc (x : xs) = first (x :) (unsnoc xs)

instance TextShow AbstractSyntax where
    showb = unambiguous
      where
        unambiguous, ambiguous :: AbstractSyntax -> Builder
        unambiguous (Variable name) = fromText name
        -- There's no technical reason for the AST to forbid nullary applications and so forth.
        -- However the parser rejects them to avoid confusing edge cases like `let x=in`,
        -- so they're forbidden here too so that `show` will never print anything the parser would refuse to accept.
        unambiguous (Application []) = error "Empty applications are currently disallowed."
        unambiguous (Application (unsnoc -> (es, final))) = foldr (\e es' -> ambiguous e <> " " <> es') final' es
            where final' = case final of
                    Application _ -> ambiguous final
                    _             -> unambiguous final
        unambiguous (Abstraction [] _) = error "Empty lambdas are currently disallowed."
        unambiguous (Abstraction names body) = "λ" <> fromText (T.intercalate " " names) <> ". " <> unambiguous body
        unambiguous (Let [] body) = error "Empty lets are currently disallowed."
        unambiguous (Let defs body) = "let " <> fromText (T.intercalate "; " $ map (toText . showDef) defs) <> " in " <> unambiguous body
          where showDef :: Definition -> Builder
                showDef (name, val) = fromText name <> " = " <> unambiguous val

        -- | Adds a grouper if omitting it could result in ambiguous syntax.
        -- (Which is to say, the parser would parse it wrong because a different parse has a higher priority.)
        ambiguous e@(Variable _) = unambiguous e
        ambiguous e = "(" <> unambiguous e <> ")"

instance Show AbstractSyntax where
    show = T.unpack . showt
