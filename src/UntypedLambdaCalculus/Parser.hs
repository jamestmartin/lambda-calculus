module UntypedLambdaCalculus.Parser (parseExpr) where

import Control.Applicative (liftA2)
import Control.Monad.Reader (Reader, runReader, withReader, asks)
import Data.Type.Equality ((:~:)(Refl))
import Data.Type.Nat
import Data.Vec
import Text.Parsec (SourceName, ParseError, (<|>), many, sepBy, letter, alphaNum, char, between, spaces, parse, string)
import Text.Parsec.String (Parser)
import UntypedLambdaCalculus (Expr (Free, Var, Lam, App, Drop))

data Ast = AstVar String
         | AstLam [String] Ast
         | AstApp [Ast]
         | AstLet String Ast Ast

-- | A variable name.
name :: Parser String
name = liftA2 (:) letter $ many alphaNum

-- | A variable expression.
var :: Parser Ast
var = AstVar <$> name

-- | Run parser between parentheses.
parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

-- | A lambda expression.
lam :: Parser Ast
lam = do
  vars <- between (char '\\') (char '.') $ name `sepBy` spaces
  spaces
  body <- app
  return $ AstLam vars body

-- | An application expression.
app :: Parser Ast
app = AstApp <$> consumesInput `sepBy` spaces

let_ :: Parser Ast
let_ = do
  string "let "
  bound <- name
  string " = "
  -- we can't allow raw `app` or `lam` here
  -- because they will consume the `in` as a variable.
  val <- let_ <|> var <|> parens app
  char ' '
  spaces
  string "in "
  body <- app
  return $ AstLet bound val body

-- | An expression, but where applications must be surrounded by parentheses,
-- | to avoid ambiguity (infinite recursion on `app` in the case where the first
-- | expression in the application is also an `app`, consuming no input).
consumesInput :: Parser Ast
consumesInput = let_ <|> var <|> lam <|> parens app

toExpr :: Ast -> Expr 'Z
toExpr ast = runReader (toExpr' ast) VNil
  -- TODO: This code is absolutely atrocious.
  -- It is in dire need of cleanup.
  where toExpr' :: SNatI n => Ast -> Reader (Vec n String) (Expr n)
        toExpr' (AstVar name) = asks $ makeVar snat SZ
          where makeVar :: SNat n -> SNat m -> Vec n String -> Expr (Plus m n)
                makeVar SZ     m VNil            = dropEm m $ Free name
                makeVar (SS n) m (var ::: bound) = case plusSuc m n of
                  Refl
                    | name == var -> dropEm2 n m
                    | otherwise   -> makeVar n (SS m) bound
        toExpr' (AstApp es) = asks $ thingy id es
        toExpr' (AstLam [] body) = toExpr' body
        toExpr' (AstLam (name:names) body) =
          fmap Lam $ withReader (name :::) $ toExpr' $ AstLam names body
        toExpr' (AstLet var val body) =
          App <$> toExpr' (AstLam [var] body) <*> toExpr' val

        thingy :: SNatI n => (Expr n -> Expr n) -> [Ast] -> Vec n String -> Expr n
        thingy f []     _     = f $ Lam Var
        thingy f (e:es) bound = thingy (flip App (runReader (toExpr' e) bound) . f) es bound

        dropEm :: SNat m -> Expr n -> Expr (Plus m n)
        dropEm SZ     e = e
        dropEm (SS n) e = Drop $ dropEm n e

        dropEm2 :: SNat n -> SNat m -> Expr ('S (Plus m n))
        dropEm2 _ SZ = Var
        dropEm2 n (SS m) = Drop $ dropEm2 n m
          
-- | Since applications do not require parentheses and can contain only a single item,
-- | the `app` parser is sufficient to parse any expression at all.
parseExpr :: SourceName -> String -> Either ParseError (Expr 'Z)
parseExpr sourceName code = toExpr <$> parse app sourceName code
