{-# LANGUAGE DataKinds #-}
module UntypedLambdaCalculus.Parser (parseExpr) where

import Control.Monad.Reader (ReaderT, runReaderT, withReaderT, mapReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Data.List (foldl1')
import Data.Nat (Nat (Z))
import Data.Vec (Vec (Empty, (:.)), elemIndex)
import Text.Parsec (Parsec, SourceName, ParseError, many, sepBy1, letter, alphaNum, char, between, (<|>), spaces, parse)
import UntypedLambdaCalculus (Expr (Free, Var, Lam, App))

type Parser s a = ReaderT s (Parsec String ()) a

-- | A variable name.
name :: Parsec String () String
name = do
  c <- letter
  cs <- many alphaNum
  return $ c : cs

-- | A variable expression.
var :: Parser (Vec n String) (Expr n)
var = do
  varn <- lift name
  bound <- ask
  return $ maybe (Free varn) Var $ elemIndex varn bound

-- | Run parser between parentheses.
parens :: Parsec String () a -> Parsec String () a
parens = between (char '(') (char ')')

-- | A lambda expression.
lam :: Parser (Vec n String) (Expr n)
lam = do
  (lift $ between (char '\\') (char '.' >> spaces) $ name `sepBy1` spaces) >>= help
  where help :: [String] -> Parser (Vec n String) (Expr n)
        help []     = app
        help (v:vs) = Lam v <$> withReaderT (v :.) (help vs)

-- | An application expression.
app :: Parser (Vec n String) (Expr n)
app = foldl1' App <$> mapReaderT (`sepBy1` spaces) safeExpr

ll :: (Parsec String () a -> Parsec String () b -> Parsec String () c) -> Parser s a -> Parser s b -> Parser s c
ll f p1 p2 = do
  bound <- ask
  lift $ f (runReaderT p1 bound) (runReaderT p2 bound)

-- | An expression, but where applications must be surrounded by parentheses,
-- | to avoid ambiguity (infinite recursion on `app` in the case where the first
-- | expression in the application is also an `app`, consuming no input).
safeExpr :: Parser (Vec n String) (Expr n)
safeExpr = ll (<|>) var $ ll (<|>) lam $ mapReaderT parens (ll (<|>) lam app)

-- | Since applications do not require parentheses and can contain only a single item,
-- | the `app` parser is sufficient to parse any expression at all.
parseExpr :: SourceName -> String -> Either ParseError (Expr 'Z)
parseExpr sourceName code = parse (runReaderT app Empty) sourceName code
