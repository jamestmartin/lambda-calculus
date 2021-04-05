module Ivo.Syntax.Base
  ( Scope (..), Def (..)
  , Expr (..), Type
  , CaseBranches (..), Pattern (..)
  , Literal (..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

data Scope name = Scope { getScope :: ![Def name] }
  deriving (Eq, Show)

data Def name
  = BasicDef !name !(Expr name)
  | BasicDecl !(NonEmpty name) !(Type name)
  deriving (Eq, Show)

data Expr name
  = Var !name
  | Lit !(Literal (Expr name))
  | App !(Expr name) !(NonEmpty (Expr name))
  | Let !(NonEmpty (Def name)) !(Expr name)
  | Lam !(NonEmpty name) !(Expr name)
  | Case !(Expr name) !(CaseBranches name)
  | Forall !(NonEmpty name) !(Type name)
  | Arrow !(Type name) !(Type name)
  | Ann !(Expr name) !(Type name)
  | Hole
  deriving (Eq, Show)

type Type name = Expr name

data CaseBranches name = CaseBranches ![(Pattern name, Expr name)]
  deriving (Eq, Show)

data Pattern name
  = PatVar !name
  | PatLit !(Literal (Pattern name))
  | Irrelevant
  | PatApp !name [Pattern name]
  deriving (Eq, Show)

data Literal r
  = LitInt Int
  | LitChar Char
  | LitStr Text
  | LitList [r]
  deriving (Eq, Show)
