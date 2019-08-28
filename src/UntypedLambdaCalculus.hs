module UntypedLambdaCalculus where

import Control.Applicative ((<|>))
import Data.Type.Nat (Nat (Z, S), SNat (SZ, SS), SNatI, Plus, snat)

-- | Expressions are parametrized by the depth of the variable bindings they may access.
-- An expression in which no variables are bound (a closed expression) is represented by `Expr 'Z`.
data Expr :: Nat -> * where
  -- | The body of a lambda abstraction may reference all of the variables
  -- bound in its parent, in addition to a new variable bound by the abstraction.
  Lam :: Expr ('S n) -> Expr n
  -- | On the other hand, any sub-expression may choose to simply ignore
  -- the variable bound by the lambda expression,
  -- only referencing the variables bound in its parent instead.
  --
  -- For example, in the constant function `\x. \y. x`,
  -- although the innermost expression *may* access the innermost binding (`y`),
  -- it instead only accesses the outer one, `x`.
  -- Thus the body of the expression would be `Drop Var`.
  --
  -- Given the lack of any convention for how to write 'Drop',
  -- I have chosen to write it as `?x` where `x` is the body of the drop.
  Drop :: Expr n -> Expr ('S n)
  -- | For this reason (see 'Drop'),
  -- variables only need to access the innermost accessible binding.
  -- To access outer bindings, you must first 'Drop' all of the bindings
  -- in between the variable and the desired binding to access.
  Var :: Expr ('S n)
  -- | Function application. The left side is the function, and the right side is the argument.
  App :: Expr n -> Expr n -> Expr n
  -- | A free expression is a symbolic placeholder which reduces to itself.
  Free :: String -> Expr 'Z
  Subst :: SNat n -> Expr m -> Expr ('S (Plus n m)) -> Expr (Plus n m)

instance SNatI n => Show (Expr n) where
  show expr = show' snat expr
    where show' :: SNat n -> Expr n -> String
          show' (SS n) Var         = show n
          show' SZ     (Free name) = name
          show' (SS n) (Drop body) = '?' : show' n body
          show' n      (Lam body)  = "(\\" ++ show n ++ " " ++ show' (SS n) body ++ ")"
          show' n      (App fe xe) = "(" ++ show' n fe ++ " " ++ show' n xe ++ ")"

-- | The meaning of expressions is defined by how they can be reduced.
-- There are three kinds of reduction: beta-reduction ('betaReduce'),
-- which defines how applications interact with lambda abstractions;
-- eta-reduction ('etaReduce'), which describes how lambda abstractions interact with applications;
-- and a new form, which I call scope-reduction ('scopeReduce'),
-- which describes how 'Drop' scope delimiters propogate through expressions.
--
-- This function takes an expression and either reduces it,
-- or, if there is no applicable reduction rule, returns nothing.
-- The lack of an applicable reduction rule does not necessarily mean
-- that an expression is irreducible: see 'evaluate' for more information.
reduce :: Expr n -> Maybe (Expr n)
-- Note: there are no expressions which are reducible in multiple ways.
-- Only one of these cases can apply at once.
reduce expr = scopeReduce expr <|> substitute expr <|> betaReduce expr <|> etaReduce expr

-- | A subexpression to which a reduction step may be applied is called a "redex",
-- short for "reducible expression".
isRedex :: Expr n -> Bool
isRedex expr = isScopeRedex expr || isBetaRedex expr || isEtaRedex expr

-- | Beta reduction describes how functions behave when applied to arguments.
-- To reduce a function application, you simply 'substitute` the parameter into the function body.
--
-- Beta reduction is the fundamental computation step of the lambda calculus.
-- Only this rule is necessary for the lambda calculus to be turing-complete;
-- the other reductions merely define *equivalences* between expressions,
-- so that every expression has a canonical form.
betaReduce :: Expr n -> Maybe (Expr n)
betaReduce (App (Lam fe) xe) = Just $ Subst SZ xe fe
-- (^) Aside: 'App' represents function application in the lambda calculus.
-- Haskell convention would be to name the function `f` and the argument `x`,
-- but that often collides with Haskell functions and arguments,
-- so instead I will be calling them `fe` and `xe`,
-- where the `e` stands for "expression".
betaReduce  _                = Nothing

-- TODO: Document this.
substitute :: Expr n -> Maybe (Expr n)
substitute (Subst SZ     x Var)         = Just x
substitute (Subst (SS _) _ Var)         = Just Var
substitute (Subst SZ     x (Drop body)) = Just body
substitute (Subst (SS n) x (Drop body)) = Just $ Drop $ Subst n x body
substitute (Subst n      x (App fe xe)) = Just $ App (Subst n x fe) (Subst n x xe)
substitute (Subst n      x (Lam body))  = Just $ Lam $ Subst (SS n) x body
substitute _                            = Nothing

-- | A predicate determining whether a subexpression would allow a beta-reduction step.
isBetaRedex :: Expr n -> Bool
isBetaRedex (App (Lam _) _) = True
isBetaRedex _               = False

-- | For any expression `f`, `f` is equivalent to `\x. ?f x`, a property called "eta-equivalence".
-- The conversion between these two forms is called "eta-conversion",
-- with the conversion from `f` to `\x. ?f x` being called "eta-expansion",
-- and its inverse (from `\x. ?f x` to `f`) being called "eta-reduction".
--
-- This rule is not necessary for the lambda calculus to express computation;
-- that's the purpose of 'betaReduce'; rather, it expresses the idea of "extensionality",
-- which in general describes the principles that judge objects to be equal
-- if they have the same external properties
-- (from within the context of the logical system, i.e. without regard to representation).
-- In the context of functions, this would mean that two functions are equal
-- if and only if they give the same result for all arguments.
etaReduce :: Expr n -> Maybe (Expr n)
etaReduce (Lam (App (Drop fe) Var)) = Just fe
etaReduce _                         = Nothing

-- | A predicate determining whether a subexpression would allow an eta-reduction step.
isEtaRedex :: Expr n -> Bool
isEtaRedex (Lam (App (Drop _) Var )) = True
isEtaRedex _                         = False

-- | Eta-expansion, the inverse of 'etaReduce'.
etaExpand :: Expr n -> Expr n
etaExpand fe = Lam $ App (Drop fe) Var

-- TODO: Scope conversion isn't a real conversion relationship!
-- 'scopeExpand' can only be applied a finite number of times.
-- That doesn't break the code, but I don't want to misrepresent it.
-- 'scopeExpand' is only the *left* inverse of 'scopeReduce',
-- not the inverse overall.
--
-- Alternatively, 'scopeExpand' could be defined on expressions with no sub-expressions.
-- That would make sense, but then 'scopeReduce' would also have to be defined like that,
-- which would make every reduction valid as well, meaning we can't use it in the same way
-- we use the other reduction, because it always succeeds, and thus every expression
-- could be considered reducible.
--
-- Perhaps delimiting scope should be external to the notion of an expression,
-- like how Thyer represented it in the "Lazy Specialization" paper
-- (http://thyer.name/phd-thesis/thesis-thyer.pdf).
--
-- In addition, it really doesn't fit in with the current scheme of reductions.
-- It doesn't apply to any particular constructor/eliminator relationship,
-- instead being this bizarre syntactical fragment instead of a real expression.
-- After all, I could have also chosen to represent the calculus
-- so that variables are parameterized by `Fin n` instead of being wrapped in stacks of 'Drop'.
--
-- I think this problem will work itself out as I work further on evaluation strategies
-- and alternative representations, but for now, it'll do.
-- | Scope-conversion describes how 'Drop'-delimited scopes propgate through expressions.
-- It expresses the idea that a variable is used in an expression
-- if and only if it is used in at least one of its sub-expressions.
--
-- Similarly to 'etaReduce', there is also define an inverse function, 'scopeExpand'.
scopeReduce :: Expr n -> Maybe (Expr n)
scopeReduce (App (Drop fe) (Drop xe)) = Just $ Drop $ App fe xe
-- TODO: I feel like there's a more elegant way to represent this.
-- It feels like `Lam (Drop body)` should be its own atomic unit.
-- Maybe I could consider a combinator-based representation,
-- where `Lam (Drop body)` is just the `K` combinator `K body`?
scopeReduce (Lam (Drop (Drop body)))  = Just $ Drop $ Lam $ Drop body
scopeReduce _                         = Nothing

-- | A predicate determining whether a subexpression would allow a scope-reduction step.
isScopeRedex :: Expr n -> Bool
isScopeRedex (App (Drop _) (Drop _)) = True
isScopeRedex (Lam (Drop (Drop _)))   = True
isScopeRedex _                       = False

-- | Scope-expansion, the left inverse of 'scopeReduce'.
scopeExpand :: Expr n -> Maybe (Expr n)
scopeExpand (Drop (App fe xe))       = Just $ App (Drop fe) (Drop xe)
scopeExpand (Drop (Lam (Drop body))) = Just $ Lam $ Drop $ Drop body
scopeExpand _                        = Nothing

-- | An expression is in "normal form" if it contains no redexes (see 'isRedex').
isNormal :: Expr n -> Bool
isNormal expr = not (isRedex expr) && case expr of
  -- In addition to this expression not being a redex,
  -- we must check that none of its subexpressions are redexes either.
  App fe xe -> isNormal fe && isNormal xe
  Lam e     -> isNormal e
  Drop e    -> isNormal e
  _         -> True

-- TODO: Finish the below documentation on reduction strategies. I got bored.
-- | Now that we have defined the ways in which an expression may be reduced ('reduce'),
-- we need to define a "reduction strategy" to describe in what order we will apply reductions.
-- Different reduction strategies have different performance characteristics,
-- and even produce different results.
--
-- One of the most important distinctions between strategies is whether they are "normalizing".
-- A normalising strategy will terminate if and only if
-- the expression it is normalizing has a normal form.
--
-- I have chosen to use a normalizing reduction strategy.
eval :: Expr n -> Expr n
eval expr = case reduce innerReduced of
  Just e -> eval e
  -- The expression didn't make any progress,
  -- so we know that no further reductions can be applied here.
  -- It must be blocked on something.
  -- TODO: return why we stopped evaluating,
  -- so we can avoid redundantly re-evaluating stuff if nothing changed.
  Nothing -> innerReduced
  where innerReduced = case expr of
          -- TODO: Factor out this recursive case (from 'isNormal' too).
          App fe xe -> App (eval fe) (eval xe)
          Lam e -> Lam (eval e)
          Drop e -> Drop (eval e)
          x -> x
