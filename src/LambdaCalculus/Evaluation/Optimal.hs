-- | !!!IMPORTANT!!!
-- This module is a WORK IN PROGRESS.
-- It DOES NOT YET IMPLEMENT OPTIMAL EVALUATION.
-- It currently implements *lazy* evaluation with the reverse de bruijn syntax,
-- and my end goal is to make it support optimal evaluation,
-- but currently it is not even close.
module LambdaCalculus.Evaluation.Optimal where

import Control.Applicative ((<|>))
import Data.Type.Nat
import LambdaCalculus.Representation.Dependent.ReverseDeBruijn

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
reduce :: Expression n -> Maybe (Expression n)
-- Note: there are no expressions which are reducible in multiple ways.
-- Only one of these cases can apply at once.
reduce expr = scopeReduce expr <|> substitute expr <|> betaReduce expr <|> etaReduce expr

-- | A subexpression to which a reduction step may be applied is called a "redex",
-- short for "reducible expression".
isRedex :: Expression n -> Bool
isRedex expr = isScopeRedex expr || isBetaRedex expr || isEtaRedex expr

-- | Beta reduction describes how functions behave when applied to arguments.
-- To reduce a function application, you simply 'substitute` the parameter into the function body.
--
-- Beta reduction is the fundamental computation step of the lambda calculus.
-- Only this rule is necessary for the lambda calculus to be turing-complete;
-- the other reductions merely define *equivalences* between expressions,
-- so that every expression has a canonical form.
betaReduce :: Expression n -> Maybe (Expression n)
betaReduce (Application (Abstraction fe) xe) = Just $ Substitution SZ xe fe
-- (^) Aside: 'Application' represents function application in the lambda calculus.
-- Haskell convention would be to name the function `f` and the argument `x`,
-- but that often collides with Haskell functions and arguments,
-- so instead I will be calling them `fe` and `xe`,
-- where the `e` stands for "expression".
betaReduce  _                = Nothing

-- TODO: Document this.
substitute :: Expression n -> Maybe (Expression n)
substitute (Substitution SZ     x Variable)         = Just x
substitute (Substitution (SS _) _ Variable)         = Just Variable
substitute (Substitution SZ     x (Drop body)) = Just body
substitute (Substitution (SS n) x (Drop body)) = Just $ Drop $ Substitution n x body
substitute (Substitution n      x (Application fe xe)) = Just $ Application (Substitution n x fe) (Substitution n x xe)
substitute (Substitution n      x (Abstraction body))  = Just $ Abstraction $ Substitution (SS n) x body
substitute _                            = Nothing

-- | A predicate determining whether a subexpression would allow a beta-reduction step.
isBetaRedex :: Expression n -> Bool
isBetaRedex (Application (Abstraction _) _) = True
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
etaReduce :: Expression n -> Maybe (Expression n)
etaReduce (Abstraction (Application (Drop fe) Variable)) = Just fe
etaReduce _                         = Nothing

-- | A predicate determining whether a subexpression would allow an eta-reduction step.
isEtaRedex :: Expression n -> Bool
isEtaRedex (Abstraction (Application (Drop _) Variable )) = True
isEtaRedex _                         = False

-- | Eta-expansion, the inverse of 'etaReduce'.
etaExpand :: Expression n -> Expression n
etaExpand fe = Abstraction $ Application (Drop fe) Variable

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
scopeReduce :: Expression n -> Maybe (Expression n)
scopeReduce (Application (Drop fe) (Drop xe)) = Just $ Drop $ Application fe xe
-- TODO: I feel like there's a more elegant way to represent this.
-- It feels like `Abstraction (Drop body)` should be its own atomic unit.
-- Maybe I could consider a combinator-based representation,
-- where `Abstraction (Drop body)` is just the `K` combinator `K body`?
scopeReduce (Abstraction (Drop (Drop body)))  = Just $ Drop $ Abstraction $ Drop body
scopeReduce _                         = Nothing

-- | A predicate determining whether a subexpression would allow a scope-reduction step.
isScopeRedex :: Expression n -> Bool
isScopeRedex (Application (Drop _) (Drop _)) = True
isScopeRedex (Abstraction (Drop (Drop _)))   = True
isScopeRedex _                       = False

-- | Scope-expansion, the left inverse of 'scopeReduce'.
scopeExpand :: Expression n -> Maybe (Expression n)
scopeExpand (Drop (Application fe xe))         = Just $ Application (Drop fe) (Drop xe)
scopeExpand (Drop (Abstraction (Drop body))) = Just $ Abstraction $ Drop $ Drop body
scopeExpand _                                = Nothing

-- | An expression is in "normal form" if it contains no redexes (see 'isRedex').
isNormal :: Expression n -> Bool
isNormal expr = not (isRedex expr) && case expr of
  -- In addition to this expression not being a redex,
  -- we must check that none of its subexpressions are redexes either.
  Application fe xe -> isNormal fe && isNormal xe
  Abstraction e     -> isNormal e
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
eval :: Expression n -> Expression n
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
          Application fe xe -> Application (eval fe) (eval xe)
          Abstraction e -> Abstraction (eval e)
          Drop e -> Drop (eval e)
          x -> x
