{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-|
Module      : Verify.Check
Description : CTL Model Checking for Transition Systems

This module implements a model checker for CTL (Computation Tree Logic) formulas
over transition systems, using Existential Normal Form (ENF) as an intermediate
representation. The primary entry point is the 'verify' function, which checks
whether a given CTL formula holds in all initial states of a transition system.

The module includes:

- A definition of the 'ENF' data type representing formulas in Existential Normal Form.
- A transformation from arbitrary CTL formulas to ENF via 'toENF'.
- A fixpoint-based satisfaction function 'satFun' that computes the set of states
  satisfying a given ENF formula.
- Helpers for evaluating temporal operators like "globally" and "until".

This implementation follows the standard semantics for existential CTL operators,
and translates universal ones via dualities.

== Example

Assuming a transition system `ts` and a CTL formula `ctl`:

@
verify ts ctl  -- returns True if the formula holds in all initial states
@

-}

module Verify.Check (verify, toENF, ENF(..)) where

import Model.CTL (CTL(..), LogicalOperator(..), PathFormula(..))
import Model.TS (TS, State, states, labels, state, prop, lProp, trans, from, to, initial)
import Data.List (nub, (\\))
import Data.Set (Set, fromList, isSubsetOf, intersection, difference, union)
import qualified Data.Set as Set (filter)

data ENF -- Existential Normal Form
  = ETruth
  | EAtomicProposition String
  | EConjunction ENF ENF
  | ENegation ENF
  | ENext ENF
  | EUntil ENF ENF
  | EGlobally ENF
  deriving (Show, Eq)

toENF :: CTL -> ENF
toENF Truth = ETruth
toENF Falsity = ENegation ETruth
toENF (AtomicProposition p) = EAtomicProposition p
toENF (BinaryOperation Conjunction x y) = EConjunction (toENF x) (toENF y)
toENF (BinaryOperation Disjunction x y) = ENegation (EConjunction (ENegation $ toENF x) (ENegation $ toENF y))
toENF (BinaryOperation Implication x y) = toENF $ BinaryOperation Disjunction (Negation x) y
toENF (BinaryOperation Equivalence x y) = toENF $ BinaryOperation Conjunction (BinaryOperation Implication x y) (BinaryOperation Implication y x)
toENF (BinaryOperation ExclusiveDisjunction x y) = toENF $ (BinaryOperation Disjunction (BinaryOperation Conjunction x (Negation y)) (BinaryOperation Conjunction (Negation x) y))
toENF (Negation x) = ENegation (toENF x)
toENF (Exists (Next x)) = ENext (toENF x)
toENF (Exists (Until x y)) = EUntil (toENF x) (toENF y)
toENF (Exists (Eventually x)) = EUntil ETruth (toENF x)
toENF (Exists (Globally x)) = EGlobally (toENF x)
toENF (ForAll (Next x)) = ENegation (ENext (ENegation $ toENF x))
toENF (ForAll (Until x y)) = ENegation (EConjunction (EUntil (ENegation $ toENF y) (EConjunction (ENegation $ toENF x) (ENegation $ toENF y))) (ENegation (EGlobally (ENegation $ toENF y))))
toENF (ForAll (Eventually x)) = ENegation (EGlobally (ENegation $ toENF x))
toENF (ForAll (Globally x)) = toENF $ Negation (Exists (Eventually (Negation x)))

-- | Check whether a CTL formula holds for all initial states of a transition system.
--
-- Given a transition system and a CTL formula, this function evaluates whether
-- the formula is satisfied in all of the system's initial states.
--
-- === Parameters
--
-- * @ts@ - The 'TS' transition system to evaluate the formula against.
--         It defines the states, transitions, initial states, and proposition labeling.
--
-- * @ctl@ - The 'CTL' (Computation Tree Logic) formula to verify.
--          It is automatically converted to ENF (Existential Normal Form) before evaluation.
--
-- === Returns
--
-- Returns 'True' if the CTL formula holds in all initial states of the transition system.
-- Returns 'False' otherwise.
--
-- === Example
--
-- > verify myTransitionSystem (EX (Prop "p"))
--
verify :: TS -> CTL -> Bool
verify ts ctl = ia `isSubsetOf` satSet
  where
    satSet = satFun ts (toENF ctl)
    ia     = fromList $ initial ts

satFun :: TS -> ENF -> Set State

-- Sat(true) := S
satFun ts ETruth = fromList $ states ts

-- Sat(a) := { s ∈ S | a ∈ L(s) }, for any a ∈ AP
satFun ts (EAtomicProposition p) = fromList $ statesWithLabel ts p
  where statesWithLabel t x = (map state . filter (\l -> x == prop (lProp l))) (labels t)

-- Sat(ɸ && Ψ) := Sat(ɸ) ∩ Sat(Ψ)
satFun ts (EConjunction a b) = satFun ts a `intersection` satFun ts b

-- Sat(⌐ɸ) := S \ Sat(ɸ)
satFun ts (ENegation p) = fromList (states ts) `difference` satFun ts p

-- Sat(∃X ɸ) := { s ∈ S | (Post(s) ∩ Sat(ɸ)) != ∅ }
satFun ts (ENext p) = Set.filter hasNext (fromList $ states ts)
  where
    satP      = satFun ts p
    hasNext s = (not . null) (post ts s `intersection` satP)

-- Sat(∃(ɸ 𝒰 Ψ))
satFun ts (EUntil p q) = satFunUntil ts satP t
  where
    t    = satFun ts q
    satP = satFun ts p

-- Sat(∃G ɸ)
satFun ts (EGlobally p) = satFunGlobally ts t
  where t = satFun ts p

satFunGlobally :: TS -> Set State -> Set State
satFunGlobally ts t
  | null del  = t
  | otherwise = satFunGlobally ts newT
    where
      del  = Set.filter (\s -> null (post ts s `intersection` t)) t
      newT = t `difference` del

satFunUntil :: TS -> Set State -> Set State -> Set State
satFunUntil ts satP t
  | null add  = t
  | otherwise = satFunUntil ts satP newT
    where
      add  = Set.filter (\s -> (not . null) (post ts s `intersection` t)) satP `difference` t
      newT = t `union` add

-- Utility functions

post :: TS -> State -> Set State
post ts s = fromList $ (map to . filter (\t -> from t == s)) (trans ts)


