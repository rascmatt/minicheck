{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib.Verify.Check (verify) where

import Lib.Model.CTL (CTL(..), LogicalOperator(..), PathFormula(..))
import Lib.Model.TS (TS, State, states, labels, state, prop, lProp, trans, from, to, initial)
import Data.List (nub, (\\))

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

-- TODO: Assert that all propositions in CTL are also in TS

verify :: TS -> CTL -> Bool
verify ts ctl = ia `subset` satSet
  where
    satSet = satFun ts (toENF ctl)
    ia     = initial ts

satFun :: TS -> ENF -> [State]

-- Sat(true) := S
satFun ts ETruth = states ts

-- Sat(a) := { s ∈ S | a ∈ L(s) }, for any a ∈ AP
satFun ts (EAtomicProposition p) = statesWithLabel ts p
  where statesWithLabel t x = (nub . map state . filter (\l -> x == prop (lProp l))) (labels t)

-- Sat(ɸ && Ψ) := Sat(ɸ) ∩ Sat(Ψ)
satFun ts (EConjunction a b) = satFun ts a `intersect` satFun ts b

-- Sat(⌐ɸ) := S \ Sat(ɸ)
satFun ts (ENegation p) = filter (`notElem` satFun ts p) (states ts)

-- Sat(∃X ɸ) := { s ∈ S | (Post(s) ∩ Sat(ɸ)) != ∅ }
satFun ts (ENext p) = (nub . filter hasNext) (states ts)
  where
    satP      = satFun ts p
    hasNext s = (not . null) (post ts s `intersect` satP)

-- Sat(∃(ɸ 𝒰 Ψ))
satFun ts (EUntil p q) = satFunUntil ts satP t
  where
    t = satFun ts q
    satP = satFun ts p

-- Sat(∃G ɸ)
satFun ts (EGlobally p) = satFunGlobally ts t
  where t = satFun ts p

satFunGlobally :: TS -> [State] -> [State]
satFunGlobally ts t
  | null del  = t
  | otherwise = satFunGlobally ts newT
    where
      del = [s | s <- t, null (post ts s `intersect` t)]
      newT = t `diff` del

satFunUntil :: TS -> [State] -> [State] -> [State]
satFunUntil ts satP t
  | null add  = t
  | otherwise = satFunUntil ts satP newT
    where
      add = [ s | s <- satP `diff` t, (not . null) (post ts s `intersect` t)]
      newT = t `union` add

-- Utility functions

post :: TS -> State -> [State]
post ts s = (map to . filter (\t -> from t == s)) (trans ts)

intersect :: (Eq a) => [a] -> [a] -> [a]
intersect xs ys = filter (`elem` ys) xs

union :: (Eq a) => [a] -> [a] -> [a]
union xs ys = nub (xs ++ ys)

diff :: (Eq a) => [a] -> [a] -> [a]
diff xs ys = xs \\ ys

subset :: (Eq a) => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs

