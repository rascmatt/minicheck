module Lib.Verify.Check (verify) where

import Lib.Model.CTL (CTL(..), LogicalOperator(..), PathFormula(..))
import Lib.Model.TS (TS)

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
toENF (ForAll (Eventually x)) = toENF $ ForAll (Until Truth x)
toENF (ForAll (Globally x)) = toENF $ Negation (Exists (Eventually (Negation x)))

-- Does the CTL formula hold in the given TS?
verify :: TS -> CTL -> Bool
verify _ _ = False
