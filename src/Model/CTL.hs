{-|
Module      : Model.CTL
Description : Abstract syntax tree for Computation Tree Logic (CTL)

Defines the core data types for representing CTL formulas, including
logical operators, temporal path operators, and quantifiers over computation paths.
-}

module Model.CTL where

import Model.Pattern

-- | CTL formula representation.
data CTL
  = Truth
  | Falsity
  | AtomicProposition Pattern
  | BinaryOperation LogicalOperator CTL CTL
  | Negation CTL
  | Exists PathFormula
  | ForAll PathFormula
  deriving (Show, Eq)

-- | Logical binary operators.
data LogicalOperator
  = Conjunction
  | Disjunction
  | Implication
  | Equivalence
  | ExclusiveDisjunction
  deriving (Show, Eq)

-- | CTL path formulas.
data PathFormula
  = Next CTL
  | Until CTL CTL
  | Eventually CTL
  | Globally CTL
  deriving (Show, Eq)
