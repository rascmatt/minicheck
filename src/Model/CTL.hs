module Model.CTL where

data CTL
  = Truth
  | Falsity
  | AtomicProposition String
  | BinaryOperation LogicalOperator CTL CTL
  | Negation CTL
  | Exists PathFormula
  | ForAll PathFormula
  deriving (Show, Eq)

data LogicalOperator
  = Conjunction
  | Disjunction
  | Implication
  | Equivalence
  | ExclusiveDisjunction
  deriving (Show, Eq)

data PathFormula
  = Next CTL
  | Until CTL CTL
  | Eventually CTL
  | Globally CTL
  deriving (Show, Eq)
