module Lib.Extension.Minimm.Ast where

type Variable = String

type Literal = Bool

data Relator = And | Or | Impl | Equiv | Xor
  deriving (Show, Eq)

data BoolExpr
  = Var Variable
  | Lit Literal
  | Not BoolExpr
  | BinOp Relator BoolExpr BoolExpr
  deriving (Show, Eq)

data Statement
  = If BoolExpr [Statement]
  | IfElse BoolExpr [Statement] [Statement]
  | Assign Variable BoolExpr
  | Print BoolExpr
  | Read Variable
  | Return BoolExpr
  deriving (Show, Eq)

data Program = Prog [Variable] [Statement]
  deriving (Show, Eq)
