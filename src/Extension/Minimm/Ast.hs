{-|
Module      : Extension.Minimm.Ast
Description : Abstract Syntax Tree (AST) for the Mini-- language

This module defines the core syntax tree structures for the Mini-- language, a minimal
imperative language designed to manipulate boolean values.

=== AST Overview

The Mini-- language consists of:

* **Variables** — Named boolean placeholders (e.g. @x@, @flag@).
* **Literals** — Boolean constants: @true@ and @false@.
* **Boolean expressions** — Expressions involving variables, literals, negation, and binary operations.
* **Statements** — Program instructions including assignments, conditionals, I/O, and return.
* **Programs** — The top-level representation, containing procedure arguments and a sequence of statements.

=== Key Types

* 'Relator' — Supported binary boolean operators: @And@, @Or@, @Impl@, @Equiv@, @Xor@.
* 'BoolExpr' — Boolean expressions formed using variables, literals, negation, and binary operators.
* 'Statement' — Executable instructions like @Assign@, @If@, @Print@, etc.
* 'Program' — A complete Mini-- program, consisting of argument declarations and a body of statements.

These types are designed to be used with the Mini-- parser and verification tools.
-}
module Extension.Minimm.Ast where

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
