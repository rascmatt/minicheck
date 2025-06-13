{-|
Module      : Extension.Minimm.Parser
Description : Parser for the Mini-- language

Parses Mini-- source code into its abstract syntax tree as defined in "Extension.Minimm.Ast".
-}

module Extension.Minimm.Parser where

import Extension.Minimm.Ast
import Parser.Base
import Control.Monad (mplus, mzero)

import Data.Char (isDigit, isLower)

-- | Parser for a single identifier character: lowercase letter, digit, or underscore.
mIdentChar :: Parse Char
mIdentChar = sat (\c -> isLower c || isDigit c || c == '_')

-- | Parser for identifiers.
-- Starts with a lowercase letter, followed by any combination of lowercase letters, digits, or underscores.
mIdent :: Parse Variable
mIdent = do
    c0   <- (skipWs . sat)  isLower
    rest <- list mIdentChar
    return (c0:rest)

-- | Parser for a boolean literal: @true@ or @false@.
mBool :: Parse Literal
mBool = do
    s <- (skipWs . strings) ["true", "false"]
    return (s == "true")

-- | Parser for logical relators used in binary boolean expressions.
-- Maps MiniMM operators to 'Relator' constructors:
--
-- * @&@ → 'And'
-- * @|@ → 'Or'
-- * @=>@ → 'Impl'
-- * @=@ → 'Equiv'
-- * @^@ → 'Xor'
mRelator :: Parse Relator
mRelator = do
    r <- (skipWs . strings) ["&", "|", "=>", "=", "^"]
    case r of
        "&"  -> return And
        "|"  -> return Or
        "=>" -> return Impl
        "="  -> return Equiv
        "^"  -> return Xor
        _    -> mzero

-- | Parser for a nested boolean expression.
-- Matches literals, variables, or parenthesized subexpressions.
mBoolExprNest :: Parse BoolExpr
mBoolExprNest =
    do { Lit <$> skipWs mBool;  } `mplus`
    do { Var <$> skipWs mIdent; } `mplus`
    do
        _ <- (skipWs . sat) (== '(')
        e <- skipWs mBoolExpr
        _ <- (skipWs . sat) (== ')')
        return e

-- | Parser for full boolean expressions.
--
-- Supports:
--
-- * Literals: @true@, @false@
-- * Variables
-- * Unary negation: @!expr@
-- * Binary operations: @expr op expr@
-- * Parenthesized subexpressions
mBoolExpr :: Parse BoolExpr
mBoolExpr =
    skipWs mBoolExprNest `mplus`
    do { _ <- (skipWs . sat) (== '!'); Not <$> skipWs mBoolExprNest; } `mplus`
    do
        e1 <- skipWs mBoolExprNest
        rl <- skipWs mRelator
        BinOp rl e1 <$> skipWs mBoolExprNest

-- | Parser for a @print_bool(expr);@ statement.
mPrintBool :: Parse Statement
mPrintBool = do
    _ <- (skipWs . string) "print_bool"
    _ <- (skipWs . sat) (== '(')
    b <- skipWs mBoolExpr
    _ <- (skipWs . sat) (== ')')
    _ <- (skipWs . sat) (== ';')
    return (Print b)

-- | Parser for a @x = read_bool();@ input statement.
mReadBool :: Parse Statement
mReadBool = do
    v <- skipWs mIdent
    _ <- (skipWs . sat) (== '=')
    _ <- (skipWs . string) "read_bool"
    _ <- (skipWs . sat) (== '(')
    _ <- (skipWs . sat) (== ')')
    _ <- (skipWs . sat) (== ';')
    return (Read v)

-- | Parser for variable assignment: @x = expr;@
mAssignment :: Parse Statement
mAssignment = do
    v <- skipWs mIdent
    _ <- (skipWs . sat) (== '=')
    b <- skipWs mBoolExpr
    _ <- (skipWs . sat) (== ';')
    return (Assign v b)

-- | Parser for an @if (cond) { ... }@ statement.
mIfStatement :: Parse Statement
mIfStatement = do
    _ <- (skipWs . string) "if"
    _ <- (skipWs . sat) (== '(')
    b <- skipWs mBoolExpr
    _ <- (skipWs . sat) (== ')')
    _ <- (skipWs . sat) (== '{')
    s <- mStatements
    _ <- (skipWs . sat) (== '}')
    return (If b s)

-- | Parser for an @if (cond) { ... } else { ... }@ statement.
-- Parses by extending 'mIfStatement'.
mIfElseStatement :: Parse Statement
mIfElseStatement = do
    i <- mIfStatement
    _ <- (skipWs . string) "else"
    _ <- (skipWs . sat) (== '{')
    e <- mStatements
    _ <- (skipWs . sat) (== '}')
    case i of
      (If b t) -> return (IfElse b t e)
      _ -> mzero

-- | Parser for @return expr;@ statement.
mReturnStatement :: Parse Statement
mReturnStatement = do
    _ <- (skipWs . string) "return"
    _ <- sat isWs
    b <- skipWs mBoolExpr
    _ <- (skipWs . sat) (== ';')
    return (Return b)

-- | Parser for a sequence of statements, including conditionals, assignments,
-- print, input, and return statements.
mStatements :: Parse [Statement]
mStatements = list (
        mIfStatement      `mplus`
        mIfElseStatement  `mplus`
        mAssignment       `mplus`
        mPrintBool        `mplus`
        mReadBool)

-- | Parser for a comma-separated list of procedure argument variables.
mArguments :: Parse [Variable]
mArguments = do
    a0 <- skipWs mIdent
    aa <- list (do { _ <- (skipWs . sat) (== ','); skipWs mIdent})
    return (a0:aa)

-- | Parser for a complete MiniMM program.
--
-- Expects the following structure:
--
-- > procedure main(arg1, arg2, ...) {
-- >   <statements>
-- >   return <expr>;
-- > }
mProgram :: Parse Program
mProgram = do
    _ <- (skipWs . string) "procedure"
     -- Require the two keywords to be separated by whitespace (i.e. 'proceduremain' is invalid)
    _ <- sat isWs
    _ <- (skipWs . string) "main"
    _ <- (skipWs . sat) (== '(')
    a <- mArguments
    _ <- (skipWs . sat) (== ')')
    _ <- (skipWs . sat) (== '{')
    s <- mStatements
    r <- mReturnStatement
    _ <- (skipWs . sat) (== '}')
    _ <- space
    return (Prog a (s++[r]))

-- | Entry point for parsing a full MiniMM program from a string.
--
-- Returns 'Just Program' on success, or 'Nothing' on failure.
parse :: String -> Maybe Program
parse = topLevel mProgram
