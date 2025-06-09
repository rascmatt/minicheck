module Extension.Minimm.Parser where

import Extension.Minimm.Ast
import Parser.Base

import Data.Char (isDigit, isLower)

mIdentChar :: Parse Char
mIdentChar = sat (\c -> isLower c || isDigit c || c == '_')

mIdent :: Parse Variable
mIdent = do
    c0   <- (skipWs . sat)  isLower
    rest <- list mIdentChar
    return (c0:rest)

mBool :: Parse Literal
mBool = do
    s <- (skipWs . strings) ["true", "false"]
    return (s == "true")

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

mBoolExprNest :: Parse BoolExpr
mBoolExprNest =
    do { Lit <$> skipWs mBool;  } `mplus`
    do { Var <$> skipWs mIdent; } `mplus`
    do
        _ <- (skipWs . sat) (== '(')
        e <- skipWs mBoolExpr
        _ <- (skipWs . sat) (== ')')
        return e

mBoolExpr :: Parse BoolExpr
mBoolExpr =
    skipWs mBoolExprNest `mplus`
    do { _ <- (skipWs . sat) (== '!'); Not <$> skipWs mBoolExprNest; } `mplus`
    do
        e1 <- skipWs mBoolExprNest
        rl <- skipWs mRelator
        BinOp rl e1 <$> skipWs mBoolExprNest

mPrintBool :: Parse Statement
mPrintBool = do
    _ <- (skipWs . string) "print_bool"
    _ <- (skipWs . sat) (== '(')
    b <- skipWs mBoolExpr
    _ <- (skipWs . sat) (== ')')
    _ <- (skipWs . sat) (== ';')
    return (Print b)

mReadBool :: Parse Statement
mReadBool = do
    v <- skipWs mIdent
    _ <- (skipWs . sat) (== '=')
    _ <- (skipWs . string) "read_bool"
    _ <- (skipWs . sat) (== '(')
    _ <- (skipWs . sat) (== ')')
    _ <- (skipWs . sat) (== ';')
    return (Read v)

mAssignment :: Parse Statement
mAssignment = do
    v <- skipWs mIdent
    _ <- (skipWs . sat) (== '=')
    b <- skipWs mBoolExpr
    _ <- (skipWs . sat) (== ';')
    return (Assign v b)

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

mReturnStatement :: Parse Statement
mReturnStatement = do
    _ <- (skipWs . string) "return"
    _ <- sat isWs
    b <- skipWs mBoolExpr
    _ <- (skipWs . sat) (== ';')
    return (Return b)

mStatements :: Parse [Statement]
mStatements = list (
        mIfStatement      `mplus`
        mIfElseStatement  `mplus`
        mAssignment       `mplus`
        mPrintBool        `mplus`
        mReadBool)

mArguments :: Parse [Variable]
mArguments = do
    a0 <- skipWs mIdent
    aa <- list (do { _ <- (skipWs . sat) (== ','); skipWs mIdent})
    return (a0:aa)

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
    return (Prog a (s++[r]))

parse :: String -> Maybe Program
parse = topLevel mProgram