{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-|
Module      : Parser.CTL
Description : Parser for Computation Tree Logic (CTL) formulas

Parses CTL formulas from strings into their abstract syntax tree representation
defined in 'Model.CTL'. Supports standard logical and temporal CTL operators,
including nested and associative expressions.
-}
module Parser.CTL (parse, ctlFormula) where

import Model.Pattern
import Parser.Base
import Model.CTL
import Control.Monad (mplus, mfilter)
import Data.Char (isLower, isDigit)

-- | Parse a full CTL formula from a string.
-- Returns 'Just CTL' on success, or 'Nothing' if parsing fails.
parse :: String -> Maybe CTL
parse = topLevel ctlFormula

-- | Entry point for parsing a CTL formula using the 'Parse' monad.
ctlFormula :: Parse CTL
ctlFormula = space >> stateFormula

stateFormula :: Parse CTL
stateFormula =
    stateFormulaParen
    `mplus` associativeOperation
    `mplus` (BinaryOperation Implication <$> stateFormulaParen <*> (symbol "=>" >> stateFormulaParen))
    `mplus` (Exists <$> (symbol "E" >> pathFormula))
    `mplus` (ForAll <$> (symbol "A" >> pathFormula))

stateFormulaParen :: Parse CTL
stateFormulaParen =
    (Truth <$ symbol "True")
    `mplus` (Falsity <$ symbol "False")
    `mplus` proposition
    `mplus` (Negation <$> (symbol "!" >> stateFormulaParen))
    `mplus` do {symbol "("; sf <- stateFormula; symbol ")"; return sf }

pathFormula :: Parse PathFormula
pathFormula = do
    (Next <$> (symbol "X" >> stateFormulaParen))
    `mplus` (Until <$> stateFormulaParen <*> (symbol "U" >> stateFormulaParen))
    `mplus` (Globally <$> (symbol "G" >> stateFormulaParen))
    `mplus` (Eventually <$> (symbol "F" >> stateFormulaParen))

proposition :: Parse CTL
proposition = do
    prop <- token $ greedyMany1 $ sat allowedChar
    return $ AtomicProposition $ makePattern prop
  where
    allowedChar c = isLower c || isDigit c || elem c ['_', '.', '-', '*', '#']

associativeOperation :: Parse CTL
associativeOperation = do
    x <- stateFormulaParen
    op <- assocOp
    rest <- stateFormulaParen `sepby1` mfilter (== op) assocOp
    return $ foldr1 (BinaryOperation op) (x:rest)
  where
    assocOp :: Parse LogicalOperator
    assocOp =
        (Conjunction <$ symbol "&&")
        `mplus` (Disjunction <$ symbol "||")
        `mplus` (Equivalence <$ symbol "==")
        `mplus` (ExclusiveDisjunction <$ symbol "!=")
