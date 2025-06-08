module Lib.Parser.CTL (parse, ctlFormula) where

import Lib.Parser.Base
import Lib.Model.CTL
import Data.Char (isLower, isDigit)
-- import Control.Monad (mfilter)

parse :: String -> Maybe CTL
parse = topLevel ctlFormula

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
proposition = AtomicProposition <$> token mIdent

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

    -- TODO
    mfilter :: (a -> Bool) -> Parse a -> Parse a
    mfilter f m = do {x <- m; if f x then return x else mzero}

-- TODO: Import from TS.hs ?
mIdent :: Parse String
mIdent = many1 $ sat (\c -> isLower c || isDigit c || elem c ['_', '.', '-'])
