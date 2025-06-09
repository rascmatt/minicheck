{-|
Module      : Parser.TS
Description : Parser for textual representations of labeled transition systems (TS)

This module defines a parser for labeled transition systems (TS) used in CTL model checking.

It supports a relaxed syntax with optional section headers (e.g. @states:@, @transitions:@),
and bracketed lists of elements. The parser automatically normalizes the resulting
transition system by deduplicating elements, ensuring at least self-loops for terminal states,
and adding the state's own name as a label.

The module also includes semantic validation of the transition system, checking for:

- presence of initial states
- validity of state references in transitions and labels
- validity of action and proposition usage

Use 'parse' to read a TS from a string, which will automatically 'normalize' to ensure a well-formed TS.

This module is intended to work with `Model.TS` and integrates with the CTL model checking backend.
-}

module Parser.TS where

import Parser.Base
import Model.TS
import Data.Char (isLower, isDigit)
import Data.Set (toList, fromList)

mIdent :: Parse String
mIdent = (neList . sat) (\c -> isLower c || isDigit c || elem c ['_', '.', '-'])

mBrackets :: Parse a -> Parse a
mBrackets p = do
    _ <- skipWs (sat (== '['))
    r <- skipWs p
    _ <- skipWs (sat (== ']'))
    return r

mElems :: Parse a -> Parse [a]
mElems pElem = do
    e0 <- skipWs pElem
    ee <- list (do { _ <- skipWs (sat (== ',')); skipWs pElem})
    return (e0:ee)

mElemListNe :: Parse a -> Parse [a]
mElemListNe = mBrackets . mElems

mElemList :: Parse a -> Parse [a]
mElemList p = mBrackets (pure [] `mplus` mElems p)

m2Tuple :: Parse a -> Parse b -> Parse (a, b)
m2Tuple p1 p2 = do
    _ <- skipWs (sat (== '('))
    a <- skipWs p1
    _ <- skipWs (sat (== ','))
    b <- skipWs p2
    _ <- skipWs (sat (== ')'))
    return (a, b)

m3Tuple :: Parse a -> Parse b -> Parse c -> Parse (a, b, c)
m3Tuple p1 p2 p3 = do
    _ <- skipWs (sat (== '('))
    a <- skipWs p1
    _ <- skipWs (sat (== ','))
    b <- skipWs p2
    _ <- skipWs (sat (== ','))
    c <- skipWs p3
    _ <- skipWs (sat (== ')'))
    return (a, b, c)

mState :: Parse State
mState = do State <$> mIdent

mAction :: Parse Action
mAction = do Act <$> mIdent

mTransition :: Parse Transition
mTransition = do
    (s0, a, s1) <- m3Tuple mState mAction mState
    return (Trans s0 a s1)

mProposition :: Parse Proposition
mProposition = do Prop <$> mIdent

mLabel :: Parse Label
mLabel = do
    (s, p) <- m2Tuple mState mProposition
    return (Label s p)

mAlt :: [String] -> Parse String
mAlt = foldr (mplus . string) mzero

mSection :: [String] -> Parse String
mSection s = do
    res <- skipWs (mAlt s)
    _   <- (mOptional . skipWs . string ) ":"
    return res

mOptional :: Parse String -> Parse String
mOptional p = pure "" `mplus` p

mTS :: Parse TS
mTS = do
    _ <- (mOptional . mSection) ["s", "state", "states"]
    s  <- mElemListNe mState -- Not empty
    _ <- (mOptional . mSection) ["a", "action", "actions"]
    a <- mElemList mAction
    _ <- (mOptional . mSection) ["t", "trans", "transition", "transitions"]
    t <- mElemList mTransition
    _ <- (mOptional . mSection) ["i", "init", "initial"]
    i <- mElemListNe mState -- Not empty
    _ <- (mOptional . mSection) ["p", "props", "propositions"]
    p <- mElemList mProposition
    _ <- (mOptional . mSection) ["l", "lable", "label", "lables", "labels"]
    l <- mElemList mLabel
    return (TS s a t i p l)

-- | Parse a transition system from a string.
-- Accepts optional section headers and relaxed formatting.
-- Returns a normalized transition system if successful.
parse :: String -> Maybe TS
parse input = normalize <$> topLevel mTS input

-- Transformation

dedup :: Ord a => [a] -> [a]
dedup = toList . fromList

-- For a terminal state (no transition exists) add a self-loop
addSinkStates :: TS -> TS
addSinkStates (TS st a ts i p l) = TS st acts (dedup (ts ++ sinkTs)) i p l
    where
        isTerminal s = not (any (\(Trans t _ _) -> t == s) ts)
        sinkTs = [Trans t (Act "_") t | t <- st, isTerminal t]
        acts   = dedup (if null sinkTs then a else Act "_" : a)

-- Normalize the labels:
-- * add the state as label
normLabels :: TS -> TS
normLabels (TS st a ts i p l)  = TS st a ts i nProps nLabels
    where
        labs s = filter (\(Label (State x) _) -> x == name s) l
        pp   s = Prop (name s) : map lProp (labs s)
        normalized s = dedup (pp s)
        nLabels = [Label s pr | s <- st, pr <- normalized s]
        nProps  = dedup (p ++ [ Prop ((prop . lProp) x) | x <- nLabels ])

-- Deduplicate the lists
deduplicateTs :: TS -> TS
deduplicateTs (TS st a ts i p l)
    = TS (dedup st) (dedup a) (dedup ts) (dedup i) (dedup p) (dedup l)

-- | Normalize a transition system by:
-- * Deduplicating all sets (states, actions, etc.)
-- * Ensuring each state has at least one outgoing transition (self-loop if necessary)
-- * Adding each state name as an atomic proposition label for itself
normalize :: TS -> TS
normalize = addSinkStates . normLabels . deduplicateTs

-- Semantic validation

-- Validate that there is at least one initial state
validateInitial :: TS -> Bool
validateInitial (TS _ _ _ [] _ _) = False
validateInitial _ = True

-- Validate that all referenced states are defined in the
-- set of states
validateStates :: TS -> Bool
validateStates (TS st _ ts i _ l) = not (any (`notElem` st) refs)
    where
        tt    = concat [[t1, t2] | Trans t1 _ t2 <- ts]
        label = [s | Label s _ <- l]
        refs  = tt ++ label ++ i

-- Validate that every action referenced in the transitions
-- is defined in the set of available actions
validateActions :: TS -> Bool
validateActions (TS _ a ts _ _ _) = not (any (`notElem` a) refs)
    where
        refs = [ r | (Trans _ r _) <- ts]

-- Validate that every proposition referenced as a label
-- is defined in the set of propositions
validateProps :: TS -> Bool
validateProps (TS _ _ _ _ ps ls) = not (any (`notElem` ps) refs)
    where
        refs = [ p | (Label _ p) <- ls]

-- | Validate the semantic correctness of a transition system.
-- The system is considered valid if all of the following conditions hold:
--
-- * At least one initial state is defined.
-- * All referenced states in transitions, labels, and initial states are declared.
-- * All actions used in transitions are declared.
-- * All propositions used in labels are declared.
--
-- Returns 'True' if the system passes all validations; otherwise, 'False'.
validate :: TS -> Bool
validate ts = all (\p -> p ts) [validateInitial, validateStates, validateActions, validateProps]
