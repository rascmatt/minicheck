module Lib.Parser.TS (parse, labels, validate) where

import Lib.Parser.Base
import Lib.Model.TS
import Data.Char (isLetter, isDigit)
import Data.Set (toList, fromList)

mIdent :: Parse String
mIdent = (neList . sat) (\c -> isLetter c || isDigit c)

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
    return (Trans (s0, a, s1))

mProposition :: Parse Proposition
mProposition = do Prop <$> mIdent

mLabel :: Parse Label
mLabel = do
    (s, p) <- m2Tuple mState mProposition
    return (Label (s, p))

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
    _       <- (mOptional . mSection) ["s", "state", "states"]
    states  <- mElemListNe mState -- Not empty
    _       <- (mOptional . mSection) ["a", "action", "actions"]
    actions <- mElemList mAction
    _       <- (mOptional . mSection) ["t", "trans", "transition", "transitions"]
    trans   <- mElemList mTransition
    _       <- (mOptional . mSection) ["i", "init", "initial"]
    initial <- mElemListNe mState -- Not empty
    _       <- (mOptional . mSection) ["p", "props", "propositions"]
    props   <- mElemList mProposition
    _       <- (mOptional . mSection) ["l", "lable", "label", "lables", "labels"]
    lbls    <- mElemList mLabel
    return (states, actions, trans, initial, props, lbls)

parse :: String -> Maybe TS
parse = topLevel mTS

validate :: Maybe TS -> Maybe TS
validate Nothing = Nothing
validate (Just ts)
    | valid normalized = Just normalized
    | otherwise        = Nothing
    where normalized = transform ts

-- Utility

labels :: TS -> State -> [Proposition]
labels (_,_,_,_,_,[]) _         = []
labels (_,_,_,_,_,ls) (State s) = mapped
    where
        filtered = filter (\(Label (State x, _)) -> x == s) ls
        mapped   = map (\(Label (_, pp)) -> pp) filtered

-- Transformation

dedup :: Ord a => [a] -> [a]
dedup = toList . fromList

-- For a terminal state (no transition exists) add a self-loop
addSinkStates :: TS -> TS
addSinkStates (st,a,ts,i,p,l) = (st, acts, ts ++ sinkTs, i, p, l)
    where
        isTerminal s = not (any (\(Trans (t,_,_)) -> t == s) ts)
        sinkTs = [Trans (t, Act "_", t) | t <- st, isTerminal t]
        acts   = if null sinkTs then a else Act "_" : a

-- Normalize the labels:
-- * add the state as label
normLabels :: TS -> TS
normLabels (st,a,ts,i,p,l) = (st,a,ts,i,p, nLabels)
    where
        name (State s) = s
        labs s = filter (\(Label (State x, _)) -> x == name s) l
        props  s = Prop (name s) : map (\(Label (_, pp)) -> pp) (labs s)
        normalized s = dedup (props s)
        nLabels = [Label (s, pr) | s <- st, pr <- normalized s]

-- Deduplicate the lists
deduplicateTs :: TS -> TS
deduplicateTs (st,a,ts,i,p,l)
    = (dedup st, dedup a, dedup ts, dedup i, dedup p, dedup l)

-- Apply all transformations
transform :: TS -> TS
transform = addSinkStates . normLabels . deduplicateTs

-- Semantic validation

-- Validate that there is at least one initial state
validateInitial :: TS -> Bool
validateInitial (_,_,_,[],_,_) = False
validateInitial _ = True

-- Validate that all referenced states are defined in the
-- set of states
validateStates :: TS -> Bool
validateStates (st,_,ts,i,_,l) = length deduped == length st
    where
        trans = concat [[t1, t2] | Trans (t1, _, t2) <- ts]
        label = [s | Label (s, _) <- l]
        deduped = dedup (trans ++ label ++ i)

-- Validate that every action referenced in the transitions
-- is defined in the set of available actions
validateActions :: TS -> Bool
validateActions (_,a,ts,_,_,_) = length a == length (dedup tsActions)
    where
        tsActions = map (\(Trans (_, aa, _)) -> aa) ts

-- Apply all validations
valid :: TS -> Bool
valid ts = all (\p -> p ts) [validateInitial, validateStates, validateActions]