module Lib.Parser.TS (parse) where

import Lib.Parser.Base
import Lib.Model.TS
import Data.Char (isLetter, isDigit)

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

mElemList :: Parse a -> Parse [a]
mElemList = mBrackets . mElems

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
    states  <- mElemList mState
    _       <- (mOptional . mSection) ["a", "action", "actions"]
    actions <- mElemList mAction
    _       <- (mOptional . mSection) ["t", "trans", "transition", "transitions"]
    trans   <- mElemList mTransition
    _       <- (mOptional . mSection) ["i", "init", "initial"]
    initial <- mElemList mState
    _       <- (mOptional . mSection) ["p", "props", "propositions"]
    props   <- mElemList mProposition
    _       <- (mOptional . mSection) ["l", "lable", "label", "lables", "labels"]
    labels  <- mElemList mLabel
    return (states, actions, trans, initial, props, labels)

parse :: String -> Maybe TS
parse = topLevel mTS