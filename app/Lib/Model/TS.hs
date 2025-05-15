module Lib.Model.TS (TS, State(..), Action(..), Transition(..), Proposition(..), Label(..)) where

newtype State = State String
    deriving (Show, Eq, Ord)

newtype Action = Act String
    deriving (Show, Eq, Ord)

newtype Transition = Trans (State, Action, State)
    deriving (Show, Eq, Ord)

newtype Proposition = Prop String
    deriving (Show, Eq, Ord)

newtype Label = Label (State, Proposition)
    deriving (Show, Eq, Ord)

type TS = ([State], [Action], [Transition], [State], [Proposition], [Label])