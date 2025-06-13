{-|
Module      : Model.TS
Description : Core data types for representing labeled transition systems (TS)

This module defines the core types used to represent labeled transition systems for CTL model checking.
A transition system consists of states, transitions between them via actions, a set of initial states,
and a labeling of states with propositions that hold in them.

It also includes utilities for pretty-printing and exporting a TS to [Graphviz DOT format](https://graphviz.org/doc/info/lang.html).
-}

module Model.TS (TS(..), State(..), Action(..), Transition(..), Proposition(..), Label(..), toDot) where
import Data.List (intercalate)

-- | A state in a transition system.
newtype State = State {
    name :: String
} deriving (Show, Eq, Ord)

-- | An action labeling a transition.
newtype Action = Act {
    action :: String
} deriving (Show, Eq, Ord)

-- | A transition between two states under a specific action.
data Transition = Trans {
    from    :: State,
    through :: Action,
    to      :: State
} deriving (Show, Eq, Ord)

-- | A named atomic proposition.
newtype Proposition = Prop {
    prop :: String
} deriving (Show, Eq, Ord)

-- | Associates a proposition with a state, representing that the proposition holds in that state.
data Label = Label {
    state :: State,
    lProp :: Proposition
} deriving (Show, Eq, Ord)

-- | A labeled transition system (TS).
--
-- It consists of:
--
-- * a list of all states
-- * the actions that may occur
-- * a list of transitions
-- * the initial states
-- * the propositions defined in the system
-- * the labels connecting states to propositions
data TS = TS {
    states  :: [State],
    actions :: [Action],
    trans   :: [Transition],
    initial :: [State],
    props   :: [Proposition],
    labels  :: [Label]
} deriving (Eq)

-- Pretty-print TS

instance Show TS where
    show (TS st a ts i p l) =
        "states:\n" ++
        "  [" ++ intercalate ", " (map name st) ++ "]\n" ++
        "actions:\n" ++
        "  [" ++ intercalate ", " (map action a ) ++ "]\n" ++
        (if null ts then "transitions:\n  []\n" else
        "transitions: [\n" ++
        "  " ++ intercalate ",\n  " (map pTr ts) ++ "\n]\n") ++
        "initial:\n" ++
        "  [" ++ intercalate ", " (map name i ) ++ "]\n" ++
        "propositions:\n" ++
        "  [" ++ intercalate ", " (map pPr p ) ++ "]\n" ++
        (if null l then "labels:\n  []" else
        "labels: [\n" ++
        "  " ++ intercalate ",\n  " (map pLb l) ++ "\n]")
        where
            pTr (Trans s1 a0 s2)
                = "(" ++ name s1 ++ ", " ++ action a0 ++ ", " ++ name s2 ++ ")"
            pPr (Prop pr) = pr
            pLb lbl = "(" ++ name (state lbl) ++ ", " ++ pPr (lProp lbl) ++ ")"

-- | Converts a transition system to a Graphviz DOT-format string.
--
-- This allows visualization of the TS using tools like Graphviz.
-- Initial states are marked with incoming arrows from invisible points.
-- State labels include proposition names.
--
-- Example usage:
--
-- > putStrLn $ toDot myTS
toDot :: TS -> String
toDot (TS st _ tsList initStates _ lbls) =
    "digraph TS {\n" ++
    "  rankdir=LR;\n" ++
    "  node [shape=ellipse];\n" ++
    concatMap renderState st ++
    concatMap renderInitArrow initStates ++
    concatMap renderTrans tsList ++
    "}"
  where
    renderState :: State -> String
    renderState s =
      let properties = [prop p | Label s' p <- lbls, s' == s]
          labelText = name s ++
                      if null properties then "" else "\\n{" ++ intercalate ", " properties ++ "}"
          baseAttrs = ["label=\"" ++ labelText ++ "\""]
          styleAttrs = if s `elem` initStates
                         then ["style=filled", "fillcolor=lightgray"]
                         else []
          attrs = baseAttrs ++ styleAttrs
      in "  \"" ++ name s ++ "\" [" ++ intercalate ", " attrs ++ "];\n"

    renderInitArrow :: State -> String
    renderInitArrow s =
      "  \"init_" ++ name s ++ "\" [shape=point];\n" ++
      "  \"init_" ++ name s ++ "\" -> \"" ++ name s ++ "\";\n"

    renderTrans :: Transition -> String
    renderTrans (Trans fromS act toS) =
      "  \"" ++ name fromS ++ "\" -> \"" ++ name toS ++
      "\" [label=\"" ++ action act ++ "\"];\n"
