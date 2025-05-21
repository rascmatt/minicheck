module Lib.Model.TS (TS(..), State(..), Action(..), Transition(..), Proposition(..), Label(..)) where
import Data.List (intercalate)

newtype State = State {
    name :: String
} deriving (Show, Eq, Ord)

newtype Action = Act {
    action :: String
} deriving (Show, Eq, Ord)

data Transition = Trans {
    from :: State,
    when :: Action,
    to   :: State
} deriving (Show, Eq, Ord)

newtype Proposition = Prop {
    prop :: String
} deriving (Show, Eq, Ord)

data Label = Label {
    state :: State,
    lProp :: Proposition
} deriving (Show, Eq, Ord)

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