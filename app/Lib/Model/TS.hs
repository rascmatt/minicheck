module Lib.Model.TS (TS, PTS(..), State(..), Action(..), Transition(..), Proposition(..), Label(..)) where
import Data.List (intercalate)

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

-- Pretty-print TS

newtype PTS = P TS
instance Show PTS where
    show (P (st,a,ts,i,p,l)) =
        "states:\n" ++
        "  [" ++ intercalate ", " (map pSt st) ++ "]\n" ++
        "actions:\n" ++
        "  [" ++ intercalate ", " (map pAc a ) ++ "]\n" ++
        (if null ts then "transitions:\n  []\n" else
        "transitions: [\n" ++
        "    " ++ intercalate ",\n    " (map pTr ts) ++ "\n]\n") ++
        "initial:\n" ++
        "  [" ++ intercalate ", " (map pSt i ) ++ "]\n" ++
        "propositions:\n" ++
        "  [" ++ intercalate ", " (map pPr p ) ++ "]\n" ++
        "labels: [\n" ++
        "    " ++ intercalate ",\n    " (map pLb l) ++ "\n]"
        where
            pSt (State s) = s
            pAc (Act ac)  = ac
            pTr (Trans (s1, a0, s2)) 
                = "(" ++ pSt s1 ++ ", " ++ pAc a0 ++ ", " ++ pSt s2 ++ ")"
            pPr (Prop pr) = pr
            pLb (Label (s0, p0)) 
                = "(" ++ pSt s0 ++ ", " ++ pPr p0 ++ ")"