module Model.TS (TS(..), State(..), Action(..), Transition(..), Proposition(..), Label(..), toDot) where
import Data.List (intercalate)

newtype State = State {
    name :: String
} deriving (Show, Eq, Ord)

newtype Action = Act {
    action :: String
} deriving (Show, Eq, Ord)

data Transition = Trans {
    from    :: State,
    through :: Action,
    to      :: State
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
      let props = [prop p | Label s' p <- lbls, s' == s]
          labelText = name s ++
                      if null props then "" else "\\n{" ++ intercalate ", " props ++ "}"
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
