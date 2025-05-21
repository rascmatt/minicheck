{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib.Extension.Minimm.Transform (transform) where

import Lib.Extension.Minimm.Ast
import Lib.Model.TS
import Data.Set (toList, fromList)

data Context = Ctx {
    cProg :: [Statement],         -- The rest of the program to transform
    cStat :: String,              -- Name of the current state
    cTs   :: TS                   -- The current transition system
} deriving (Show, Eq)

transform :: Program -> TS
transform (Prog a s) = tCombine tss
    where
        -- All possible variable assignments
        assignments = subsets a
        -- For each initial assignment, create a state
        toname i = 'i':show i
        states = [State (toname i) | i <- [0.. length assignments - 1]]
        props  = [Prop ai | ai <- a]
        -- Label the states according to the atomic propositions true in that state
        labels = [Label (State (toname i)) p | i <- [0.. length assignments - 1], p <- [Prop ai | ai <- assignments !! i]]
        initialTS = TS states [] [] states props labels
        -- Create the transformation contexts
        ctxs = [Ctx s (toname i) initialTS | i <- [0.. length assignments - 1]]
        -- Get the resulting TS from each path of execution
        tss = map cTs (concatMap transformCtx ctxs)

transformCtx :: Context -> [Context]
transformCtx (Ctx [] c ts) = [Ctx [] c ts]

-- Rule 1: If Statement
transformCtx (Ctx ((If expr b):p) c ts)
    | tUndefined ctx expr = [tError ctx]
    | tEval ctx expr    = t1
    | otherwise           = t2
        where
            ctx = Ctx (If expr b:p) c ts
            a1  = tNext ctx "t" (b++p) []
            a2  = tNext ctx "f" p []
            t1 = transformCtx a1
            t2 = transformCtx a2

-- Rule 2: If-Else Statement
transformCtx (Ctx ((IfElse expr b1 b2):p) c ts)
    | tUndefined ctx expr = [tError ctx]
    | tEval ctx expr    = tt
    | otherwise         = tf
        where
            ctx = Ctx (IfElse expr b1 b2:p) c ts
            at  = tNext ctx "t" (b1++p) []
            af  = tNext ctx "f" (b2++p) []
            tt = transformCtx at
            tf = transformCtx af

-- Rule 3: Assignment
transformCtx (Ctx ((Assign v b):p) c ts)
    | tUndefined ctx b = [tError ctx]
    | otherwise = transformCtx (tNext ctx "a" p [(v, val)])
        where
            ctx = Ctx (Assign v b:p) c ts
            val = tEval ctx b

-- Rule 4: Print Statement
transformCtx (Ctx ((Print b):p) c ts)
    | tUndefined ctx b = [tError ctx]
    | otherwise = transformCtx (tNext ctx "p" p [])
        where
            ctx = Ctx (Print b:p) c ts

-- Rule 5: Read Statement
transformCtx (Ctx ((Read v):p) c ts) = transformCtx (tNext ctx "rt" p [(v, True)]) ++ transformCtx (tNext ctx "rf" p [(v, False)])
        where
            ctx = Ctx (Read v:p) c ts

-- Rule 6: Return Statement
transformCtx (Ctx ((Return b):p) c ts)
    | tUndefined ctx b = [tError ctx]
    | otherwise = transformCtx (tNext ctx "r" p [])
        where
            ctx = Ctx (Return b:p) c ts

-- Transition to the error state
tError :: Context -> Context
tError (Ctx _ s ts) = Ctx [] "e" (TS tSt tAc tTr tIn tPr tLb)
    where
        tSt = dedup (State "e":states ts)
        tAc = dedup (Act "_":actions ts)
        tIn = initial ts
        tTr = dedup (Trans (State s) (Act "_") (State "e") : trans ts)
        tPr = props ts
        tLb = labels ts

-- Utility functions

subsets :: [a] -> [[a]]
subsets []     = [[]]
subsets (x:xs) = let rest = subsets xs
                 in rest ++ map (x:) rest

-- Get the variables read by an expression
tVariables :: BoolExpr -> [Variable]
tVariables (Var v) = [v]
tVariables (Lit _) = []
tVariables (Not b) = tVariables b
tVariables (BinOp _ b1 b2) = dedup (tVariables b1 ++ tVariables b2)
tVariables (Nested b) = tVariables b

-- Check if any variables in BoolExpr are undefined
-- in the current context
tUndefined :: Context -> BoolExpr -> Bool
tUndefined (Ctx _ _ ts) b = any not [x `elem` p | x <- v]
    where
        p = map prop (props ts)
        v = tVariables b

-- Evaluate an expression in the given context
-- Return true if the BoolExpr evaluates to true
tEval :: Context -> BoolExpr -> Bool
tEval (Ctx _ c ts) (Var v) = v `elem` lbls
    where lbls = tProps ts c
tEval _ (Lit l) = l
tEval ctx (Not b) = not (tEval ctx b)
tEval ctx (BinOp r b1 b2)
    | r == And   = s1 && s2
    | r == Or    = s1 || s2
    | r == Impl  = not s1 || s2
    | r == Equiv = s1 == s2
    | r == Xor   = (s1 || s2) && not (s1 && s2)
        where
            s1 = tEval ctx b1
            s2 = tEval ctx b2
tEval ctx (Nested b) = tEval ctx b

-- Transition to the next state with the specified name, remaining
-- statements respecting the given variable assignments
-- (0) Add a new state to the transition system
-- (1) Add a transition from the current state to the next one
-- (2) Add the new atomic proposition based on the variable assignment
-- (3) Add correct labels based on active propositions in the next state
tNext :: Context -> String -> [Statement] -> [(Variable, Bool)] -> Context
tNext (Ctx _ c (TS sts act trs ini prs lbs)) n p v = Ctx p newName (
                TS (dedup (State newName:sts)) (dedup (Act "_":act))
                (dedup ([Trans (State x) (Act "_") (State newName) | x <- if null c then map name ini else [c]] ++ trs))
                ini
                (dedup ([Prop x | (x, _) <- v] ++ prs))
                (dedup ([Label (State newName) (Prop x) | x <- nextTrueProps] ++ lbs)))
        where
            newName = c ++ "." ++ n
            prevTrueProps = map (prop . lProp) (filter (\(Label (State x) _) -> x == c) lbs)
            aFalse = [x | (x, False) <- v]
            aTrue  = [x | (x, True) <- v]
            nextTrueProps = aTrue ++ filter (`notElem` aFalse) prevTrueProps

-- Combine multiple transition systems to one non deterministic one
tCombine :: [TS] -> TS
tCombine [ts] = ts
tCombine [TS s1 a1 t1 i1 p1 l1, TS  s2 a2 t2 i2 p2 l2] =
    TS (dedup (s1 ++ s2)) (dedup (a1 ++ a2)) (dedup (t1 ++ t2)) (dedup (i1 ++ i2)) (dedup (p1 ++ p2)) (dedup (l1 ++ l2))
tCombine (t:ts) = tCombine [t, tCombine ts]

-- Get the atomic propositions true in a given state in
-- a given transition system
tProps :: TS -> String -> [Variable]
tProps (TS _ _ _ _ _ lbls) s = map (prop . lProp) (filter (\(Label (State x) _) -> x == s) lbls)

dedup :: Ord a => [a] -> [a]
dedup = toList . fromList
