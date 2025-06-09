module Extension.TransformSpec (spec) where

import Test.Hspec

import Extension.Minimm.Parser (mProgram)
import Extension.Minimm.Ast
import Extension.Minimm.Transform
import Parser.Base (Parse(..), topLevel, unbox)
import Model.TS (TS (..), State(..), Transition(..), Label (..), Proposition (..))
import Control.Monad (when, unless)

spec :: Spec
spec = do
    describe "Transform Mini--" $ do
        it "initial states" $ do
            let ts = parse "procedure main(a){ return a; }"
            ts `hasStates` ["origin", "i0", "i1", "i0.r", "i1.r"]
            ts `hasInitial` ["origin"]
            ts `hasTransitions` [("origin", "i0"), ("origin", "i1")]
            ts `hasTransitions` [("i0", "i0.r"), ("i1", "i1.r")]
        it "transform if - undefined var" $ do
            let ts = parse "procedure main(a){ if (b) {} return a; }"
            ts `hasTransitions` [("i0", "e"), ("i1", "e")]
        it "transform if - evaluate condition" $ do
            let ts = parse "procedure main(a){ if (a) { b = a; } return a; }"
            ts `hasLabels`      [("i0", "a", False), ("i1", "a", True)]
            ts `hasTransitions` [("i0", "i0.f"), ("i0.f", "i0.f.r")]
            ts `hasTransitions` [("i1", "i1.t"), ("i1.t", "i1.t.a"), ("i1.t.a", "i1.t.a.r")]
            ts `hasLabels`      [("i0.f.r", "b", False), ("i1.t.a", "b", True), ("i1.t.a.r", "b", True)]
        it "transform if/else - undefined var" $ do
            let ts = parse "procedure main(a){ if (b) {} else {} return a; }"
            ts `hasTransitions` [("i0", "e"), ("i1", "e")]
        it "transform if/else - evaluate condition" $ do
            let ts = parse "procedure main(a){ if (a) { b = true; } else { c = true; } return a; }"
            ts `hasLabels`      [("i0", "a", False), ("i1", "a", True)]
            ts `hasTransitions` [("i0", "i0.f"), ("i0.f", "i0.f.a"), ("i0.f.a", "i0.f.a.r")]
            ts `hasTransitions` [("i1", "i1.t"), ("i1.t", "i1.t.a"), ("i1.t.a", "i1.t.a.r")]
            ts `hasLabels`      [("i0.f.a", "c", True), ("i0.f.a.r", "c", True), ("i0.f.a.r", "b", False)]
            ts `hasLabels`      [("i1.t.a", "b", True), ("i1.t.a.r", "b", True), ("i1.t.a.r", "c", False)]
        it "transform assignment - assign label" $ do
            let ts = parse "procedure main(a){ b = a; return b; }"
            ts `hasStates`      ["origin", "i0", "i1", "i0.a", "i1.a", "i0.a.r", "i1.a.r"]
            ts `hasTransitions` [("i0", "i0.a"), ("i0.a", "i0.a.r")]
            ts `hasTransitions` [("i1", "i1.a"), ("i1.a", "i1.a.r")]
            ts `hasLabels`      [("i0", "b", False), ("i0.a", "b", False), ("i0.a.r", "b", False)]
            ts `hasLabels`      [("i1", "b", False), ("i1.a", "b", True), ("i1.a.r", "b", True)]
        it "transform assignment - undefined var" $ do
            let ts = parse "procedure main(a){ b = c; return b; }"
            ts `hasStates`      ["origin", "i0", "i1", "e"]
            ts `hasTransitions` [("i0", "e"), ("i1", "e")]
        it "transform assignment - evaluate expression" $ do
            let ts = parse "procedure main(a){ b = a ^ (a & false); return b; }"
            ts `hasStates`      ["origin", "i0", "i1", "i0.a", "i1.a", "i0.a.r", "i1.a.r"]
            ts `hasTransitions` [("i0", "i0.a"), ("i0.a", "i0.a.r")]
            ts `hasTransitions` [("i1", "i1.a"), ("i1.a", "i1.a.r")]
            ts `hasLabels`      [("i0", "b", False), ("i0.a", "b", False), ("i0.a.r", "b", False)]
            ts `hasLabels`      [("i1", "b", False), ("i1.a", "b", True), ("i1.a.r", "b", True)]
        it "transform assignment - evaluate expression with undefined var" $ do
            let ts = parse "procedure main(a){ b = a ^ (c & false); return b; }"
            ts `hasStates`      ["origin", "i0", "i1", "e"]
            ts `hasTransitions` [("i0", "e"), ("i1", "e")]
        it "transform pint - undefined var" $ do
            let ts = parse "procedure main(a){ print_bool(a ^ (c & false)); return b; }"
            ts `hasStates`      ["origin", "i0", "i1", "e"]
            ts `hasTransitions` [("i0", "e"), ("i1", "e")]
        it "transform pint - defined var" $ do
            let ts = parse "procedure main(a){ print_bool(a); return a; }"
            ts `hasStates`      ["origin", "i0", "i1", "i0.p", "i1.p", "i0.p.r", "i1.p.r"]
            ts `hasTransitions` [("i0", "i0.p"), ("i0.p", "i0.p.r")]
            ts `hasTransitions` [("i1", "i1.p"), ("i1.p", "i1.p.r")]
            ts `hasLabels`      [("i0.p.r", "a", False), ("i1.p.r", "a", True)]
        it "transform read - non deterministic" $ do
            let ts = parse "procedure main(a){ b = read_bool(); return b; }"
            ts `hasStates`      ["origin", "i0", "i1", "i0.rt", "i0.rf", "i1.rt", "i1.rf", "i0.rt.r", "i0.rf.r", "i1.rt.r", "i1.rf.r"]
            ts `hasTransitions` [("i0", "i0.rt"), ("i0", "i0.rf"), ("i0.rt", "i0.rt.r"), ("i0.rf", "i0.rf.r")]
            ts `hasTransitions` [("i1", "i1.rt"), ("i1", "i1.rf"), ("i1.rt", "i1.rt.r"), ("i1.rf", "i1.rf.r")]
            ts `hasLabels`      [("i0.rt", "b", True), ("i0.rf", "b", False)]
            ts `hasLabels`      [("i1.rt", "b", True), ("i1.rf", "b", False)]
        it "transform return - undefined var" $ do
            let ts = parse "procedure main(a){ return b; }"
            ts `hasStates`      ["origin", "i0", "i1", "e"]
            ts `hasTransitions` [("i0", "e"), ("i1", "e")]
        it "transform return - expression" $ do
            let ts = parse "procedure main(a){ return a ^ (a & false); }"
            ts `hasStates`      ["origin", "i0", "i1", "i0.r", "i1.r"]


hasStates :: TS -> [String] -> Expectation
hasStates ts ss = do
    let st = [ s | (State s) <- states ts]
    let condition = all (`elem` st) ss
    unless condition $
        expectationFailure $ "Expected states " ++ show ss ++ " but were " ++ show st

hasInitial :: TS -> [String] -> Expectation
hasInitial ts ss = do
    let st = [ s | (State s) <- states ts]
    let condition = all (`elem` st) ss
    unless condition $
        expectationFailure $ "Expected initial states " ++ show ss ++ " but were " ++ show st

hasTransition :: TS -> (String, String) -> Expectation
hasTransition ts (a, b) = do
    let transitions = [x ++ "-" ++ y | (Trans (State x) _ (State y)) <- trans ts, a == x, b == y]
    when (null transitions) $
        expectationFailure $ "Expected a transition from " ++ a ++ " to " ++ b

hasTransitions :: TS -> [(String, String)] -> Expectation
hasTransitions ts []      = return ()
hasTransitions ts (t:trs) = do
    hasTransition ts t
    hasTransitions ts trs

hasLabel :: TS -> (String, String, Bool) -> Expectation
hasLabel ts (a, b, c) = do
    let ls = [id | (Label (State x) (Prop p)) <- labels ts, a == x, b == p]
    when (c && null ls) $
        expectationFailure $ "Expected state " ++ a ++ " to have label " ++ b
    when (not c && (not . null) ls) $
        expectationFailure $ "Expected state " ++ a ++ " to not have label " ++ b

hasLabels :: TS -> [(String, String, Bool)] -> Expectation
hasLabels ts []      = return ()
hasLabels ts (l:lbs) = do
    hasLabel  ts l
    hasLabels ts lbs


parse :: String -> TS
parse input = case results of
        []         -> error "syntax error"
        (result:_) -> transform result
    where results = [ found | (found, []) <- unbox mProgram input]