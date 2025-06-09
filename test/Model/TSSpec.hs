module Model.TSSpec (spec) where

import Test.Hspec

import Model.TS

spec :: Spec
spec = do
  describe "Pretty-Print TS" $ do
    it "TS show simple" $ do
      show (TS 
          [State "s0", State "s1"] 
          [Act "a", Act "b"] 
          [Trans (State "s0") (Act "a") (State "s0"), Trans (State "s0") (Act "b") (State "s1")] 
          [State "s0", State "s1"] 
          [Prop "p", Prop "q"] 
          [Label (State "s0") (Prop "p"), Label (State "s1") (Prop "q")])
        `shouldBe` 
        "states:\n  [s0, s1]\nactions:\n  [a, b]\ntransitions: [\n  (s0, a, s0),\n  (s0, b, s1)\n]\ninitial:\n  [s0, s1]\npropositions:\n  [p, q]\nlabels: [\n  (s0, p),\n  (s1, q)\n]"
      
    it "TS empty lists" $ do
      show (TS [State "s0"] [] [] [State "s0"] [] [])
        `shouldBe` 
        "states:\n  [s0]\nactions:\n  []\ntransitions:\n  []\ninitial:\n  [s0]\npropositions:\n  []\nlabels:\n  []"

  describe "DOT Graph TS" $ do
    it "TS show simple" $ do
      toDot (TS 
          [State "s0", State "s1"] 
          [Act "a", Act "b"] 
          [Trans (State "s0") (Act "a") (State "s0"), Trans (State "s0") (Act "b") (State "s1")] 
          [State "s0", State "s1"] 
          [Prop "p", Prop "q"] 
          [Label (State "s0") (Prop "p"), Label (State "s1") (Prop "q")])
        `shouldBe` 
        "digraph TS {\n  rankdir=LR;\n  node [shape=ellipse];\n  \"s0\" [label=\"s0\\n{p}\", style=filled, fillcolor=lightgray];\n  \"s1\" [label=\"s1\\n{q}\", style=filled, fillcolor=lightgray];\n  \"init_s0\" [shape=point];\n  \"init_s0\" -> \"s0\";\n  \"init_s1\" [shape=point];\n  \"init_s1\" -> \"s1\";\n  \"s0\" -> \"s0\" [label=\"a\"];\n  \"s0\" -> \"s1\" [label=\"b\"];\n}"