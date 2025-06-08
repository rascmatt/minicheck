module Parser.TSSpec (spec) where

import Test.Hspec

import Lib.Parser.Base (topLevel, string)
import Lib.Parser.TS
import Lib.Model.TS

spec :: Spec
spec = do
    describe "Parser.TS (parse)" $ do
        it "parse identifier" $ do
            topLevel mIdent "a" `shouldBe` Just "a"
            topLevel mIdent "abc" `shouldBe` Just "abc"
            topLevel mIdent "a1" `shouldBe` Just "a1"
            topLevel mIdent "a.b" `shouldBe` Just "a.b"
            topLevel mIdent "a_b" `shouldBe` Just "a_b"
            topLevel mIdent "a-b" `shouldBe` Just "a-b"
            topLevel mIdent "" `shouldBe` Nothing
            topLevel mIdent "A" `shouldBe` Nothing
        it "parse state" $ do
            topLevel mState "st" `shouldBe` Just (State "st")
        it "parse action" $ do
            topLevel mAction "act" `shouldBe` Just (Act "act")
        it "parse transition" $ do
            topLevel mTransition "(s0, a, s1)" `shouldBe` Just (Trans (State "s0") (Act "a") (State "s1"))
        it "parse proposition" $ do
            topLevel mProposition "prop" `shouldBe` Just (Prop "prop")
        it "parse label" $ do
            topLevel mLabel "(s0, a)" `shouldBe` Just (Label (State "s0") (Prop "a"))
        it "parse helper optional" $ do
            topLevel ((mOptional . string) "abc") "x" `shouldBe` Nothing
            topLevel ((mOptional . string) "abc") "" `shouldBe` Just ""
            topLevel ((mOptional . string) "abc") "abc" `shouldBe` Just "abc"
        it "parse helper elemList" $ do
            topLevel ((mElemList . string) "a") "x" `shouldBe` Nothing
            topLevel ((mElemList . string) "a") "" `shouldBe` Nothing
            topLevel ((mElemList . string) "a") "[]" `shouldBe` Just []
            topLevel ((mElemList . string) "a") "[a]" `shouldBe` Just["a"]
            topLevel ((mElemList . string) "a") "[a,]" `shouldBe` Nothing
            topLevel ((mElemList . string) "a") "[,]" `shouldBe` Nothing
            topLevel ((mElemList . string) "a") "[,a]" `shouldBe` Nothing
            topLevel ((mElemList . string) "a") "[a,a]" `shouldBe` Just ["a", "a"]
            topLevel (mElemList mIdent) "[ a , b ]" `shouldBe` Just ["a", "b"]
        it "parse helper elemListNe" $ do
            topLevel (mElemListNe mIdent) "[]" `shouldBe` Nothing
            topLevel (mElemListNe mIdent) "[a]" `shouldBe` Just["a"]
            topLevel (mElemListNe mIdent) "[a,b]" `shouldBe` Just["a", "b"]
        it "parse ts" $ do
            topLevel mTS "[s0,s1][a][(s0,a,s1)][s0][p][(s0,p)]" `shouldBe` Just (TS 
                [State "s0", State "s1"] 
                [Act "a"] 
                [Trans (State "s0") (Act "a") (State "s1")] 
                [State "s0"] [Prop "p"] 
                [Label (State "s0") (Prop "p")])
            topLevel mTS "s: [s0,s1] a: [a] t: [(s0,a,s1)] i: [s0] p: [p] l: [(s0,p)]" `shouldBe` Just (TS 
                [State "s0", State "s1"] 
                [Act "a"] 
                [Trans (State "s0") (Act "a") (State "s1")] 
                [State "s0"] [Prop "p"] 
                [Label (State "s0") (Prop "p")])
            topLevel mTS "s: [s0] a: [] t: [] i: [s0] p: [] l: []" `shouldBe` Just (TS 
                [State "s0"] [] [] [State "s0"] [] [])
            topLevel mTS "s: [s0] a: [] t: [] i: []   p: [] l: []" `shouldBe` Nothing
            topLevel mTS "s: []   a: [] t: [] i: [s0] p: [] l: []" `shouldBe` Nothing
            topLevel mTS "s: []   a: [] t: [] i: []   p: [] l: []" `shouldBe` Nothing
    describe "Parser.TS (transform)" $ do
        it "sink state" $ do
            -- Add a self-loop for all states which do not have an outgoing transition
            addSinkStates 
                (TS [State "s0"] [] [] [State "s0"] [] []) 
                `shouldBe` 
                (TS [State "s0"] [Act "_"] [Trans (State "s0") (Act "_") (State "s0")] [State "s0"] [] [])
        it "deduplicate" $ do
            -- Deduplicate all elements of the transition system
            deduplicateTs 
                (TS [State "s0", State "s0"] [Act "a", Act "a"] [Trans (State "s0") (Act "a") (State "s0"), Trans (State "s0") (Act "a") (State "s0")] [State "s0", State "s0"] [Prop "p", Prop "p"] [Label (State "s0") (Prop "p"), Label (State "s0") (Prop "p")]) 
                `shouldBe`
                (TS [State "s0"] [Act "a"] [Trans (State "s0") (Act "a") (State "s0")] [State "s0"] [Prop "p"] [Label (State "s0") (Prop "p")])
        it "normalize labels" $ do
            -- Add a label for each state
            normLabels
                (TS [State "s0"] [] [] [State "s0"] [] []) 
                `shouldBe`
                (TS [State "s0"] [] [] [State "s0"] [Prop "s0"] [Label (State "s0") (Prop "s0")]) 
        it "normalize all" $ do
            -- Apply all transformations
            normalize
                (TS [State "s0"] [] [] [State "s0"] [] []) 
                `shouldBe`
                (TS [State "s0"] [Act "_"] [Trans (State "s0") (Act "_") (State "s0")] [State "s0"] [Prop "s0"] [Label (State "s0") (Prop "s0")])
    describe "Parser.TS (validate)" $ do
        it "validate initial" $ do
            validateInitial (TS [State "s0"] [] [] [] [] []) `shouldBe` False
            validateInitial (TS [State "s0"] [] [] [State "s0"] [] []) `shouldBe` True
        it "validate states" $ do
            -- Validate occurences in transitions
            validateStates 
                (TS [State "s0"] [] [Trans (State "s0") (Act "_") (State "s0")] [] [] []) `shouldBe` True
            validateStates
                (TS [State "s0"] [] [Trans (State "s0") (Act "_") (State "s1")] [] [] []) `shouldBe` False
            validateStates
                (TS [State "s0"] [] [Trans (State "s1") (Act "_") (State "s0")] [] [] []) `shouldBe` False
            validateStates
                (TS [State "s0"] [] [Trans (State "s1") (Act "_") (State "s1")] [] [] []) `shouldBe` False
            -- Validate occurences in initial states
            validateStates
                (TS [State "s0"] [] [] [State "s0"] [] []) `shouldBe` True
            validateStates
                (TS [State "s0"] [] [] [State "s1"] [] []) `shouldBe` False
            -- Validate occurences in labels
            validateStates
                (TS [State "s0"] [] [] [] [] [Label (State "s0") (Prop "p")]) `shouldBe` True
            validateStates
                (TS [State "s0"] [] [] [] [] [Label (State "s1") (Prop "p")]) `shouldBe` False
        it "validate actions" $ do
            -- Validate occurences of actions in transitions
            validateActions 
                (TS [State "s0"] [Act "a"] [Trans (State "s0") (Act "_") (State "s0")] [] [] []) `shouldBe` False
            validateActions
                (TS [State "s0"] [Act "a", Act "_"] [Trans (State "s0") (Act "_") (State "s0")] [] [] []) `shouldBe` True
        it "validate propositions" $ do
            -- Validate occurences of propositions in labels
            validateProps
                (TS [State "s0"] [] [] [] [Prop "p"] [Label (State "s0") (Prop "q")]) `shouldBe` False
            validateProps
                (TS [State "s0"] [] [] [] [Prop "p"] [Label (State "s0") (Prop "p")]) `shouldBe` True
