module Verify.CheckSpec (spec) where

import Test.Hspec

import qualified Parser.TS as TSParser (parse)
import qualified Parser.CTL as CTLParser (parse)

import Model.TS (TS(TS), State(State))
import Model.CTL

import Verify.Check

spec :: Spec
spec = do
    describe "Verify CTL" $ do
        it "literal true" $ do
            c  <- ctl "True"
            verify minimal c `shouldBe` True
        it "literal false" $ do
            c  <- ctl "False"
            verify minimal c `shouldBe` False
        it "atomic proposition true" $ do
            t <- ts  "data/ts/soda.txt"
            c <- ctl "pay" -- is true in the initial state
            verify t c `shouldBe` True
        it "atomic proposition false" $ do
            t <- ts  "data/ts/soda.txt"
            c <- ctl "select" -- is false in the initial state
            verify t c `shouldBe` False
        it "conjunction" $ do
            t  <- ts  "data/ts/ts6.txt"
            c1 <- ctl "p && q"
            verify t c1 `shouldBe` True
            c2 <- ctl "p && r"
            verify t c2 `shouldBe` False
        it "negation" $ do
            t  <- ts  "data/ts/ts6.txt"
            c1 <- ctl "!r"
            verify t c1 `shouldBe` True
            c2 <- ctl "!p"
            verify t c2 `shouldBe` False
        it "next true" $ do
            t <- ts  "data/ts/soda.txt"
            c1 <- ctl "EX select"
            c2 <- ctl "AX select"
            verify t c1 `shouldBe` True
            verify t c2 `shouldBe` True
        it "next false" $ do
            t <- ts  "data/ts/soda.txt"
            c1 <- ctl "EX soda"
            c2 <- ctl "AX soda"
            verify t c1 `shouldBe` False
            verify t c2 `shouldBe` False
        it "until true" $ do
            t <- ts  "data/ts/soda.txt"
            c1 <- ctl "E pay U select"
            c2 <- ctl "E (pay || select) U beer"
            verify t c1 `shouldBe` True
            verify t c2 `shouldBe` True
        it "until false" $ do
            t <- ts  "data/ts/soda.txt"
            c1 <- ctl "E pay U beer"
            c2 <- ctl "E beer U soda"
            verify t c1 `shouldBe` False
            verify t c2 `shouldBe` False
        it "exists eventually true" $ do
            t <- ts  "data/ts/soda.txt"
            c1 <- ctl "EF beer"
            c2 <- ctl "EF soda"
            c3 <- ctl "EF pay"
            verify t c1 `shouldBe` True
            verify t c2 `shouldBe` True
            verify t c3 `shouldBe` True
        it "exists eventually false" $ do
            t <- ts  "data/ts/soda.txt"
            c1 <- ctl "EF (soda && beer)"
            verify t c1 `shouldBe` False
        it "all eventually true" $ do
            t <- ts  "data/ts/soda.txt"
            c1 <- ctl "AF pay"
            c2 <- ctl "AF select"
            verify t c1 `shouldBe` True
            verify t c2 `shouldBe` True
        it "all eventually false" $ do
            t <- ts  "data/ts/soda.txt"
            c1 <- ctl "AF beer"
            c2 <- ctl "AF soda"
            verify t c1 `shouldBe` False
            verify t c2 `shouldBe` False
        it "globally true" $ do
            t <- ts  "data/ts/ts7.txt"
            c1 <- ctl "EG p"
            c2 <- ctl "AG p"
            verify t c1 `shouldBe` True
            verify t c2 `shouldBe` True
        it "globally false" $ do
            t <- ts  "data/ts/ts7.txt"
            c1 <- ctl "EG r"
            c2 <- ctl "AG r"
            c3 <- ctl "EG q"
            c4 <- ctl "EG q"
            verify t c1 `shouldBe` False
            verify t c2 `shouldBe` False
            verify t c3 `shouldBe` False
            verify t c4 `shouldBe` False
        it "soda: AF pay" $ do
            t <- ts  "data/ts/soda.txt"
            c <- ctl "AF pay"
            verify t c `shouldBe` True
        it "soda: EF soda" $ do
            t <- ts  "data/ts/soda.txt"
            c <- ctl "EF soda"
            verify t c `shouldBe` True
        it "soda: AG (select => (AX !select))" $ do
            t <- ts  "data/ts/soda.txt"
            c <- ctl "AG (select => (AX !select))"
            verify t c `shouldBe` True
        it "soda: AF soda" $ do
            t <- ts  "data/ts/soda.txt"
            c <- ctl "AF soda"
            verify t c `shouldBe` False
        it "soda: EG (select => (AX soda))" $ do
            t <- ts  "data/ts/soda.txt"
            c <- ctl "EG (select => (AX soda))"
            verify t c `shouldBe` False
    
    describe "Convert to ENF" $ do
        it "Truth" $ do
            toENF Truth `shouldBe` ETruth
        it "Falsity" $ do
            toENF Falsity `shouldBe` ENegation ETruth
        it "Atomic Proposition" $ do
            toENF (AtomicProposition "a") `shouldBe` EAtomicProposition "a"
        it "Conjunction" $ do
            toENF (BinaryOperation Conjunction (AtomicProposition "a") (AtomicProposition "b")) 
            `shouldBe` EConjunction (EAtomicProposition "a") (EAtomicProposition "b")
        it "Disjunction" $ do
            toENF (BinaryOperation Disjunction (AtomicProposition "a") (AtomicProposition "b")) 
            `shouldBe` 
            ENegation (EConjunction (ENegation (EAtomicProposition "a")) (ENegation (EAtomicProposition "b")))
        it "Implication" $ do
            toENF (BinaryOperation Implication Truth Falsity) 
            `shouldBe` 
            ENegation (EConjunction (ENegation (ENegation ETruth)) (ENegation (ENegation ETruth)))
        it "Equivalence" $ do
            toENF (BinaryOperation Equivalence Truth Falsity) 
            `shouldBe` 
            EConjunction (ENegation (EConjunction (ENegation (ENegation ETruth)) (ENegation (ENegation ETruth)))) (ENegation (EConjunction (ENegation (ENegation (ENegation ETruth))) (ENegation ETruth)))
        it "Xor" $ do
            toENF (BinaryOperation ExclusiveDisjunction Truth Falsity) 
            `shouldBe` 
            ENegation (EConjunction (ENegation (EConjunction ETruth (ENegation (ENegation ETruth)))) (ENegation (EConjunction (ENegation ETruth) (ENegation ETruth))))
        it "Negation" $ do
            toENF (Negation (AtomicProposition "a")) `shouldBe` ENegation (EAtomicProposition "a")
        it "Exists Next" $ do
            toENF (Exists (Next (AtomicProposition "p")))
                `shouldBe` ENext (EAtomicProposition "p")
        it "Exists Until" $ do
            toENF (Exists (Until (AtomicProposition "a") (AtomicProposition "b")))
                `shouldBe` EUntil (EAtomicProposition "a") (EAtomicProposition "b")
        it "Exists Eventually" $ do
            toENF (Exists (Eventually (AtomicProposition "p")))
                `shouldBe` EUntil ETruth (EAtomicProposition "p")
        it "Exists Globally" $ do
            toENF (Exists (Globally (AtomicProposition "p")))
                `shouldBe` EGlobally (EAtomicProposition "p")
        it "ForAll Next" $ do
            toENF (ForAll (Next (AtomicProposition "p")))
                `shouldBe` ENegation (ENext (ENegation (EAtomicProposition "p")))
        it "ForAll Until" $ do
            toENF (ForAll (Until (AtomicProposition "a") (AtomicProposition "b")))
                `shouldBe` ENegation (EConjunction
                    (EUntil (ENegation (EAtomicProposition "b"))
                            (EConjunction (ENegation (EAtomicProposition "a")) (ENegation (EAtomicProposition "b"))))
                    (ENegation (EGlobally (ENegation (EAtomicProposition "b")))))
        it "ForAll Eventually" $ do
            toENF (ForAll (Eventually (AtomicProposition "p")))
                `shouldBe` ENegation (EGlobally (ENegation (EAtomicProposition "p")))
        it "ForAll Globally" $ do
            toENF (ForAll (Globally (AtomicProposition "p")))
                `shouldBe` ENegation (EUntil ETruth (ENegation (EAtomicProposition "p")))


ts :: String -> IO TS
ts filename = do
    soda <- readFile filename
    let ts = TSParser.parse soda
    case ts of
        Just t  -> return t
        Nothing -> error "TS syntax error"

ctl :: String -> IO CTL
ctl formula = do
    let ts = CTLParser.parse formula
    case ts of
        Just c  -> return c
        Nothing -> error "CTL syntax error"

minimal :: TS
minimal = TS [State "s"] [] [] [State "s"] [] []