module Parser.CTLSpec (spec) where

import Test.Hspec

import Lib.Parser.Base (topLevel)
import Lib.Parser.CTL
import Lib.Model.CTL
import Lib.Model.TS (Proposition(..))

spec :: Spec
spec = do
    describe "Parser.CTL" $ do
        it "parses single atomic proposition" $ do
            topLevel ctlFormula "atom" `shouldBe` Just (AtomicProposition "atom")
            topLevel ctlFormula "(atom)" `shouldBe` Just (AtomicProposition "atom")
        it "ignores whitespace" $ do
            topLevel ctlFormula "   ws     " `shouldBe` Just (AtomicProposition "ws")
        it "ignores extra parenthesis" $ do
            topLevel ctlFormula "(((((a && b)))))" `shouldBe` Just (BinaryOperation Conjunction (AtomicProposition "a") (AtomicProposition "b"))
        it "parses atomic proposition negation without parentheses" $ do
            topLevel ctlFormula "A !red U !white" `shouldBe` Just (ForAll (Until (Negation (AtomicProposition "red")) (Negation (AtomicProposition "white"))))
        it "parses equivalence" $ do
            topLevel ctlFormula "abc == xyz == p1 == bool2" `shouldBe` Just
                (BinaryOperation Equivalence (AtomicProposition "abc")
                    (BinaryOperation Equivalence (AtomicProposition "xyz")
                        (BinaryOperation Equivalence (AtomicProposition "p1") (AtomicProposition "bool2"))))
        it "parses left-nested implication" $ do
            topLevel ctlFormula "(x => y) => z" `shouldBe` Just
                (BinaryOperation Implication
                    (BinaryOperation Implication (AtomicProposition "x") (AtomicProposition "y"))
                    (AtomicProposition "z"))
        it "parses right-nested implication" $ do
            topLevel ctlFormula "x => (y => z)" `shouldBe` Just
                (BinaryOperation Implication
                    (AtomicProposition "x")
                    (BinaryOperation Implication (AtomicProposition "y") (AtomicProposition "z")))
        it "disallows nested implication without parenthesis" $ do -- Because implication is not associative
            topLevel ctlFormula "x => y => z" `shouldBe` Nothing
        it "parses Exists-Until" $ do
            topLevel ctlFormula "E (x && abc) U p" `shouldBe` Just
                (Exists $ Until
                    (BinaryOperation Conjunction (AtomicProposition "x") (AtomicProposition "abc"))
                    (AtomicProposition "p"))
        it "parses xor of Global, Until and Next" $ do
            topLevel ctlFormula "(AG is_blue) != (E(is_yellow || is_blue)U is_red) != (EX is_orange)" `shouldBe` Just
                (BinaryOperation ExclusiveDisjunction
                    (ForAll $ Globally $ AtomicProposition "is_blue")
                    (BinaryOperation ExclusiveDisjunction
                        (Exists $ Until (BinaryOperation Disjunction (AtomicProposition "is_yellow") (AtomicProposition "is_blue")) (AtomicProposition "is_red"))
                        (Exists $ Next $ AtomicProposition "is_orange")))
