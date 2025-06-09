module Extension.ParserSpec (spec) where

import Test.Hspec

import Extension.Minimm.Parser
import Extension.Minimm.Ast
import Parser.Base (topLevel)

spec :: Spec
spec = do
    describe "Parse Mini--" $ do
        it "parse expression" $ do
            topLevel mBoolExpr "true" `shouldBe` Just (Lit True)
            topLevel mBoolExpr "false" `shouldBe` Just (Lit False)
            topLevel mBoolExpr "!true" `shouldBe` Just (Not (Lit True))
            topLevel mBoolExpr "a" `shouldBe` Just (Var "a")
            topLevel mBoolExpr "a & b" `shouldBe` Just (BinOp And (Var "a") (Var "b"))
            topLevel mBoolExpr "a | b" `shouldBe` Just (BinOp Or (Var "a") (Var "b"))
            topLevel mBoolExpr "a ^ b" `shouldBe` Just (BinOp Xor (Var "a") (Var "b"))
            topLevel mBoolExpr "a => b" `shouldBe` Just (BinOp Impl (Var "a") (Var "b"))
            topLevel mBoolExpr "a = b" `shouldBe` Just (BinOp Equiv (Var "a") (Var "b"))
            topLevel mBoolExpr "(a)" `shouldBe` Just (Var "a")
            topLevel mBoolExpr "((a))" `shouldBe` Just (Var "a")
            topLevel mBoolExpr "(a & b) | c" `shouldBe` Just (BinOp Or (BinOp And (Var "a") (Var "b")) (Var "c"))
        it "parse if statement" $ do
            topLevel mStatements 
                "if (a) { b = true; }" `shouldBe` Just [If (Var "a") [Assign "b" (Lit True)]]
        it "parse if else statement" $ do
            topLevel mStatements 
                "if (a) { b = true; } else { b = false; }" 
                    `shouldBe` Just [IfElse (Var "a") [Assign "b" (Lit True)] [Assign "b" (Lit False)]]
        it "parse assignment" $ do
            topLevel mStatements 
                "a = b = c;" 
                    `shouldBe` Just [Assign "a" (BinOp Equiv (Var "b") (Var "c"))]
        it "parse print statement" $ do
            topLevel mStatements "print_bool(a);" `shouldBe` Just [Print (Var "a")]
        it "parse read statement" $ do
            topLevel mStatements "a = read_bool();" `shouldBe` Just [Read "a"]
        it "parse program" $ do
            parse "procedure main(a){ return a; }" `shouldBe` 
                Just (Prog ["a"] [Return (Var "a")])
            parse "procedure main(a){ }" `shouldBe` Nothing
            parse "procedure main(a){ a = true; return a; }" 
                `shouldBe` Just (Prog ["a"] [Assign "a" (Lit True), Return (Var "a")])
            parse "procedure main(a){ a = true; }" `shouldBe` Nothing
            parse "procedure main(a){ return true; }" `shouldBe` Just (Prog ["a"] [Return (Lit True)])
            parse "procedure main(){ return true; }" `shouldBe` Nothing
