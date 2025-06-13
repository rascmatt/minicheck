module Model.PatternSpec (spec) where

import Test.Hspec
import Model.Pattern

spec :: Spec
spec = do
  describe "Empty pattern" $ do
    it "matches empty string" $
      matchesPattern (makePattern "") ""
    it "does not match whitespace" $
      not $ matchesPattern (makePattern "") " "
  describe "Pattern without wildcards" $ do
      it "matches a simple string" $
        matchesPattern (makePattern "abc_123") "abc_123"
      it "does not match larger string" $
        not $ matchesPattern (makePattern "xyz") "xyzw"
      it "does not match shorter string" $
        not $ matchesPattern (makePattern "xyz0") "xyz"
  describe "with a * wildcard" $ do
    it "at the end might match no characters" $
      matchesPattern (makePattern "abc*") "abc"
    it "matches any substring" $
      matchesPattern (makePattern "fo*") "foobar_987"
  describe "with a # wildcard" $ do
    it "does not match dot character" $
      not $ matchesPattern (makePattern "foo.#xyz") "foo.bar.xyz"
    it "matches everything except dot character" $
      matchesPattern (makePattern "foo.#.xyz") "foo.bar.xyz"
    it "at the end might match no characters" $
      matchesPattern (makePattern "abc#") "abc"
