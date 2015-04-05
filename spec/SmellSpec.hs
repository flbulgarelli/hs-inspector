module SmellSpec (spec) where

import           Test.Hspec
import           Language.Haskell.Inspector.Smell

spec :: Spec
spec = do
  describe "hasRedundantIf" $ do
    it "is True when if present and both branches are boolean literals" $ do
      hasRedundantIf "x" "x = if m then True else False" `shouldBe` True
      hasRedundantIf "x" "x = if m then False else True" `shouldBe` True

    it "is False when there is no if" $ do
      hasRedundantIf "x" "x = False" `shouldBe` False

  describe "hasRedundantBooleanComparison" $ do
    it "is True when comparing a literal in an if" $ do
      hasRedundantBooleanComparison "x" "x = if m == True then 1 else 2" `shouldBe` True

    it "is True when comparing a literal in an unguarded expression" $ do
      hasRedundantBooleanComparison "f" "f x = x == True" `shouldBe` True

    it "is False on normal comparison" $ do
      hasRedundantBooleanComparison "f" "f x = x == 2" `shouldBe` False

    it "is False when no comparison" $ do
      hasRedundantBooleanComparison "f" "f x = True" `shouldBe` False