module InspectorSpec (spec) where

import           Test.Hspec
import           Inspector

spec :: Spec
spec = do
  describe "parsing analyzer" $ do
    it "detects parseable code" $ do
      isParseable "x = 3" `shouldBe` True

    it "detects unparseable code" $ do
      isParseable "3" `shouldBe` False

  describe "hasBinding" $ do
    describe "with constants" $ do
      it "is True when binding exists" $ do
        hasBinding "x"  "x = 1" `shouldBe` True

      it "is False when binding doesnt exists" $ do
        hasBinding "y"  "x = 1" `shouldBe` False

      it "is False when code is unparseable" $ do
        hasBinding "y"  "7" `shouldBe` False

    describe "with functions" $ do
      it "is True when binding exists" $ do
        hasBinding "x"  "x m = 1" `shouldBe` True

      it "is False when binding doesnt exists" $ do
        hasBinding "y"  "x m = 1" `shouldBe` False

  describe "composition analyzer" $ do
    describe "detects composition, constant assignment when" $ do
      it "is present" $ do
        hasComposition "x" "x = y . z" `shouldBe` True
      it "is not present when" $ do
        hasComposition "x" "x = 1" `shouldBe` False

    describe "detects composition, unguarded function when" $ do
      it "is present" $ do
        hasComposition "f" "f x = (g . f) x" `shouldBe` True
      it "is not present when" $ do
        hasComposition "f" "f x = g x" `shouldBe` False

    describe "detects composition, guarded function when" $ do
      it "is present in result" $ do
        hasComposition "f" "f x | c x = g . f $ x\n\
                           \    | otherwise = 4" `shouldBe` True
      it "is present in guard" $ do
        hasComposition "f" "f x | (c . g) x = g x\n\
                           \    | otherwise = 4" `shouldBe` True
      it "is not present" $ do
        hasComposition "f" "f x | c x = f x\n\
                           \    | otherwise = 4" `shouldBe` False

  describe "guards analyzer" $ do
    describe "detects guards when" $ do
      it "is present" $ do
        hasGuards "f" "f x | c x = 2\n\
                      \    | otherwise = 4"  `shouldBe` True

      it "is present" $ do
        hasGuards "f" "f x = c x = 2"  `shouldBe` False

  describe "lambda analyzer" $ do
    describe "detects lambdas when" $ do
      it "is present" $ do
        hasLambda "f" "f x = \\y -> 4" `shouldBe` True

      it "is present" $ do
        hasLambda "f" "f x = 4" `shouldBe` False



