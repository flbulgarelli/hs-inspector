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

  describe "hasComposition" $ do
    describe "when constant assignment" $ do
      it "is True when composition is present on top level" $ do
        hasComposition "x" "x = y . z" `shouldBe` True

      it "is True when composition is present inside lambda" $ do
        hasComposition "x" "x = \\m -> y . z" `shouldBe` True

      it "is True when composition is present inside application" $ do
        hasComposition "x" "x = f (g.h) x" `shouldBe` True

      it "is False when composition not present" $ do
        hasComposition "x" "x = 1" `shouldBe` False

    describe "when unguarded function" $ do
      it "is True when composition is present on top level" $ do
        hasComposition "f" "f x = (g . f) x" `shouldBe` True

      it "is True when composition is present within if" $ do
        hasComposition "f" "f x = if True then (g . f) x else 5" `shouldBe` True

      it "is True when composition is present within list" $ do
        hasComposition "f" "f x = [(g.h x), m]" `shouldBe` True

      it "is True when composition is present within comprehension" $ do
        hasComposition "f" "f x = [ (g.h x) m | m <- [1..20]]" `shouldBe` True

      it "is True when composition is present within where" $ do
        hasComposition "f" "f x = m\n\
                           \      where m = (f.g) " `shouldBe` True

      it "is False when composition not present" $ do
        hasComposition "f" "f x = g x" `shouldBe` False

    describe "when guarded function " $ do
      it "is True when composition is present on top level" $ do
        hasComposition "f" "f x | c x = g . f $ x\n\
                           \    | otherwise = 4" `shouldBe` True

      it "is True when composition is present on guard" $ do
        hasComposition "f" "f x | (c . g) x = g x\n\
                           \    | otherwise = 4" `shouldBe` True

      it "is False when composition not present" $ do
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



