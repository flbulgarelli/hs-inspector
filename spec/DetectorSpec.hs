module DetectorSpec (spec) where

import           Test.Hspec
import           Language.Haskell.Inspector
import           Language.Haskell.Detector

spec :: Spec
spec = do
  describe "detect" $ do
    it "can detect inspections" $ do
      let code = "x = if True then True else False\n\
                 \y = 2\n\
                 \z = if True then True else False"
      detect hasIf code `shouldBe` ["x", "z"]
