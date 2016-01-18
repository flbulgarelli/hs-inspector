{-# LANGUAGE OverloadedStrings #-}

module ExplorerSpec (spec) where

import           Test.Hspec
import           Language.Haskell.Explorer
import           Language.Haskell.Mu

spec :: Spec
spec = do
  describe "bindingsOf" $ do
    it "answers bindings for binding" $ do
      let code = "f x =  (:[]) . m x y . g h 2"
      (bindingsOf "f" code) `shouldBe` [".","m","x","y", "g","h"]

  describe "transitiveBindingsOf" $ do
    it "answers transitive bindings for binding" $ do
      let code = "f x = m x\n\
                 \m 0 = p 0\n\
                 \p x = g x"
      (transitiveBindingsOf "f" code) `shouldBe` ["f","m","x","p","g"]

  describe "astOf" $ do
    it "converts comprehensions into binds and returns" $ do
      let (MuProgram [MuConstant _ (MuUnGuardedRhs comprehension) _]) = "m = [x | x <- xs ]"
      let (MuProgram [MuConstant _ (MuUnGuardedRhs application)   _]) = "m = xs >>= (\\x -> return x)"

      comprehension `shouldBe` application

    it "converts comprehensions into binds and returns 2" $ do
      let (MuProgram [MuConstant _ (MuUnGuardedRhs comprehension) _]) = "m = [y | y <- xs ]"
      let (MuProgram [MuConstant _ (MuUnGuardedRhs application)   _]) = "m = xs >>= (\\y -> return y)"

      comprehension `shouldBe` application

    it "converts comprehensions into binds and returns 3" $ do
      let (MuProgram [MuConstant _ (MuUnGuardedRhs comprehension) _]) = "m = [f y | y <- xs ]"
      let (MuProgram [MuConstant _ (MuUnGuardedRhs application)   _]) = "m = xs >>= (\\y -> return (f y))"

      comprehension `shouldBe` application

    it "converts comprehensions into binds and returns 4" $ do
      let (MuProgram [MuConstant _ (MuUnGuardedRhs comprehension) _]) = "m = [f y | y <- ys ]"
      let (MuProgram [MuConstant _ (MuUnGuardedRhs application)   _]) = "m = ys >>= (\\y -> return (f y))"

      comprehension `shouldBe` application

    it "converts comprehensions into binds and returns 5" $ do
      let (MuProgram [MuConstant _ (MuUnGuardedRhs comprehension) _]) = "m = [f y z | y <- ys, z <- zs ]"
      let (MuProgram [MuConstant _ (MuUnGuardedRhs application)   _]) = "m = ys >>= \\y -> zs >>= \\z -> return (f y z)"

      comprehension `shouldBe` application

    it "converts comprehensions into binds and returns 6" $ do
      let (MuProgram [MuConstant _ (MuUnGuardedRhs comprehension) _]) = "m = [f y z | y <- ks, z <- zs ]"
      let (MuProgram [MuConstant _ (MuUnGuardedRhs application)   _]) = "m = ks >>= \\y -> zs >>= \\z -> return (f y z)"

      comprehension `shouldBe` application

    it "converts comprehensions into binds and returns 7" $ do
      let (MuProgram [MuConstant _ (MuUnGuardedRhs comprehension) _]) = "m = [f y z | k <- ks, z <- zs ]"
      let (MuProgram [MuConstant _ (MuUnGuardedRhs application)   _]) = "m = ks >>= \\k -> zs >>= \\z -> return (f y z)"

      comprehension `shouldBe` application

    it "converts comprehensions into binds and returns 8" $ do
      let (MuProgram [MuConstant _ (MuUnGuardedRhs comprehension) _]) = "m = [g k (f z) | k <- ks, z <- zs ]"
      let (MuProgram [MuConstant _ (MuUnGuardedRhs application)   _]) = "m = ks >>= \\k -> zs >>= \\z -> return (g k (f z))"

      comprehension `shouldBe` application

    it "converts comprehensions into binds and returns 9" $ do
      let (MuProgram [MuConstant _ (MuUnGuardedRhs comprehension) _]) = "m = [g k (f o) | k <- ks, o <- os ]"
      let (MuProgram [MuConstant _ (MuUnGuardedRhs application)   _]) = "m = ks >>= \\k -> os >>= \\o -> return (g k (f o))"

      comprehension `shouldBe` application

    it "converts comprehensions into binds and returns 10" $ do
      let (MuProgram [MuConstant _ (MuUnGuardedRhs comprehension) _]) = "m = [g k (f o) | k <- ks, o <- os, j <- js]"
      let (MuProgram [MuConstant _ (MuUnGuardedRhs application)   _]) = "m = ks >>= \\k -> os >>= \\o -> js >>= \\j -> return (g k (f o))"

      comprehension `shouldBe` application


