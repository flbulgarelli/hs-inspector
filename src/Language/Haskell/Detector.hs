module Language.Haskell.Detector (detect) where

import Language.Haskell.Inspector
import Language.Haskell.Syntax
import Language.Haskell.Names
import Language.Haskell.Explorer

detect :: Inspection -> Code -> [Binding]
detect inspection code = filter (`inspection` code) $ parseBindings code

-- private

parseBindings :: Code -> [Binding]
parseBindings = concatMap bindings . parseDecls
  where
  bindings (HsTypeSig _ [b] _) = [nameOf b]
  bindings (HsTypeDecl _ b _ _) = [nameOf b]
  bindings (HsPatBind _ (HsPVar n) _ _) = [nameOf n]
  bindings (HsFunBind cases)  = map bindingInMatch cases
  bindings _                  = []

