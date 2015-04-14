module Detector (detect) where

import Language.Haskell.Inspector

bindingsOf :: Code -> [String]
bindingsOf = orNil . withCode (concatMap bindings)
  where
  bindings (HsTypeSig _ [b] _) = [nameOf b]
  bindings (HsTypeDecl _ b _ _) = [nameOf b]
  bindings (HsPatBind _ (HsPVar n) _ _) = [nameOf n]
  bindings (HsFunBind cases)  = map bindingInMatch cases
  bindings _                  = []

detect :: Inspection -> Code -> [Binding]
detect inspection code = filter (`inspection` code) $ bindingsOf code
