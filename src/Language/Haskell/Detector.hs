module Language.Haskell.Detector (detect) where

import Language.Haskell.Inspector
import Language.Haskell.Syntax
import Language.Haskell.Names
import Language.Haskell.Explorer

detect :: Inspection -> Code -> [Binding]
detect inspection code = filter (`inspection` code) $ parseBindings code

-- private

parseBindings :: Code -> [Binding]
parseBindings = map declName . parseDecls

