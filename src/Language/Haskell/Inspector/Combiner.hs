module Language.Haskell.Inspector.Combiner (
  detect,
  negative,
  transitive) where

import Language.Haskell.Inspector
import Language.Haskell.Explorer

detect :: Inspection -> AST -> [Binding]
detect inspection code = filter (`inspection` code) $ parseBindings code

negative :: Inspection -> Inspection
negative f code = not . f code

transitive :: Inspection -> Inspection
transitive inspection binding code = inspection binding code || inUsage
  where inUsage = any (`inspection` code) . transitiveBindingsOf binding $ code
