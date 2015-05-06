module Language.Haskell.Inspector.Combiner (
  detect,
  negative,
  transitive) where

import Language.Haskell.Inspector
import Language.Haskell.Explorer
import Data.Maybe (maybeToList)

detect :: Inspection -> Code -> [Binding]
detect inspection code = filter (`inspection` code) $ parseBindings code

negative :: Inspection -> Inspection
negative f code = not . f code

transitive :: Inspection -> Inspection
transitive inspection binding code = inspection binding code || inUsage
  where inUsage = any (flip (transitive inspection) code) $ do
                      expr <- expressionsOf binding code
                      maybeToList . expressionToBinding $ expr
