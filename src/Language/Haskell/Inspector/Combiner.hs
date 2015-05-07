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
  where inUsage = any (`inspection` code) . transitiveBindingsOf binding $ code

transitiveBindingsOf binding code =  expand (`bindingsOf` code) binding

bindingsOf binding code = do
          expr <- expressionsOf binding code
          maybeToList . expressionToBinding $ expr

expand :: Eq a => (a-> [a]) -> a -> [a]
expand f x = expand' [] f [x]

expand' _ _ [] = []
expand' ps f (x:xs) | elem x ps = expand' ps f xs
                    | otherwise = [x] ++ expand' (x:ps) f (xs ++ f x)

