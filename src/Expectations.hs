module Expectations where

import              Inspector

newtype Expectations = Expectations [Expectation]

data Expectation = Expectation Binding ExpectationType

data ExpectationType = HasLambda | HasGuards | HasComposition | HasRecursion

compile (Expectations exs) = map compileExpectation  exs

compileExpectation (Expectation binding eType) = (inspectionFor eType) binding

inspectionFor :: ExpectationType -> Inspection
inspectionFor HasLambda = hasLambda
inspectionFor HasGuards = hasGuards
inspectionFor HasComposition = hasComposition
inspectionFor HasRecursion = hasRecursion

runExpectations code = run.zipApply compileExpectation
   where run = map (\(e, f) -> (e, f code))

zipApply :: (a -> b) -> [a] ->[(a, b)]
zipApply f xs = zip xs (map f xs)

