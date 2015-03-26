module Expectations where

newtype Expectations = Expectations [Expectation]

type Binding = String
type Code = String
type ExpectationFunction = Binding  -> Code  -> Bool 

data Expectation = Expectation Binding ExpectationType

data ExpectationType = HasLambda | HasGuards | HasComposition 

compile (Expectations exs) = map compileExpectation  exs

compileExpectation (Expectation binding eType) = (functionForType eType) binding

functionForType :: ExpectationType -> ExpectationFunction
functionForType HasLambda = hasLambda
functionForType HasGuards = hasGuards
functionForType HasComposition = hasComposition

runExpectations code = run.zipApply
   where run = map (\(e, f) -> (e, f code))  
         zipApply f xs = zip xs (f xs) 
          
