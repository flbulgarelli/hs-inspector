module Inspector where

type Binding = String
type Code = String
type Inspection = Binding -> Code  -> Bool

hasComposition :: Inspection
hasComposition _ _ = False

hasRecursion :: Inspection
hasRecursion _ _ = False

hasGuards :: Inspection
hasGuards _ _ = False

hasLambda :: Inspection
hasLambda _ _ = False


isParseable :: Code -> Bool
isParseable _ = False