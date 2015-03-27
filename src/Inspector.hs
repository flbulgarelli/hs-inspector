module Inspector where

import  Language.Haskell.Parser
import  Language.Haskell.Syntax

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

hasBinding :: Inspection
hasBinding binding code = testWithCode code (any isBinding)
  where isBinding (HsPatBind _ (HsPVar (HsIdent name))  _ _) = name == binding
        isBinding (HsFunBind cases)  = any isBindingInMatch cases
        isBinding _ = False

        isBindingInMatch (HsMatch _ (HsIdent name) _ _ _ ) = name == binding
        isBindingInMatch _ = False

isParseable :: Code -> Bool
isParseable code  = testWithCode code (const True)

testWithCode code f | ParseOk (HsModule _ _ _ _ decls) <- parseModule code = f decls
                    | otherwise = False

