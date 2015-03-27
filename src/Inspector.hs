module Inspector where

import  Language.Haskell.Parser
import  Language.Haskell.Syntax
import  Data.Maybe (fromMaybe, isJust)
import  Control.Monad (join)
import  Data.List (find)

type Binding = String
type Code = String
type Inspection = Binding -> Code  -> Bool

hasComposition :: Inspection
hasComposition _ _ = False

hasRecursion :: Inspection
hasRecursion _ _ = False

hasGuards :: Inspection
hasGuards binding code = testWithBindingRhs binding code (any f)
  where f (HsGuardedRhss _) = True
        f _ = False

hasLambda :: Inspection
hasLambda _ _ = False

hasBinding :: Inspection
hasBinding binding = isJust . findBindingRhs binding

isParseable :: Code -> Bool
isParseable code  = testWithCode code (const True)

testWithBindingRhs :: Binding -> Code -> ([HsRhs] -> Bool) -> Bool
testWithBindingRhs binding code = orFalse . withBindingRhs binding code

withBindingRhs :: Binding -> Code -> ([HsRhs] -> a) -> Maybe a
withBindingRhs binding code f = fmap f $ findBindingRhs binding code

findBindingRhs binding code = fmap rhsForBinding $ join $ withCode code (find isBinding)
  where isBinding (HsPatBind _ (HsPVar (HsIdent name))  _ _) = name == binding
        isBinding (HsFunBind cases)  = any isBindingInMatch cases
        isBinding _ = False

        isBindingInMatch (HsMatch _ (HsIdent name) _ _ _ ) = name == binding
        isBindingInMatch _ = False

rhsForBinding :: HsDecl -> [HsRhs]
rhsForBinding (HsPatBind _ _ rhs _) = [rhs]
rhsForBinding (HsFunBind cases) = map (\(HsMatch _ _ _ rhs _) -> rhs) cases
rhsForBinding _ = []

testWithCode code =  orFalse . withCode code

withCode :: Code -> ([HsDecl] -> a) -> Maybe a
withCode code f | ParseOk (HsModule _ _ _ _ decls) <- parseModule code = Just (f decls)
                | otherwise = Nothing


orFalse = fromMaybe False
