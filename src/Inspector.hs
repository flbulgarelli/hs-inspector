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
hasComposition = testAnyWithBindingRhs f
  where f rhs = exploreExprs g $ topExprs rhs
        g (O (HsQVarOp (UnQual (HsSymbol ".")))) = True
        g _ = False

hasRecursion :: Inspection
hasRecursion _ _ = False

hasGuards :: Inspection
hasGuards = testAnyWithBindingRhs f
  where f (HsGuardedRhss _) = True
        f _ = False

hasLambda :: Inspection
hasLambda _ _ = False

hasBinding :: Inspection
hasBinding binding = isJust . findBindingRhs binding

isParseable :: Code -> Bool
isParseable = testWithCode (const True)

testAnyWithBindingRhs f = testWithBindingRhs (any f)

testWithBindingRhs :: ([HsRhs] -> Bool) -> Binding -> Code -> Bool
testWithBindingRhs f binding  = orFalse . withBindingRhs f binding

withBindingRhs :: ([HsRhs] -> a) -> Binding -> Code -> Maybe a
withBindingRhs f binding = fmap f . findBindingRhs binding

findBindingRhs binding = fmap rhsForBinding . join . withCode (find isBinding)
  where isBinding (HsPatBind _ (HsPVar (HsIdent name))  _ _) = name == binding
        isBinding (HsFunBind cases)  = any isBindingInMatch cases
        isBinding _ = False

        isBindingInMatch (HsMatch _ (HsIdent name) _ _ _ ) = name == binding
        isBindingInMatch _ = False

rhsForBinding :: HsDecl -> [HsRhs]
rhsForBinding (HsPatBind _ _ rhs _) = [rhs]
rhsForBinding (HsFunBind cases) = map (\(HsMatch _ _ _ rhs _) -> rhs) cases
rhsForBinding _ = []

testWithCode f =  orFalse . withCode f

withCode :: ([HsDecl] -> a) -> Code -> Maybe a
withCode f code | ParseOk (HsModule _ _ _ _ decls) <- parseModule code = Just (f decls)
                | otherwise = Nothing

orFalse = fromMaybe False

data EO = E HsExp | O HsQOp

exploreExprs f exprs = exploreEO f $ map (E) exprs

exploreEO f es = any f es || any (exploreEO' f) es

exploreEO' :: (EO -> Bool) -> EO -> Bool
exploreEO' f (E (HsInfixApp a b c)) = exploreEO f [E a, O b, E c]
exploreEO' f (E (HsApp a b))  = exploreEO f [E a, E b]
exploreEO' f (E (HsNegApp a))  = exploreEO f [E a]
exploreEO' f (E (HsLambda _ _ a)) = exploreEO f [E a]
exploreEO' f (E (HsList a)) = exploreEO f $ map (E) a
exploreEO' _ _ = False

topExprs (HsUnGuardedRhs e) = [e]
topExprs (HsGuardedRhss rhss) = concatMap (\(HsGuardedRhs _ es1 es2) -> [es1, es2]) rhss
