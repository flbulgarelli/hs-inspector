module Language.Haskell.Inspector where

import  Language.Haskell.Parser
import  Language.Haskell.Syntax
import  Data.Maybe (fromMaybe, isJust)
import  Control.Monad (join)
import  Data.List (find)
import  Language.Haskell.Explorer

type Binding = String
type Code = String
type Inspection = Binding -> Code  -> Bool

-- | Inspection that tells whether a binding uses the composition operator '.'
-- in its definition
hasComposition :: Inspection
hasComposition = testAnyWithBindingExpr f
  where f (O (HsQVarOp (UnQual (HsSymbol ".")))) = True
        f _ = False

-- | Inspection that tells whether a binding uses guards
-- in its definition
hasGuards :: Inspection
hasGuards = testAnyWithBindingRhs f
  where f (HsGuardedRhss _) = True
        f _ = False


-- | Inspection that tells whether a binding uses a lambda expression
-- in its definition
hasLambda :: Inspection
hasLambda = testAnyWithBindingExpr f
  where f (E (HsLambda _ _ _)) = True
        f _ = False


-- | Inspection that tells whether a binding is direct recursive
hasDirectRecursion :: Inspection
hasDirectRecursion binding = hasUsage binding binding


-- | Inspection that tells whether a binding uses the the given target binding
-- in its definition
hasUsage :: String -> Inspection
hasUsage target = testAnyWithBindingExpr f
  where f (O (HsQVarOp name)) = isTarget name
        f (E (HsVar    name)) = isTarget name
        f _ = False

        isTarget (Qual  _ (HsSymbol t)) = t == target
        isTarget (Qual  _ (HsIdent  t)) = t == target
        isTarget (UnQual  (HsSymbol t)) = t == target
        isTarget (UnQual  (HsIdent  t)) = t == target
        isTarget _                           = False


-- | Inspection that tells whether a binding uses lists comprehensions
-- in its definition
hasComprehension :: Inspection
hasComprehension = testAnyWithBindingExpr f
  where f (E (HsListComp _ _)) = True
        f _ = False

-- | Inspection that tells whether a top level binding exists
hasBinding :: Inspection
hasBinding binding = isJust . findBindingRhs binding

isParseable :: Code -> Bool
isParseable = testWithCode (const True)

testAnyWithBindingExpr f = testAnyWithBindingRhs testExprs
  where testExprs rhs = exploreExprs f $ topExprs rhs

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
rhsForBinding (HsPatBind _ _ rhs localDecls) = concatRhs rhs localDecls
rhsForBinding (HsFunBind cases) = cases >>= \(HsMatch _ _ _ rhs localDecls) -> concatRhs rhs localDecls
rhsForBinding _ = []

concatRhs rhs l = [rhs] ++ concatMap rhsForBinding l

testWithCode f =  orFalse . withCode f

withCode :: ([HsDecl] -> a) -> Code -> Maybe a
withCode f code | ParseOk (HsModule _ _ _ _ decls) <- parseModule code = Just (f decls)
                | otherwise = Nothing

orFalse = fromMaybe False

