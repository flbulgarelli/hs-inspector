module Language.Haskell.Inspector where

import  Language.Haskell.Parser
import  Language.Haskell.Syntax
import  Language.Haskell.Names
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
hasComposition = isBindingEO f
  where f (O (HsQVarOp (UnQual (HsSymbol ".")))) = True
        f _ = False

-- | Inspection that tells whether a binding uses guards
-- in its definition
hasGuards :: Inspection
hasGuards = isBindingRhs f
  where f (HsGuardedRhss _) = True
        f _ = False

-- | Inspection that tells whether a binding uses ifs
-- in its definition
hasIf :: Inspection
hasIf = isBindingEO f
  where f (E (HsIf _ _ _)) = True
        f _ = False

-- | Inspection that tells whether a binding uses ifs or guards
-- in its definition
hasConditional :: Inspection
hasConditional target code = hasIf target code || hasGuards target code

-- | Inspection that tells whether a binding uses a lambda expression
-- in its definition
hasLambda :: Inspection
hasLambda = isBindingEO f
  where f (E (HsLambda _ _ _)) = True
        f _ = False


-- | Inspection that tells whether a binding is direct recursive
hasDirectRecursion :: Inspection
hasDirectRecursion binding = hasUsage binding binding

-- | Inspection that tells whether a binding uses the the given target binding
-- in its definition
hasUsage :: String -> Inspection
hasUsage target = isBindingEO f
  where f (O (HsQVarOp name)) = isTarget name
        f (E (HsVar    name)) = isTarget name
        f _ = False

        isTarget (Qual  _ n) = isName target n
        isTarget (UnQual  n) = isName target n
        isTarget _           = False

-- | Inspection that tells whether a binding uses lists comprehensions
-- in its definition
hasComprehension :: Inspection
hasComprehension = isBindingEO f
  where f (E (HsListComp _ _)) = True
        f _ = False

-- | Inspection that tells whether a top level binding exists
hasBinding :: Inspection
hasBinding binding = isJust . findBindingRhs binding

hasTypeDeclaration :: Inspection
hasTypeDeclaration binding = testWithCode (any f)
  where f (HsTypeDecl _ hsName _ _) = isName binding hsName
        f _                         = False

hasTypeSignature :: Inspection
hasTypeSignature binding = testWithCode (any f)
  where f (HsTypeSig _ [hsName] _)  = isName binding hsName
        f _                         = False

isParseable :: Code -> Bool
isParseable = testWithCode (const True)

negateInspection :: Inspection -> Inspection
negateInspection f code = not . f code


transitive :: Inspection -> Inspection
transitive = id

-- ===================================================


-- Language.Haskell.Expressions

isBindingEO :: (EO -> Bool) -> Inspection
isBindingEO f binding = any f . expressionsOf binding

expressionsOf :: Binding -> Code -> [EO]
expressionsOf binding code = do
  rhs <- rhssOf binding code
  top <- topExpressionsOf rhs
  unfoldExpression top

topExpressionsOf :: HsRhs -> [EO]
topExpressionsOf (HsUnGuardedRhs e) = [E e]
topExpressionsOf (HsGuardedRhss rhss) = rhss >>= \(HsGuardedRhs _ es1 es2) -> [E es1, E es2]

unfoldExpression :: EO -> [EO]
unfoldExpression expr = expr : concatMap unfoldExpression (subExpressions expr)

subExpressions :: EO -> [EO]
subExpressions (E (HsInfixApp a b c)) = [E a, O b, E c]
subExpressions (E (HsApp a b))        = [E a, E b]
subExpressions (E (HsNegApp a))       = [E a]
subExpressions (E (HsLambda _ _ a))   = [E a]
subExpressions (E (HsList as))        = map (E) as
subExpressions (E (HsListComp a _))   = [E a] --TODO
subExpressions (E (HsTuple as))       = map (E) as
subExpressions (E (HsParen a))        = [E a]
subExpressions (E (HsIf a b c))       = [E a, E b, E c]
subExpressions _ = []


rhssOf :: Binding -> Code -> [HsRhs]
rhssOf binding = concatMap rhsForBinding .filter (isBinding binding). orNil . declsOf

declsOf :: Code -> Maybe [HsDecl]
declsOf code
  | ParseOk (HsModule _ _ _ _ decls) <- parseModule code = Just decls
  | otherwise = Nothing

isBinding :: Binding -> HsDecl -> Bool
isBinding binding (HsPatBind _ (HsPVar n)  _ _) = isName binding n
isBinding binding (HsFunBind cases)  = isNameInCases cases
      where isNameInCases = any (isName binding) . map (\(HsMatch _ n _ _ _) -> n)

-- legacy

bindingInMatch (HsMatch _ n _ _ _) = nameOf n


isBindingRhs f = testWithBindingRhs (any f)

testWithBindingRhs :: ([HsRhs] -> Bool) -> Binding -> Code -> Bool
testWithBindingRhs f binding  = orFalse . withBindingRhs f binding

withBindingRhs :: ([HsRhs] -> a) -> Binding -> Code -> Maybe a
withBindingRhs f binding = fmap f . findBindingRhs binding

findBindingRhs binding = fmap rhsForBinding . join . withCode (find isBinding)
  where isBinding (HsPatBind _ (HsPVar n)  _ _) = nameOf n == binding
        isBinding (HsFunBind cases)  = any ((== binding).bindingInMatch) cases
        isBinding _ = False

rhsForBinding :: HsDecl -> [HsRhs]
rhsForBinding (HsPatBind _ _ rhs localDecls) = concatRhs rhs localDecls
rhsForBinding (HsFunBind cases) = cases >>= \(HsMatch _ _ _ rhs localDecls) -> concatRhs rhs localDecls
rhsForBinding _ = []

concatRhs rhs l = [rhs] ++ concatMap rhsForBinding l

testWithCode f =  orFalse . withCode f

withCode :: ([HsDecl] -> a) -> Code -> Maybe a
withCode f code | ParseOk (HsModule _ _ _ _ decls) <- parseModule code = Just (f decls)
                | otherwise = Nothing

-- Utils

orFalse = fromMaybe False
orNil = fromMaybe []

