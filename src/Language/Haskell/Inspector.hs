module Language.Haskell.Inspector (
  hasComposition,
  hasGuards,
  hasIf,
  hasConditional,
  hasLambda,
  hasDirectRecursion,
  hasUsage,
  hasComprehension,
  hasBinding,
  hasTypeDeclaration,
  hasTypeSignature,
  hasExpression,
  hasDecl,
  hasRhs,
  isParseable,
  transitive,
  Inspection,
  GlobalInspection
  ) where

import  Language.Haskell.Parser
import  Language.Haskell.Syntax
import  Language.Haskell.Names
import  Language.Haskell.Explorer

import  Data.Maybe (fromMaybe, isJust)
import  Control.Monad (join)
import  Data.List (find)

type Inspection = Binding -> Code  -> Bool
type GlobalInspection = Code  -> Bool

-- | Inspection that tells whether a binding uses the composition operator '.'
-- in its definition
hasComposition :: Inspection
hasComposition = hasExpression f
  where f (O (HsQVarOp (UnQual (HsSymbol ".")))) = True
        f _ = False

-- | Inspection that tells whether a binding uses guards
-- in its definition
hasGuards :: Inspection
hasGuards = hasRhs f
  where f (HsGuardedRhss _) = True
        f _ = False

-- | Inspection that tells whether a binding uses ifs
-- in its definition
hasIf :: Inspection
hasIf = hasExpression f
  where f (E (HsIf _ _ _)) = True
        f _ = False

-- | Inspection that tells whether a binding uses ifs or guards
-- in its definition
hasConditional :: Inspection
hasConditional target code = hasIf target code || hasGuards target code

-- | Inspection that tells whether a binding uses a lambda expression
-- in its definition
hasLambda :: Inspection
hasLambda = hasExpression f
  where f (E (HsLambda _ _ _)) = True
        f _ = False


-- | Inspection that tells whether a binding is direct recursive
hasDirectRecursion :: Inspection
hasDirectRecursion binding = hasUsage binding binding

-- | Inspection that tells whether a binding uses the the given target binding
-- in its definition
hasUsage :: String -> Inspection
hasUsage target = hasExpression f
  where f (O (HsQVarOp name)) = isTarget name
        f (E (HsVar    name)) = isTarget name
        f _ = False

        isTarget (Qual  _ n) = isName target n
        isTarget (UnQual  n) = isName target n
        isTarget _           = False

-- | Inspection that tells whether a binding uses lists comprehensions
-- in its definition
hasComprehension :: Inspection
hasComprehension = hasExpression f
  where f (E (HsListComp _ _)) = True
        f _ = False

-- | Inspection that tells whether a top level binding exists
hasBinding :: Inspection
hasBinding binding = not.null.rhssOf binding

hasTypeDeclaration :: Inspection
hasTypeDeclaration binding = hasDecl f
  where f (HsTypeDecl _ hsName _ _) = isName binding hsName
        f _                         = False

hasTypeSignature :: Inspection
hasTypeSignature binding = hasDecl f
  where f (HsTypeSig _ [hsName] _)  = isName binding hsName
        f _                         = False

negateInspection :: Inspection -> Inspection
negateInspection f code = not . f code

transitive :: Inspection -> Inspection
transitive = id

hasExpression :: (EO -> Bool) -> Inspection
hasExpression f binding = has f (expressionsOf binding)

hasRhs :: (HsRhs -> Bool)-> Inspection
hasRhs f binding = has f (rhssOf binding)

isParseable :: GlobalInspection
isParseable = not.null.parseDecls

hasDecl :: (HsDecl -> Bool) -> GlobalInspection
hasDecl f = has f parseDecls

has f g = any f . g

-- ===================================================


isBindingRhs f = testWithBindingRhs (any f)

testWithBindingRhs :: ([HsRhs] -> Bool) -> Binding -> Code -> Bool
testWithBindingRhs f binding  = f . rhssOf binding

testWithCode f =  f . parseDecls

-- Utils

orFalse = fromMaybe False


