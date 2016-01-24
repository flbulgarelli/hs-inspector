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
  hasAnonymousVariable,
  hasExpression,
  hasDecl,
  hasRhs,
  Inspection,
  GlobalInspection
  ) where

import  Language.Haskell.Names (isName)
import  Language.Haskell.Explorer
import  Language.Haskell.Mu

type Inspection = Binding -> AST  -> Bool
type GlobalInspection = AST  -> Bool

-- | Inspection that tells whether a binding uses the composition operator '.'
-- in its definition
hasComposition :: Inspection
hasComposition = hasExpression f
  where f (Variable ".") = True
        f _ = False

-- | Inspection that tells whether a binding uses guards
-- in its definition
hasGuards :: Inspection
hasGuards = hasRhs f
  where f (GuardedRhss _) = True
        f _ = False

-- | Inspection that tells whether a binding uses ifs
-- in its definition
hasIf :: Inspection
hasIf = hasExpression f
  where f (If _ _ _) = True
        f _ = False

-- | Inspection that tells whether a binding uses ifs or guards
-- in its definition
hasConditional :: Inspection
hasConditional target code = hasIf target code || hasGuards target code

-- | Inspection that tells whether a binding uses a lambda expression
-- in its definition
hasLambda :: Inspection
hasLambda = hasExpression f
  where f (Lambda _ _) = True
        f _ = False


-- | Inspection that tells whether a binding is direct recursive
hasDirectRecursion :: Inspection
hasDirectRecursion binding = hasUsage binding binding

-- | Inspection that tells whether a binding uses the the given target binding
-- in its definition
hasUsage :: String -> Inspection
hasUsage target = hasExpression f
  where f expr | (Just n) <- expressionToBinding expr = n == target
               | otherwise = False

-- | Inspection that tells whether a binding uses lists comprehensions
-- in its definition
hasComprehension :: Inspection
hasComprehension = hasExpression f
  where f (ListComprehension _ _) = True
        f _ = False

-- | Inspection that tells whether a top level binding exists
hasBinding :: Inspection
hasBinding binding = not.null.rhssOf binding

hasTypeDeclaration :: Inspection
hasTypeDeclaration binding = hasDecl f
  where f (TypeAlias hsName) = isName binding hsName
        f _                   = False

hasTypeSignature :: Inspection
hasTypeSignature binding = hasDecl f
  where f (TypeSignature hsName)  = isName binding hsName
        f _                       = False

hasAnonymousVariable :: Inspection
hasAnonymousVariable binding = any f . declsOf binding
  where f (FunctionDeclaration _ hsMatches)    = any (any (== WildcardPattern) . p) hsMatches
        f _                        = False
        p (Equation params _ _) = params

hasExpression :: (Expression -> Bool) -> Inspection
hasExpression f binding = has f (expressionsOf binding)

hasRhs :: (Rhs -> Bool)-> Inspection
hasRhs f binding = has f (rhssOf binding)

hasDecl :: (Declaration -> Bool) -> GlobalInspection
hasDecl f = has f parseDecls

-- private

has f g = any f . g



