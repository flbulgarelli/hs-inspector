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
  where f (O ".") = True
        f _ = False

-- | Inspection that tells whether a binding uses guards
-- in its definition
hasGuards :: Inspection
hasGuards = hasRhs f
  where f (MuGuardedRhss _) = True
        f _ = False

-- | Inspection that tells whether a binding uses ifs
-- in its definition
hasIf :: Inspection
hasIf = hasExpression f
  where f (E (MuIf _ _ _)) = True
        f _ = False

-- | Inspection that tells whether a binding uses ifs or guards
-- in its definition
hasConditional :: Inspection
hasConditional target code = hasIf target code || hasGuards target code

-- | Inspection that tells whether a binding uses a lambda expression
-- in its definition
hasLambda :: Inspection
hasLambda = hasExpression f
  where f (E (MuLambda _ _)) = True
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
  where f (E (MuListComp _ _)) = True
        f _ = False

-- | Inspection that tells whether a top level binding exists
hasBinding :: Inspection
hasBinding binding = not.null.rhssOf binding

hasTypeDeclaration :: Inspection
hasTypeDeclaration binding = hasDecl f
  where f (MuTypeDecl hsName) = isName binding hsName
        f _                   = False

hasTypeSignature :: Inspection
hasTypeSignature binding = hasDecl f
  where f (MuTypeSig hsName)  = isName binding hsName
        f _                       = False

hasAnonymousVariable :: Inspection
hasAnonymousVariable binding = any f . declsOf binding
  where f (MuFunBind hsMatches)    = any (any (== MuPWildCard) . p) hsMatches
        f _                        = False
        p (MuMatch _ params _ _) = params

hasExpression :: (Expression -> Bool) -> Inspection
hasExpression f binding = has f (expressionsOf binding)

hasRhs :: (MuRhs -> Bool)-> Inspection
hasRhs f binding = has f (rhssOf binding)

hasDecl :: (MuDecl -> Bool) -> GlobalInspection
hasDecl f = has f parseDecls

-- private

has f g = any f . g



