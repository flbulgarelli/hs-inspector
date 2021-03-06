module Language.Haskell.Inspector.Smell (
  hasRedundantBooleanComparison,
  hasRedundantIf,
  hasRedundantGuards,
  hasRedundantLambda,
  hasRedundantParameter) where

import Language.Haskell.Explorer
import Language.Haskell.Syntax
import Language.Haskell.Inspector


-- | Inspection that tells whether a binding has expressions like 'x == True'
hasRedundantBooleanComparison :: Inspection
hasRedundantBooleanComparison = hasExpression f
  where f (E (HsInfixApp x (HsQVarOp (UnQual (HsSymbol c))) y)) = any isBooleanLiteral [x, y] && isComp c
        f _ = False

        isComp c = c == "==" || c == "/="

-- | Inspection that tells whether a binding has an if expression where both branches return
-- boolean literals
hasRedundantIf :: Inspection
hasRedundantIf = hasExpression f
  where f (E (HsIf _ x y)) = all isBooleanLiteral [x, y]
        f _            = False


-- | Inspection that tells whether a binding has guards where both branches return
-- boolean literals
hasRedundantGuards :: Inspection
hasRedundantGuards = hasRhs f -- TODO not true when condition is a pattern
  where f (HsGuardedRhss [
            HsGuardedRhs _ _ x,
            HsGuardedRhs _ (HsVar (UnQual (HsIdent "otherwise"))) y]) = all isBooleanLiteral [x, y]
        f _ = False


-- | Inspection that tells whether a binding has lambda expressions like '\x -> g x'
hasRedundantLambda :: Inspection
hasRedundantLambda = hasExpression f
  where f (E (HsLambda _ [HsPVar (HsIdent x)] (HsApp _ (HsVar (UnQual (HsIdent y)))))) = x == y
        f _ = False -- TODO consider parenthesis and symbols

-- | Inspection that tells whether a binding has parameters that
-- can be avoided using point-free
hasRedundantParameter :: Inspection
hasRedundantParameter binding = any f . declsOf binding
  where f (HsFunBind [
             HsMatch _ _ params (HsUnGuardedRhs (HsApp _ (HsVar (UnQual arg)))) _ ]) | (HsPVar param) <- last params = param == arg
        f _ = False
--private
isBooleanLiteral (HsCon (UnQual (HsIdent "True")))  = True
isBooleanLiteral (HsCon (UnQual (HsIdent "False"))) = True
isBooleanLiteral _                                  = False
