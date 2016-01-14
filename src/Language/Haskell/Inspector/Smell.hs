module Language.Haskell.Inspector.Smell (
  hasRedundantBooleanComparison,
  hasRedundantIf,
  hasRedundantGuards,
  hasRedundantLambda,
  hasRedundantParameter) where

import Language.Haskell.Explorer
import Language.Haskell.Mu
import Language.Haskell.Inspector


-- | Inspection that tells whether a binding has expressions like 'x == True'
hasRedundantBooleanComparison :: Inspection
hasRedundantBooleanComparison = hasExpression f
  where f (MuInfixApp x c y) = any isBooleanLiteral [x, y] && isComp c
        f _ = False

        isComp c = c == "==" || c == "/="

-- | Inspection that tells whether a binding has an if expression where both branches return
-- boolean literals
hasRedundantIf :: Inspection
hasRedundantIf = hasExpression f
  where f (MuIf _ x y) = all isBooleanLiteral [x, y]
        f _            = False


-- | Inspection that tells whether a binding has guards where both branches return
-- boolean literals
hasRedundantGuards :: Inspection
hasRedundantGuards = hasRhs f -- TODO not true when condition is a pattern
  where f (MuGuardedRhss [
            MuGuardedRhs _ x,
            MuGuardedRhs (MuVar "otherwise") y]) = all isBooleanLiteral [x, y]
        f _ = False


-- | Inspection that tells whether a binding has lambda expressions like '\x -> g x'
hasRedundantLambda :: Inspection
hasRedundantLambda = hasExpression f
  where f (MuLambda [MuPVar (x)] (MuApp _ (MuVar (y)))) = x == y
        f _ = False -- TODO consider parenthesis and symbols

-- | Inspection that tells whether a binding has parameters that
-- can be avoided using point-free
hasRedundantParameter :: Inspection
hasRedundantParameter binding = any f . declsOf binding
  where f (MuFunction [
             MuMatch _ params (MuUnGuardedRhs (MuApp _ (MuVar arg))) _ ]) | (MuPVar param) <- last params = param == arg
        f _ = False
--private
isBooleanLiteral (MuCon "True")  = True
isBooleanLiteral (MuCon "False") = True
isBooleanLiteral _                                  = False
