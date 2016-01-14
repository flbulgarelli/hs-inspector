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
  where f (E (MuInfixApp x (MuQVarOp (UnQual (MuSymbol c))) y)) = any isBooleanLiteral [x, y] && isComp c
        f _ = False

        isComp c = c == "==" || c == "/="

-- | Inspection that tells whether a binding has an if expression where both branches return
-- boolean literals
hasRedundantIf :: Inspection
hasRedundantIf = hasExpression f
  where f (E (MuIf _ x y)) = all isBooleanLiteral [x, y]
        f _            = False


-- | Inspection that tells whether a binding has guards where both branches return
-- boolean literals
hasRedundantGuards :: Inspection
hasRedundantGuards = hasRhs f -- TODO not true when condition is a pattern
  where f (MuGuardedRhss [
            MuGuardedRhs _ x,
            MuGuardedRhs (MuVar (UnQual (MuIdent "otherwise"))) y]) = all isBooleanLiteral [x, y]
        f _ = False


-- | Inspection that tells whether a binding has lambda expressions like '\x -> g x'
hasRedundantLambda :: Inspection
hasRedundantLambda = hasExpression f
  where f (E (MuLambda [MuPVar (MuIdent x)] (MuApp _ (MuVar (UnQual (MuIdent y)))))) = x == y
        f _ = False -- TODO consider parenthesis and symbols

-- | Inspection that tells whether a binding has parameters that
-- can be avoided using point-free
hasRedundantParameter :: Inspection
hasRedundantParameter binding = any f . declsOf binding
  where f (MuFunBind [
             MuMatch _ params (MuUnGuardedRhs (MuApp _ (MuVar (UnQual arg)))) _ ]) | (MuPVar param) <- last params = param == arg
        f _ = False
--private
isBooleanLiteral (MuCon (UnQual (MuIdent "True")))  = True
isBooleanLiteral (MuCon (UnQual (MuIdent "False"))) = True
isBooleanLiteral _                                  = False
