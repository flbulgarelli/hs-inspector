module Language.Haskell.Inspector.Smell where

import Language.Haskell.Explorer
import Language.Haskell.Syntax
import Language.Haskell.Inspector

hasRedundantBooleanComparison :: Inspection
hasRedundantBooleanComparison = isBindingEO f
  where f (E (HsInfixApp x (HsQVarOp (UnQual (HsSymbol c))) y)) = any isBooleanLiteral [x, y] && isComp c
        f _ = False

        isComp c = c == "==" || c == "/="

hasRedundantIf :: Inspection
hasRedundantIf = isBindingEO f
  where f (E (HsIf _ x y)) = all isBooleanLiteral [x, y]
        f _            = False


hasRedundantGuards :: Inspection
hasRedundantGuards = isBindingRhs f -- TODO not true when condition is a pattern
  where f (HsGuardedRhss [
            HsGuardedRhs _ _ x,
            HsGuardedRhs _ (HsVar (UnQual (HsIdent "otherwise"))) y]) = all isBooleanLiteral [x, y]
        f _ = False

hasRedundantLambda :: Inspection
hasRedundantLambda = isBindingEO f
  where f (E (HsLambda _ [HsPVar (HsIdent x)] (HsApp _ (HsVar (UnQual (HsIdent y)))))) = x == y
        f _ = False -- TODO consider parenthesis and symbols

isBooleanLiteral (HsCon (UnQual (HsIdent "True")))  = True
isBooleanLiteral (HsCon (UnQual (HsIdent "False"))) = True
isBooleanLiteral _                                  = False
