module Language.Haskell.Inspector.Smell where

import Language.Haskell.Explorer
import Language.Haskell.Syntax
import Language.Haskell.Inspector

hasRedundantBooleanComparison :: Inspection
hasRedundantBooleanComparison = testAnyWithBindingExpr f
  where f (E (HsInfixApp x (HsQVarOp (UnQual (HsSymbol c))) y)) = any isBooleanLiteral [x, y] && isComp c
        f _ = False

        isComp c = c == "==" || c == "/="

hasRedundantIf :: Inspection
hasRedundantIf = testAnyWithBindingExpr f
  where f (E (HsIf _ x y)) = all isBooleanLiteral [x, y]
        f _            = False

isBooleanLiteral (HsCon (UnQual (HsIdent "True")))  = True
isBooleanLiteral (HsCon (UnQual (HsIdent "False"))) = True
isBooleanLiteral _                                  = False
