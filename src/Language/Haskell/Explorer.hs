module Language.Haskell.Explorer (
  parseDecls,
  declsOf,
  rhssOf,
  expressionsOf,
  EO(..),
  Binding,
  Code) where

import Language.Haskell.Syntax
import Language.Haskell.Names
import Language.Haskell.Parser
import Data.Maybe (fromMaybe, isJust)

type Binding = String
type Code = String

data EO = E HsExp | O HsQOp

-- xxxOf functions: take a binding and code

declsOf :: Binding -> Code -> [HsDecl]
declsOf binding = filter (isBinding binding) . parseDecls

rhssOf :: Binding -> Code -> [HsRhs]
rhssOf binding = concatMap rhsForBinding . declsOf binding

expressionsOf :: Binding -> Code -> [EO]
expressionsOf binding code = do
  rhs <- rhssOf binding code
  top <- topExpressions rhs
  unfoldExpression top

parseDecls :: Code -> [HsDecl]
parseDecls code
  | ParseOk (HsModule _ _ _ _ decls) <- parseModule code = decls
  | otherwise = []

-- private

topExpressions :: HsRhs -> [EO]
topExpressions (HsUnGuardedRhs e) = [E e]
topExpressions (HsGuardedRhss rhss) = rhss >>= \(HsGuardedRhs _ es1 es2) -> [E es1, E es2]

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

isBinding :: Binding -> HsDecl -> Bool
isBinding binding = (==binding).declName

rhsForBinding :: HsDecl -> [HsRhs]
rhsForBinding (HsPatBind _ _ rhs localDecls) = concatRhs rhs localDecls
rhsForBinding (HsFunBind cases) = cases >>= \(HsMatch _ _ _ rhs localDecls) -> concatRhs rhs localDecls
rhsForBinding _ = []

concatRhs rhs l = [rhs] ++ concatMap rhsForBinding l


orNil = fromMaybe []

