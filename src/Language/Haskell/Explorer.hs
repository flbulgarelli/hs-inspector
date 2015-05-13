module Language.Haskell.Explorer (
  parseDecls,
  parseBindings,
  declsOf,
  rhssOf,
  bindingsOf,
  transitiveBindingsOf,
  expressionsOf,
  expressionToBinding,
  Expression(..),
  Binding,
  Code) where

import Language.Haskell.Syntax
import Language.Haskell.Names
import Language.Haskell.Parser
import Data.Maybe (maybeToList)
import Data.List (nub)

type Binding = String
type Code = String

data Expression = E HsExp | O HsQOp

-- xxxOf functions: take a binding and code
-- parseXxx functions: take just code

declsOf :: Binding -> Code -> [HsDecl]
declsOf binding = filter (isBinding binding) . parseDecls

rhssOf :: Binding -> Code -> [HsRhs]
rhssOf binding = concatMap rhsForBinding . declsOf binding

expressionsOf :: Binding -> Code -> [Expression]
expressionsOf binding code = do
  rhs <- rhssOf binding code
  top <- topExpressions rhs
  unfoldExpression top

bindingsOf :: Binding -> Code -> [Binding]
bindingsOf binding code = nub $ do
          expr <- expressionsOf binding code
          maybeToList . expressionToBinding $ expr

transitiveBindingsOf :: Binding -> Code -> [Binding]
transitiveBindingsOf binding code =  expand (`bindingsOf` code) binding

parseDecls :: Code -> [HsDecl]
parseDecls code
  | ParseOk (HsModule _ _ _ _ decls) <- parseModule code = decls
  | otherwise = []

parseBindings :: Code -> [Binding]
parseBindings = map declName . parseDecls

expressionToBinding :: Expression -> Maybe Binding
expressionToBinding (O (HsQVarOp q)) = qName q
expressionToBinding (E (HsVar    q)) = qName q
expressionToBinding _                = Nothing

-- private

topExpressions :: HsRhs -> [Expression]
topExpressions (HsUnGuardedRhs e) = [E e]
topExpressions (HsGuardedRhss rhss) = rhss >>= \(HsGuardedRhs _ es1 es2) -> [E es1, E es2]

unfoldExpression :: Expression -> [Expression]
unfoldExpression expr = expr : concatMap unfoldExpression (subExpressions expr)

subExpressions :: Expression -> [Expression]
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


expand :: Eq a => (a-> [a]) -> a -> [a]
expand f x = expand' [] f [x]

expand' _ _ [] = []
expand' ps f (x:xs) | elem x ps = expand' ps f xs
                    | otherwise = [x] ++ expand' (x:ps) f (xs ++ f x)

