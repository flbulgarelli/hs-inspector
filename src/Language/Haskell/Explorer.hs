module Language.Haskell.Explorer (
  astOf,
  parseDecls,
  parseBindings,
  declsOf,
  rhssOf,
  bindingsOf,
  transitiveBindingsOf,
  expressionsOf,
  expressionToBinding,
  isParseable,
  Expression(..),
  Binding,
  AST) where

import Language.Haskell.Syntax
import Language.Haskell.Mu
import Language.Haskell.Names
import Language.Haskell.Parser
import Data.Maybe (maybeToList)
import Data.List (nub)
import Data.String (IsString(..))

type Binding = String
type AST = MuModule

data Expression = E MuExp | O MuQOp

-- xxxOf functions: take a binding and code
-- parseXxx functions: take just code

instance IsString MuModule where
  fromString = astOf

isParseable :: String -> Bool
isParseable code | ParseOk ast <- parseModule code = True
                 | otherwise = False

astOf :: String -> AST
astOf code | ParseOk ast <- parseModule code = mu ast

mu :: HsModule -> MuModule
mu _ = error "uninplemented"

declsOf :: Binding -> AST -> [MuDecl]
declsOf binding = filter (isBinding binding) . parseDecls

rhssOf :: Binding -> AST -> [MuRhs]
rhssOf binding = concatMap rhsForBinding . declsOf binding

expressionsOf :: Binding -> AST -> [Expression]
expressionsOf binding code = do
  rhs <- rhssOf binding code
  top <- topExpressions rhs
  unfoldExpression top

bindingsOf :: Binding -> AST -> [Binding]
bindingsOf binding code = nub $ do
          expr <- expressionsOf binding code
          maybeToList . expressionToBinding $ expr

transitiveBindingsOf :: Binding -> AST -> [Binding]
transitiveBindingsOf binding code =  expand (`bindingsOf` code) binding

parseDecls :: AST -> [MuDecl]
parseDecls (MuModule _ decls) = decls

parseBindings :: AST -> [Binding]
parseBindings = map declName . parseDecls

expressionToBinding :: Expression -> Maybe Binding
expressionToBinding (O (MuQVarOp q)) = qName q
expressionToBinding (E (MuVar    q)) = qName q
expressionToBinding _                = Nothing

-- private

topExpressions :: MuRhs -> [Expression]
topExpressions (MuUnGuardedRhs e) = [E e]
topExpressions (MuGuardedRhss rhss) = rhss >>= \(MuGuardedRhs es1 es2) -> [E es1, E es2]

unfoldExpression :: Expression -> [Expression]
unfoldExpression expr = expr : concatMap unfoldExpression (subExpressions expr)

subExpressions :: Expression -> [Expression]
subExpressions (E (MuInfixApp a b c)) = [E a, O b, E c]
subExpressions (E (MuApp a b))        = [E a, E b]
subExpressions (E (MuNegApp a))       = [E a]
subExpressions (E (MuLambda _ a))   = [E a]
subExpressions (E (MuList as))        = map (E) as
subExpressions (E (MuListComp a _))   = [E a] --TODO
subExpressions (E (MuTuple as))       = map (E) as
subExpressions (E (MuParen a))        = [E a]
subExpressions (E (MuIf a b c))       = [E a, E b, E c]
subExpressions _ = []

isBinding :: Binding -> MuDecl -> Bool
isBinding binding = (==binding).declName

rhsForBinding :: MuDecl -> [MuRhs]
rhsForBinding (MuPatBind _ rhs localDecls) = concatRhs rhs localDecls
rhsForBinding (MuFunBind cases) = cases >>= \(MuMatch _ _ rhs localDecls) -> concatRhs rhs localDecls
rhsForBinding _ = []

concatRhs rhs l = [rhs] ++ concatMap rhsForBinding l


expand :: Eq a => (a-> [a]) -> a -> [a]
expand f x = expand' [] f [x]

expand' _ _ [] = []
expand' ps f (x:xs) | elem x ps = expand' ps f xs
                    | otherwise = [x] ++ expand' (x:ps) f (xs ++ f x)

