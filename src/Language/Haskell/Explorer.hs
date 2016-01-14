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
import Data.List (nub, intercalate)
import Data.String (IsString(..))

type Binding = String
type AST = MuModule

data Expression = E MuExp | O String

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
mu (HsModule _ (Module name) _ _ decls) = (MuModule name (concatMap muDecls decls))
  where
    muDecls (HsTypeDecl _ name _ _) = [MuTypeDecl (muName name)] --MuType
    --muDecls HsDataDecl = MuDataDecl    MuName [MuName] [MuConDecl] [MuQName]
    --muDecls HsInfixDecl = MuInfixDecl   MuAssoc Int [MuOp]
    muDecls (HsTypeSig _ names _) = map (\name -> MuTypeSig (muName name)) names --MuQualType
    muDecls (HsFunBind equations) = [MuFunBind  (map muEquation equations)]
    --muDecls HsPatBind = MuPatBind     MuPat MuRhs {-where-} [MuDecl]
    muDecls _ = []

    muEquation :: HsMatch -> MuMatch
    muEquation (HsMatch _ name patterns rhs locals) =
         MuMatch (muName name) (map muPat patterns) (muRhs rhs) (concatMap muDecls locals)

    muRhs (HsUnGuardedRhs exp)          = MuUnGuardedRhs (muExp exp)
    muRhs (HsGuardedRhss  guards) = MuGuardedRhss (map muGuardedRhs guards)

    muGuardedRhs (HsGuardedRhs _ condition body) = (MuGuardedRhs (muExp condition) (muExp body))

    muPat (HsPVar name) = MuPVar (muName name)                 -- ^ variable
    muPat (HsPLit _) = MuPLit ""              -- ^ literal constant
    --muPat HsPInfixApp = MuPInfixApp MuPat MuQName MuPat
    --muPat HsPApp = MuPApp MuQName [MuPat]        -- ^ data constructor and argument
    muPat (HsPTuple elements) = MuPTuple (map muPat elements)              -- ^ tuple pattern
    muPat (HsPList elements) = MuPList (map muPat elements)               -- ^ list pattern
    --muPat HsPParen = MuPParen MuPat                -- ^ parenthesized pattern
    --muPat HsPAsPat = MuPAsPat String MuPat         -- ^ @\@@-pattern
    muPat HsPWildCard = MuPWildCard                   -- ^ wildcard pattern (@_@)
    muPat _ = MuPOther

    muExp (HsVar name) = MuVar (muQName name)                 -- ^ variable
    --muExp HsCon = MuCon MuQName                 -- ^ data constructor
    muExp (HsLit lit) = MuLit (muLit lit)
    muExp (HsInfixApp e1 op e2) = MuInfixApp (muExp e1) (muQOp op) (muExp e2)  -- ^ infix application
    muExp (HsApp e1 e2) = MuApp (muExp e1) (muExp e2)             -- ^ ordinary application
    muExp (HsNegApp e) = MuNegApp (muExp e)                -- ^ negation expression @-@ /exp/
    muExp (HsLambda _ args exp) = MuLambda (map muPat args) (muExp exp)
    --muExp HsLet = MuLet [MuDecl] MuExp          -- ^ local declarations with @let@
    muExp (HsIf e1 e2 e3) = MuIf (muExp e1) (muExp e2) (muExp e3)
    --muExp HsCase = MuCase MuExp [MuAlt]          -- ^ @case@ /exp/ @of@ /alts/
    --muExp HsTuple = MuTuple [MuExp]               -- ^ tuple expression
    muExp (HsList elements) = MuList (map muExp elements)
    --muExp HsParen = MuParen MuExp                 -- ^ parenthesized expression
    --muExp HsEnumFrom = MuEnumFrom MuExp              -- ^ unbounded arithmetic sequence,
                                            -- incrementing by 1
    --muExp HsEnumFromTo = MuEnumFromTo MuExp MuExp      -- ^ bounded arithmetic sequence,
                                            -- incrementing by 1
    --muExp HsEnumFromThen = MuEnumFromThen MuExp MuExp    -- ^ unbounded arithmetic sequence,
                                            -- with first two elements given
    --muExp HsEnumFromThenTo = MuEnumFromThenTo MuExp MuExp MuExp
                                            -- ^ bounded arithmetic sequence,
                                            -- with first two elements given
    --muExp HsListComp = MuListComp MuExp [MuStmt]     -- ^ list comprehension
    muExp _ = MuExpOther

    muLit (HsChar        v) = show v
    muLit (HsString      v) = show v          -- ^ string literal
    muLit (HsInt         v) = show v         -- ^ integer literal
    muLit (HsFrac        v) = show v        -- ^ floating point literal
    muLit (HsCharPrim    v) = show v            -- ^ GHC unboxed character literal
    muLit (HsStringPrim  v) = show v          -- ^ GHC unboxed string literal
    muLit (HsIntPrim     v) = show v         -- ^ GHC unboxed integer literal
    muLit (HsFloatPrim   v) = show v        -- ^ GHC unboxed float literal
    muLit (HsDoublePrim  v) = show v

    muName :: HsName -> String
    muName (HsSymbol n) = n
    muName (HsIdent  n) = n

    muQName (Qual _ n) = muName n
    muQName (UnQual n) = muName n
    muQName (Special HsUnitCon) = "()"
    muQName (Special HsListCon) = "[]"
    muQName (Special HsFunCon) =  "->"
    muQName (Special (HsTupleCon times)) =  intercalate "" . replicate times $ ","
    muQName (Special (HsCons)) =  ":"

    muQOp (HsQVarOp name) = muQName name
    muQOp (HsQConOp name) = muQName name

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
expressionToBinding (O q) = Just q
expressionToBinding (E (MuVar    q)) = Just q
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

