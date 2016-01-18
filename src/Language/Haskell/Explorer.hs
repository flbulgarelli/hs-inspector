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
  MuExp(..),
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
type AST = MuProgram

-- xxxOf functions: take a binding and code
-- parseXxx functions: take just code

instance IsString MuProgram where
  fromString = astOf

isParseable :: String -> Bool
isParseable code | ParseOk ast <- parseModule code = True
                 | otherwise = False

astOf :: String -> AST
astOf code | ParseOk ast <- parseModule code = mu ast

mu :: HsModule -> MuProgram
mu (HsModule _ _ _ _ decls) = (MuProgram (concatMap muDecls decls))
  where
    muDecls (HsTypeDecl _ name _ _)      = [MuTypeAlias (muName name)]
    muDecls (HsDataDecl _ _ name _ _ _ ) = [MuRecordDeclaration (muName name)]
    muDecls (HsTypeSig _ names _) = map (\name -> MuTypeSignature (muName name)) names
    muDecls (HsFunBind equations) | (HsMatch _ name _ _ _) <- head equations =
                                        [MuFunction (muName name) (map muEquation equations)]
    muDecls (HsPatBind _ (HsPVar name) rhs decls) = [MuConstant (muName name) (muRhs rhs) (concatMap muDecls decls)]
    muDecls _ = []

    muEquation :: HsMatch -> MuEquation
    muEquation (HsMatch _ _ patterns rhs locals) =
         MuEquation (map muPat patterns) (muRhs rhs) (concatMap muDecls locals)

    muRhs (HsUnGuardedRhs exp)          = MuUnGuardedRhs (muExp exp)
    muRhs (HsGuardedRhss  guards) = MuGuardedRhss (map muGuardedRhs guards)

    muGuardedRhs (HsGuardedRhs _ condition body) = (MuGuardedRhs (muExp condition) (muExp body))

    muPat (HsPVar name) = MuPVar (muName name)                 -- ^ variable
    muPat (HsPLit _) = MuPLit ""              -- ^ literal constant
    --MuPat HsPInfixApp = MuPInfixApp MuPat MuQName MuPat
    --MuPat HsPApp = MuPApp MuQName [MuPat]        -- ^ data constructor and argument
    muPat (HsPTuple elements) = MuPTuple (map muPat elements)
    muPat (HsPList elements) = MuPList (map muPat elements)
    muPat (HsPParen pattern) = muPat pattern
    --MuPat HsPAsPat = MuPAsPat String MuPat
    muPat HsPWildCard = MuPWildCard
    muPat _ = MuPOther

    muExp (HsVar name) = MuVar (muQName name)
    muExp (HsCon (UnQual (HsIdent "True")))  = MuLit (MuBool True)
    muExp (HsCon (UnQual (HsIdent "False"))) = MuLit (MuBool False)
    muExp (HsCon name)                       = MuVar (muQName name)
    muExp (HsLit lit) = MuLit (muLit lit)
    muExp (HsInfixApp e1 op e2) = MuInfixApp (muExp e1) (muQOp op) (muExp e2)  -- ^ infix application
    muExp (HsApp e1 e2) = MuApp (muExp e1) (muExp e2)             -- ^ ordinary application
    muExp (HsNegApp e) = MuApp (MuVar "-") (muExp e)
    muExp (HsLambda _ args exp) = MuLambda (map muPat args) (muExp exp)
    --muExp HsLet = MuLet [MuDeclaration] MuExp          -- ^ local declarations with @let@
    muExp (HsIf e1 e2 e3) = MuIf (muExp e1) (muExp e2) (muExp e3)
    --muExp HsCase = MuCase MuExp [MuAlt]          -- ^ @case@ /exp/ @of@ /alts/
    muExp (HsTuple elements) = MuTuple (map muExp elements)               -- ^ tuple MuExp
    muExp (HsList elements) = MuList (map muExp elements)
    muExp (HsParen e) = (muExp e)
    muExp (HsEnumFrom from)              = MuApp (MuVar "enumFrom") (muExp from)
    muExp (HsEnumFromTo from to)         = MuApp (MuApp (MuVar "enumFromTo") (muExp from)) (muExp to)
    muExp (HsEnumFromThen from thn)      = MuApp (MuApp (MuVar "enumFromThen") (muExp from)) (muExp thn)
    muExp (HsEnumFromThenTo from thn to) = MuApp (MuApp (MuApp (MuVar "enumFromThenTo") (muExp from)) (muExp thn)) (muExp to)
    muExp (HsListComp exp stmts) = muStmt stmts exp
                            where
                                muStmt :: [HsStmt] -> HsExp -> MuExp
                                muStmt []     exp = MuApp (MuVar "return") (muExp exp)
                                muStmt (HsGenerator _ element list:xs) exp =
                                  MuInfixApp (muExp list) ">>=" (MuLambda [muPat element] (muStmt xs exp))

    muExp _ = MuExpOther

    muLit (HsChar        v) = MuString [v]
    muLit (HsString      v) = MuString v
    muLit (HsInt         v) = MuInteger v
    muLit (HsFrac        v) = MuFloat v
    muLit (HsCharPrim    v) = MuString [v]
    muLit (HsStringPrim  v) = MuString v
    muLit (HsIntPrim     v) = MuInteger v
    muLit (HsFloatPrim   v) = MuFloat v
    muLit (HsDoublePrim  v) = MuFloat v

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

declsOf :: Binding -> AST -> [MuDeclaration]
declsOf binding = filter (isBinding binding) . parseDecls

rhssOf :: Binding -> AST -> [MuRhs]
rhssOf binding = concatMap rhsForBinding . declsOf binding

expressionsOf :: Binding -> AST -> [MuExp]
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

parseDecls :: AST -> [MuDeclaration]
parseDecls (MuProgram decls) = decls

parseBindings :: AST -> [Binding]
parseBindings = map declName . parseDecls

expressionToBinding :: MuExp -> Maybe Binding
expressionToBinding (MuVar    q) = Just q
expressionToBinding _                = Nothing

-- private

topExpressions :: MuRhs -> [MuExp]
topExpressions (MuUnGuardedRhs e) = [e]
topExpressions (MuGuardedRhss rhss) = rhss >>= \(MuGuardedRhs es1 es2) -> [es1, es2]

unfoldExpression :: MuExp -> [MuExp]
unfoldExpression expr = expr : concatMap unfoldExpression (subExpressions expr)

subExpressions :: MuExp -> [MuExp]
subExpressions (MuInfixApp a b c) = [a, (MuVar b), c]
subExpressions (MuApp a b)        = [a, b]
subExpressions (MuLambda _ a)   = [a]
subExpressions (MuList as)      = as
subExpressions (MuTuple as)      = as
subExpressions (MuIf a b c)       = [a, b, c]
subExpressions _ = []

isBinding :: Binding -> MuDeclaration -> Bool
isBinding binding = (==binding).declName

rhsForBinding :: MuDeclaration -> [MuRhs]
rhsForBinding (MuConstant _ rhs localDecls) = concatRhs rhs localDecls
rhsForBinding (MuFunction _ cases) = cases >>= \(MuEquation _ rhs localDecls) -> concatRhs rhs localDecls
rhsForBinding _ = []

concatRhs rhs l = [rhs] ++ concatMap rhsForBinding l


expand :: Eq a => (a-> [a]) -> a -> [a]
expand f x = expand' [] f [x]

expand' _ _ [] = []
expand' ps f (x:xs) | elem x ps = expand' ps f xs
                    | otherwise = [x] ++ expand' (x:ps) f (xs ++ f x)

