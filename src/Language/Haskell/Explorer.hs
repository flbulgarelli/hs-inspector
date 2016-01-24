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
type AST = Program

-- xxxOf functions: take a binding and code
-- parseXxx functions: take just code

instance IsString Program where
  fromString = astOf

isParseable :: String -> Bool
isParseable code | ParseOk ast <- parseModule code = True
                 | otherwise = False

astOf :: String -> AST
astOf code | ParseOk ast <- parseModule code = mu ast

mu :: HsModule -> Program
mu (HsModule _ _ _ _ decls) = (Program (concatMap muDecls decls))
  where
    muDecls (HsTypeDecl _ name _ _)      = [TypeAlias (muName name)]
    muDecls (HsDataDecl _ _ name _ _ _ ) = [RecordDeclaration (muName name)]
    muDecls (HsTypeSig _ names _) = map (\name -> TypeSignature (muName name)) names
    muDecls (HsFunBind equations) | (HsMatch _ name _ _ _) <- head equations =
                                        [FunctionDeclaration (muName name) (map muEquation equations)]
    muDecls (HsPatBind _ (HsPVar name) rhs decls) = [ConstantDeclaration (muName name) (muRhs rhs) (concatMap muDecls decls)]
    muDecls _ = []

    muEquation :: HsMatch -> Equation
    muEquation (HsMatch _ _ patterns rhs locals) =
         Equation (map muPat patterns) (muRhs rhs) (concatMap muDecls locals)

    muRhs (HsUnGuardedRhs exp)          = UnguardedRhs (muExp exp)
    muRhs (HsGuardedRhss  guards) = GuardedRhss (map muGuardedRhs guards)

    muGuardedRhs (HsGuardedRhs _ condition body) = (GuardedRhs (muExp condition) (muExp body))

    muPat (HsPVar name) = VariablePattern (muName name)                 -- ^ variable
    muPat (HsPLit _) = LiteralPattern ""              -- ^ literal constant
    --Pattern HsPInfixApp = InfixApplicationPattern Pattern MuQName Pattern
    --Pattern HsPApp = ApplicationPattern MuQName [Pattern]        -- ^ data constructor and argument
    muPat (HsPTuple elements) = TuplePattern (map muPat elements)
    muPat (HsPList elements) = ListPattern (map muPat elements)
    muPat (HsPParen pattern) = muPat pattern
    --Pattern HsPAsPat = AsPattern String Pattern
    muPat HsPWildCard = WildcardPattern
    muPat _ = OtherPattern

    muExp (HsVar name) = Variable (muQName name)
    muExp (HsCon (UnQual (HsIdent "True")))  = Literal (MuBool True)
    muExp (HsCon (UnQual (HsIdent "False"))) = Literal (MuBool False)
    muExp (HsCon name)                       = Variable (muQName name)
    muExp (HsLit lit) = Literal (muLit lit)
    muExp (HsInfixApp e1 op e2) = InfixApplication (muExp e1) (muQOp op) (muExp e2)  -- ^ infix application
    muExp (HsApp e1 e2) = Application (muExp e1) (muExp e2)             -- ^ ordinary application
    muExp (HsNegApp e) = Application (Variable "-") (muExp e)
    muExp (HsLambda _ args exp) = Lambda (map muPat args) (muExp exp)
    --muExp HsLet = Let [Declaration] Expression          -- ^ local declarations with @let@
    muExp (HsIf e1 e2 e3) = If (muExp e1) (muExp e2) (muExp e3)
    --muExp HsMatch = Match Expression [Alternative]          -- ^ @case@ /exp/ @of@ /alts/
    muExp (HsTuple elements) = MuTuple (map muExp elements)               -- ^ tuple Expression
    muExp (HsList elements) = MuList (map muExp elements)
    muExp (HsParen e) = (muExp e)
    muExp (HsEnumFrom from)              = Application (Variable "enumFrom") (muExp from)
    muExp (HsEnumFromTo from to)         = Application (Application (Variable "enumFromTo") (muExp from)) (muExp to)
    muExp (HsEnumFromThen from thn)      = Application (Application (Variable "enumFromThen") (muExp from)) (muExp thn)
    muExp (HsEnumFromThenTo from thn to) = Application (Application (Application (Variable "enumFromThenTo") (muExp from)) (muExp thn)) (muExp to)
    muExp (HsListComp exp stmts)         = ListComprehension (muExp exp) (map muStmt stmts)
    muExp _ = ExpressionOther

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

    muStmt (HsGenerator _ pat exp) = MuGenerator (muPat pat) (muExp exp)
    muStmt (HsQualifier exp) = MuQualifier (muExp exp)

declsOf :: Binding -> AST -> [Declaration]
declsOf binding = filter (isBinding binding) . parseDecls

rhssOf :: Binding -> AST -> [Rhs]
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

parseDecls :: AST -> [Declaration]
parseDecls (Program decls) = decls

parseBindings :: AST -> [Binding]
parseBindings = map declName . parseDecls

expressionToBinding :: Expression -> Maybe Binding
expressionToBinding (Variable    q) = Just q
expressionToBinding _                = Nothing

-- private

topExpressions :: Rhs -> [Expression]
topExpressions (UnguardedRhs e) = [e]
topExpressions (GuardedRhss rhss) = rhss >>= \(GuardedRhs es1 es2) -> [es1, es2]

unfoldExpression :: Expression -> [Expression]
unfoldExpression expr = expr : concatMap unfoldExpression (subExpressions expr)

subExpressions :: Expression -> [Expression]
subExpressions (InfixApplication a b c) = [a, (Variable b), c]
subExpressions (Application a b)        = [a, b]
subExpressions (Lambda _ a)   = [a]
subExpressions (MuList as)      = as
subExpressions (ListComprehension a _)   = [a] --TODO
subExpressions (MuTuple as)      = as
subExpressions (If a b c)       = [a, b, c]
subExpressions _ = []

isBinding :: Binding -> Declaration -> Bool
isBinding binding = (==binding).declName

rhsForBinding :: Declaration -> [Rhs]
rhsForBinding (ConstantDeclaration _ rhs localDecls) = concatRhs rhs localDecls
rhsForBinding (FunctionDeclaration _ cases) = cases >>= \(Equation _ rhs localDecls) -> concatRhs rhs localDecls
rhsForBinding _ = []

concatRhs rhs l = [rhs] ++ concatMap rhsForBinding l


expand :: Eq a => (a-> [a]) -> a -> [a]
expand f x = expand' [] f [x]

expand' _ _ [] = []
expand' ps f (x:xs) | elem x ps = expand' ps f xs
                    | otherwise = [x] ++ expand' (x:ps) f (xs ++ f x)

