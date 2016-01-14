module Language.Haskell.Mu (
    -- * Modules
    MuModule(..), MuExportSpec(..),
    MuImportDecl(..), MuImportSpec(..), MuAssoc(..),
    -- * Declarations
    MuDecl(..), MuConDecl(..), MuBangType(..),
    MuMatch(..), MuRhs(..), MuGuardedRhs(..),
    MuSafety(..),
    -- * Class Assertions and Contexts
    MuQualType(..), MuContext, MuAsst,
    -- * Types
    MuType(..),
    -- * Expressions
    MuExp(..), MuStmt(..), MuFieldUpdate(..),
    MuAlt(..), MuGuardedAlts(..), MuGuardedAlt(..),
    -- * Patterns
    MuPat(..), MuPatField(..),
    -- * Literals
    MuLiteral(..),
    -- * Variables, Constructors and Operators
    Module(..), MuQName(..), MuName(..), MuQOp(..), MuOp(..),
    MuSpecialCon(..), MuCName(..),
  ) where


newtype Module = Module String
  deriving (Eq,Ord,Show)

data MuSpecialCon
        = MuUnitCon             -- ^ unit type and data constructor @()@
        | MuListCon             -- ^ list type constructor @[]@
        | MuFunCon              -- ^ function type constructor @->@
        | MuTupleCon Int        -- ^ /n/-ary tuple type and data
                                --   constructors @(,)@ etc
        | MuCons                -- ^ list data constructor @(:)@
  deriving (Eq,Ord,Show)

-- | This type is used to represent qualified variables, and also
-- qualified constructors.
data MuQName
        = Qual Module MuName    -- ^ name qualified with a module name
        | UnQual MuName         -- ^ unqualified name
        | Special MuSpecialCon  -- ^ built-in constructor with special syntax
  deriving (Eq,Ord,Show)

-- | This type is used to represent variables, and also constructors.
data MuName
        = MuIdent String        -- ^ /varid/ or /conid/
        | MuSymbol String       -- ^ /varsym/ or /consym/
  deriving (Eq,Ord,Show)

-- | Possibly qualified infix operators (/qop/), appearing in expressions.
data MuQOp
        = MuQVarOp MuQName      -- ^ variable operator (/qvarop/)
        | MuQConOp MuQName      -- ^ constructor operator (/qconop/)
  deriving (Eq,Ord,Show)

-- | Operators, appearing in @infix@ declarations.
data MuOp
        = MuVarOp MuName        -- ^ variable operator (/varop/)
        | MuConOp MuName        -- ^ constructor operator (/conop/)
  deriving (Eq,Ord,Show)

-- | A name (/cname/) of a component of a class or data type in an @import@
-- or export specification.
data MuCName
        = MuVarName MuName      -- ^ name of a method or field
        | MuConName MuName      -- ^ name of a data constructor
  deriving (Eq,Ord,Show)

-- | A Haskell source module.
data MuModule = MuModule Module (Maybe [MuExportSpec])
                         [MuImportDecl] [MuDecl]
  deriving (Show)

-- | Export specification.
data MuExportSpec
         = MuEVar MuQName                       -- ^ variable
         | MuEAbs MuQName                       -- ^ @T@:
                        -- a class or datatype exported abstractly,
                        -- or a type synonym.
         | MuEThingAll MuQName                  -- ^ @T(..)@:
                        -- a class exported with all of its methods, or
                        -- a datatype exported with all of its constructors.
         | MuEThingWith MuQName [MuCName]       -- ^ @T(C_1,...,C_n)@:
                        -- a class exported with some of its methods, or
                        -- a datatype exported with some of its constructors.
         | MuEModuleContents Module             -- ^ @module M@:
                        -- re-export a module.
  deriving (Eq,Show)

-- | Import declaration.
data MuImportDecl = MuImportDecl
        {
          importModule :: Module        -- ^ name of the module imported.
        , importQualified :: Bool       -- ^ imported @qualified@?
        , importAs :: Maybe Module      -- ^ optional alias name in an
                                        -- @as@ clause.
        , importSpecs :: Maybe (Bool,[MuImportSpec])
                        -- ^ optional list of import specifications.
                        -- The 'Bool' is 'True' if the names are excluded
                        -- by @hiding@.
        }
  deriving (Eq,Show)

-- | Import specification.
data MuImportSpec
         = MuIVar MuName                        -- ^ variable
         | MuIAbs MuName                        -- ^ @T@:
                        -- the name of a class, datatype or type synonym.
         | MuIThingAll MuName                   -- ^ @T(..)@:
                        -- a class imported with all of its methods, or
                        -- a datatype imported with all of its constructors.
         | MuIThingWith MuName [MuCName]        -- ^ @T(C_1,...,C_n)@:
                        -- a class imported with some of its methods, or
                        -- a datatype imported with some of its constructors.
  deriving (Eq,Show)

-- | Associativity of an operator.
data MuAssoc
         = MuAssocNone  -- ^ non-associative operator (declared with @infix@)
         | MuAssocLeft  -- ^ left-associative operator (declared with @infixl@).
         | MuAssocRight -- ^ right-associative operator (declared with @infixr@)
  deriving (Eq,Show)

data MuDecl
         = MuTypeDecl    MuName [MuName] MuType
         | MuDataDecl    MuContext MuName [MuName] [MuConDecl] [MuQName]
         | MuInfixDecl   MuAssoc Int [MuOp]
         | MuNewTypeDecl MuContext MuName [MuName] MuConDecl [MuQName]
         | MuClassDecl   MuContext MuName [MuName] [MuDecl]
         | MuInstDecl    MuContext MuQName [MuType] [MuDecl]
         | MuDefaultDecl [MuType]
         | MuTypeSig     [MuName] MuQualType
         | MuFunBind     [MuMatch]
         | MuPatBind     MuPat MuRhs {-where-} [MuDecl]
         | MuForeignImport String MuSafety String MuName MuType
         | MuForeignExport String String MuName MuType
  deriving (Eq,Show)

-- | Clauses of a function binding.
data MuMatch
         = MuMatch MuName [MuPat] MuRhs {-where-} [MuDecl]
  deriving (Eq,Show)

-- | Declaration of a data constructor.
data MuConDecl
         = MuConDecl MuName [MuBangType]
                                -- ^ ordinary data constructor
         | MuRecDecl MuName [([MuName],MuBangType)]
                                -- ^ record constructor
  deriving (Eq,Show)

-- | The type of a constructor argument or field, optionally including
-- a strictness annotation.
data MuBangType
         = MuBangedTy   MuType  -- ^ strict component, marked with \"@!@\"
         | MuUnBangedTy MuType  -- ^ non-strict component
  deriving (Eq,Show)

-- | The right hand side of a function or pattern binding.
data MuRhs
         = MuUnGuardedRhs MuExp -- ^ unguarded right hand side (/exp/)
         | MuGuardedRhss  [MuGuardedRhs]
                                -- ^ guarded right hand side (/gdrhs/)
  deriving (Eq,Show)

-- | A guarded right hand side @|@ /exp/ @=@ /exp/.
-- The first expression will be Boolean-valued.
data MuGuardedRhs
         = MuGuardedRhs MuExp MuExp
  deriving (Eq,Show)

-- | Safety level for invoking a foreign entity
data MuSafety
        = MuSafe        -- ^ call may generate callbacks
        | MuUnsafe      -- ^ call will not generate callbacks
  deriving (Eq,Ord,Show)

-- | A type qualified with a context.
--   An unqualified type has an empty context.
data MuQualType
         = MuQualType MuContext MuType
  deriving (Eq,Show)

-- | Haskell types and type constructors.
data MuType
         = MuTyFun   MuType MuType      -- ^ function type
         | MuTyTuple [MuType]           -- ^ tuple type
         | MuTyApp   MuType MuType      -- ^ application of a type constructor
         | MuTyVar   MuName             -- ^ type variable
         | MuTyCon   MuQName            -- ^ named type or type constructor
  deriving (Eq,Show)

type MuContext = [MuAsst]

-- | Class assertions.
--   In Haskell 98, the argument would be a /tyvar/, but this definition
--   allows multiple parameters, and allows them to be /type/s.
type MuAsst    = (MuQName,[MuType])

-- | /literal/.
-- Values of this type hold the abstract value of the literal, not the
-- precise string representation used.  For example, @10@, @0o12@ and @0xa@
-- have the same representation.
data MuLiteral
        = MuChar        Char            -- ^ character literal
        | MuString      String          -- ^ string literal
        | MuInt         Integer         -- ^ integer literal
        | MuFrac        Rational        -- ^ floating point literal
        | MuCharPrim    Char            -- ^ GHC unboxed character literal
        | MuStringPrim  String          -- ^ GHC unboxed string literal
        | MuIntPrim     Integer         -- ^ GHC unboxed integer literal
        | MuFloatPrim   Rational        -- ^ GHC unboxed float literal
        | MuDoublePrim  Rational        -- ^ GHC unboxed double literal
  deriving (Eq,Show)

-- | Haskell expressions.
--
-- /Notes:/
--
-- * Because it is difficult for parsers to distinguish patterns from
--   expressions, they typically parse them in the same way and then check
--   that they have the appropriate form.  Hence the expression type
--   includes some forms that are found only in patterns.  After these
--   checks, these constructors should not be used.
--
-- * The parser does not take precedence and associativity into account,
--   so it will leave 'MuInfixApp's associated to the left.
--
-- * The 'Language.Haskell.Pretty.Pretty' instance for 'MuExp' does not
--   add parentheses in printing.

data MuExp
        = MuVar MuQName                 -- ^ variable
        | MuCon MuQName                 -- ^ data constructor
        | MuLit MuLiteral               -- ^ literal constant
        | MuInfixApp MuExp MuQOp MuExp  -- ^ infix application
        | MuApp MuExp MuExp             -- ^ ordinary application
        | MuNegApp MuExp                -- ^ negation expression @-@ /exp/
        | MuLambda [MuPat] MuExp -- ^ lambda expression
        | MuLet [MuDecl] MuExp          -- ^ local declarations with @let@
        | MuIf MuExp MuExp MuExp        -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
        | MuCase MuExp [MuAlt]          -- ^ @case@ /exp/ @of@ /alts/
        | MuDo [MuStmt]                 -- ^ @do@-expression:
                                        -- the last statement in the list
                                        -- should be an expression.
        | MuTuple [MuExp]               -- ^ tuple expression
        | MuList [MuExp]                -- ^ list expression
        | MuParen MuExp                 -- ^ parenthesized expression
        | MuLeftSection MuExp MuQOp     -- ^ left section @(@/exp/ /qop/@)@
        | MuRightSection MuQOp MuExp    -- ^ right section @(@/qop/ /exp/@)@
        | MuRecConstr MuQName [MuFieldUpdate]
                                        -- ^ record construction expression
        | MuRecUpdate MuExp [MuFieldUpdate]
                                        -- ^ record update expression
        | MuEnumFrom MuExp              -- ^ unbounded arithmetic sequence,
                                        -- incrementing by 1
        | MuEnumFromTo MuExp MuExp      -- ^ bounded arithmetic sequence,
                                        -- incrementing by 1
        | MuEnumFromThen MuExp MuExp    -- ^ unbounded arithmetic sequence,
                                        -- with first two elements given
        | MuEnumFromThenTo MuExp MuExp MuExp
                                        -- ^ bounded arithmetic sequence,
                                        -- with first two elements given
        | MuListComp MuExp [MuStmt]     -- ^ list comprehension
        | MuExpTypeSig MuExp MuQualType
                                        -- ^ expression type signature
        | MuAsPat MuName MuExp          -- ^ patterns only
        | MuWildCard                    -- ^ patterns only
        | MuIrrPat MuExp                -- ^ patterns only
  deriving (Eq,Show)

-- | A pattern, to be matched against a value.
data MuPat
        = MuPVar MuName                 -- ^ variable
        | MuPLit MuLiteral              -- ^ literal constant
        | MuPNeg MuPat                  -- ^ negated pattern
        | MuPInfixApp MuPat MuQName MuPat
                                        -- ^ pattern with infix data constructor
        | MuPApp MuQName [MuPat]        -- ^ data constructor and argument
                                        -- patterns
        | MuPTuple [MuPat]              -- ^ tuple pattern
        | MuPList [MuPat]               -- ^ list pattern
        | MuPParen MuPat                -- ^ parenthesized pattern
        | MuPRec MuQName [MuPatField]   -- ^ labelled pattern
        | MuPAsPat MuName MuPat         -- ^ @\@@-pattern
        | MuPWildCard                   -- ^ wildcard pattern (@_@)
        | MuPIrrPat MuPat               -- ^ irrefutable pattern (@~@)
  deriving (Eq,Show)

-- | An /fpat/ in a labeled record pattern.
data MuPatField
        = MuPFieldPat MuQName MuPat
  deriving (Eq,Show)

-- | This type represents both /stmt/ in a @do@-expression,
--   and /qual/ in a list comprehension.
data MuStmt
        = MuGenerator MuPat MuExp
                                -- ^ a generator /pat/ @<-@ /exp/
        | MuQualifier MuExp     -- ^ an /exp/ by itself: in a @do@-expression,
                                -- an action whose result is discarded;
                                -- in a list comprehension, a guard expression
        | MuLetStmt [MuDecl]    -- ^ local bindings
  deriving (Eq,Show)

-- | An /fbind/ in a labeled record construction or update expression.
data MuFieldUpdate
        = MuFieldUpdate MuQName MuExp
  deriving (Eq,Show)

-- | An /alt/ in a @case@ expression.
data MuAlt
        = MuAlt MuPat MuGuardedAlts [MuDecl]
  deriving (Eq,Show)

data MuGuardedAlts
        = MuUnGuardedAlt MuExp          -- ^ @->@ /exp/
        | MuGuardedAlts  [MuGuardedAlt] -- ^ /gdpat/
  deriving (Eq,Show)

-- | A guarded alternative @|@ /exp/ @->@ /exp/.
-- The first expression will be Boolean-valued.
data MuGuardedAlt
        = MuGuardedAlt MuExp MuExp
  deriving (Eq,Show)
