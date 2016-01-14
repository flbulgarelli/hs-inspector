module Language.Haskell.Mu (
    MuModule(..),
    MuAssoc(..),
    -- * Declarations
    MuDecl(..), MuConDecl(..), MuBangType(..),
    MuMatch(..), MuRhs(..), MuGuardedRhs(..),
    -- * Class Assertions and Contexts
    MuQualType(..),
    -- * Types
    MuType(..),
    -- * Expressions
    MuExp(..), MuStmt(..),
    MuAlt(..), MuGuardedAlts(..), MuGuardedAlt(..),
    -- * Patterns
    MuPat(..), MuPatField(..),
    -- * Literals
    MuLiteral(..),
    -- * Variables, Constructors and Operators
    MuQName(..), MuName(..), MuQOp(..), MuOp(..),
    MuSpecialCon(..),
  ) where


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
        = Qual String MuName    -- ^ name qualified with a module name
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

-- | A Haskell source module.
data MuModule = MuModule String [MuDecl]
  deriving (Show)

-- | Associativity of an operator.
data MuAssoc
         = MuAssocNone  -- ^ non-associative operator (declared with @infix@)
         | MuAssocLeft  -- ^ left-associative operator (declared with @infixl@).
         | MuAssocRight -- ^ right-associative operator (declared with @infixr@)
  deriving (Eq,Show)

data MuDecl
         = MuTypeDecl    MuName [MuName] MuType
         | MuDataDecl    MuName [MuName] [MuConDecl] [MuQName]
         | MuInfixDecl   MuAssoc Int [MuOp]
         | MuTypeSig     [MuName] MuQualType
         | MuFunBind     [MuMatch]
         | MuPatBind     MuPat MuRhs {-where-} [MuDecl]
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

-- | A type qualified with a context.
--   An unqualified type has an empty context.
data MuQualType
         = MuQualType MuType
  deriving (Eq,Show)

-- | Haskell types and type constructors.
data MuType
         = MuTyFun   MuType MuType      -- ^ function type
         | MuTyTuple [MuType]           -- ^ tuple type
         | MuTyApp   MuType MuType      -- ^ application of a type constructor
         | MuTyVar   MuName             -- ^ type variable
         | MuTyCon   MuQName            -- ^ named type or type constructor
  deriving (Eq,Show)

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
        | MuPInfixApp MuPat MuQName MuPat
                                        -- ^ pattern with infix data constructor
        | MuPApp MuQName [MuPat]        -- ^ data constructor and argument
                                        -- patterns
        | MuPTuple [MuPat]              -- ^ tuple pattern
        | MuPList [MuPat]               -- ^ list pattern
        | MuPParen MuPat                -- ^ parenthesized pattern
        | MuPAsPat MuName MuPat         -- ^ @\@@-pattern
        | MuPWildCard                   -- ^ wildcard pattern (@_@)
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
