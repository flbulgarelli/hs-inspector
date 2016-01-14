module Language.Haskell.Mu (
    MuProgram(..),
    MuDeclaration(..),
    MuEquation(..), MuRhs(..), MuGuardedRhs(..),
    MuExp(..), MuStmt(..),
    MuAlt(..), MuGuardedAlts(..), MuGuardedAlt(..),
    MuPat(..)
  ) where


data MuProgram = MuProgram [MuDeclaration] deriving (Show)

data MuDeclaration
         = MuTypeAlias    String
         | MuRecordDeclaration    String
         | MuTypeSignature     String
         | MuFunction  String [MuEquation]
         | MuConstant     String MuRhs [MuDeclaration]
  deriving (Eq,Show)

data MuEquation = MuEquation [MuPat] MuRhs [MuDeclaration] deriving (Eq,Show)

data MuRhs
         = MuUnGuardedRhs MuExp -- ^ unguarded right hand side (/exp/)
         | MuGuardedRhss  [MuGuardedRhs]
                                -- ^ guarded right hand side (/gdrhs/)
  deriving (Eq,Show)

data MuGuardedRhs
         = MuGuardedRhs MuExp MuExp
  deriving (Eq,Show)

data MuExp
        = MuVar String                 -- ^ variable
        | MuCon String                 -- ^ data constructor
        | MuLit String               -- ^ literal constant
        | MuInfixApp MuExp String MuExp  -- ^ infix application
        | MuApp MuExp MuExp             -- ^ ordinary application
        | MuLambda [MuPat] MuExp -- ^ lambda expression
        | MuLet [MuDeclaration] MuExp          -- ^ local declarations with @let@
        | MuIf MuExp MuExp MuExp        -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
        | MuCase MuExp [MuAlt]          -- ^ @case@ /exp/ @of@ /alts/
        | MuTuple [MuExp]               -- ^ tuple expression
        | MuList [MuExp]                -- ^ list expression
        | MuEnum MuExp (Maybe MuExp) (Maybe MuExp)
        | MuListComp MuExp [MuStmt]     -- ^ list comprehension
        | MuExpOther
  deriving (Eq,Show)

data MuPat
        = MuPVar String                 -- ^ variable
        | MuPLit String              -- ^ literal constant
        | MuPInfixApp MuPat String MuPat
        | MuPApp String [MuPat]        -- ^ data constructor and argument
        | MuPTuple [MuPat]              -- ^ tuple pattern
        | MuPList [MuPat]               -- ^ list pattern
        | MuPAsPat String MuPat         -- ^ @\@@-pattern
        | MuPWildCard                   -- ^ wildcard pattern (@_@)
        | MuPOther
  deriving (Eq,Show)

data MuStmt
        = MuGenerator MuPat MuExp
        | MuQualifier MuExp
        | MuLetStmt [MuDeclaration]
  deriving (Eq,Show)

data MuAlt = MuAlt MuPat MuGuardedAlts [MuDeclaration] deriving (Eq,Show)

data MuGuardedAlts
        = MuUnGuardedAlt MuExp          -- ^ @->@ /exp/
        | MuGuardedAlts  [MuGuardedAlt] -- ^ /gdpat/
  deriving (Eq,Show)

data MuGuardedAlt = MuGuardedAlt MuExp MuExp deriving (Eq,Show)
