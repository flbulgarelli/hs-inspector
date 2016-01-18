module Language.Haskell.Mu (
    MuProgram(..),
    MuDeclaration(..),
    MuEquation(..), MuRhs(..), MuGuardedRhs(..),
    MuExp(..), MuStmt(..),
    MuAlt(..), MuGuardedAlts(..), MuGuardedAlt(..),
    MuPat(..),
    MuLitValue(..)
  ) where


data MuProgram = MuProgram [MuDeclaration] deriving (Show)

type MuId = String


data MuDeclaration
         = MuTypeAlias MuId
         | MuRecordDeclaration MuId
         | MuTypeSignature MuId
         | MuFunction MuId [MuEquation]
         | MuConstant MuId MuRhs [MuDeclaration]
  deriving (Eq,Show)

data MuEquation = MuEquation [MuPat] MuRhs [MuDeclaration] deriving (Eq,Show)

data MuRhs
         = MuUnGuardedRhs MuExp
         | MuGuardedRhss  [MuGuardedRhs]
  deriving (Eq,Show)

data MuGuardedRhs = MuGuardedRhs MuExp MuExp deriving (Eq,Show)

data MuExp
        = MuVar MuId
        | MuLit MuLitValue
        | MuInfixApp MuExp String MuExp
        | MuApp MuExp MuExp
        | MuLambda [MuPat] MuExp
        | MuLet [MuDeclaration] MuExp          -- ^ local declarations with @let@
        | MuIf MuExp MuExp MuExp
        | MuCase MuExp [MuAlt]
        | MuTuple [MuExp]
        | MuList [MuExp]
        | MuListComp MuExp [MuStmt]
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

data MuLitValue
          = MuBool Bool
          | MuInteger Integer
          | MuFloat Rational
          | MuString String
    deriving (Eq,Show)