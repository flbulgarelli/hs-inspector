module Language.Haskell.Names (
  isName,
  name,
  declName) where

import  Language.Haskell.Syntax

isName :: String -> HsName -> Bool
isName n hsName = name hsName == n

name :: HsName -> String
name (HsSymbol n) = n
name (HsIdent  n) = n

declName :: HsDecl -> String
declName (HsTypeSig _ [b] _) = name b
declName (HsTypeDecl _ b _ _) = name b
declName (HsPatBind _ (HsPVar n) _ _) = name n
declName (HsFunBind cases)  | (HsMatch _ n _ _ _) <- head cases = name n
declName _                  = []

