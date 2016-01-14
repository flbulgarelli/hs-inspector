module Language.Haskell.Names (
  isName,
  name,
  qName,
  declName) where

import  Language.Haskell.Mu

isName :: String -> MuName -> Bool
isName n hsName = name hsName == n

name :: MuName -> String
name (MuSymbol n) = n
name (MuIdent  n) = n

qName :: MuQName -> Maybe String
qName (Qual  _ hsName) = Just (name hsName)
qName (UnQual  hsName) = Just (name hsName)
qName _                = Nothing

declName :: MuDecl -> String
declName (MuTypeSig _ [b] _) = name b
declName (MuTypeDecl _ b _ _) = name b
declName (MuPatBind _ (MuPVar n) _ _) = name n
declName (MuFunBind cases)  | (MuMatch _ n _ _ _) <- head cases = name n
declName _                  = []

