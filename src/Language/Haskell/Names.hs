module Language.Haskell.Names (
  isName,
  qName,
  declName) where

import  Language.Haskell.Mu

isName :: String -> String -> Bool
isName n hsName = hsName == n

qName :: MuQName -> Maybe String
qName (Qual  _ hsName) = Just (hsName)
qName (UnQual  hsName) = Just (hsName)
qName _                = Nothing

declName :: MuDecl -> String
declName (MuTypeSig b ) = b
declName (MuTypeDecl b ) = b
declName (MuPatBind (MuPVar n) _ _) = n
declName (MuFunBind cases)  | (MuMatch n _ _ _) <- head cases = n
declName _                  = []

