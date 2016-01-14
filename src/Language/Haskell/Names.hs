module Language.Haskell.Names (
  isName,
  declName) where

import  Language.Haskell.Mu

isName :: String -> String -> Bool
isName n hsName = hsName == n

declName :: MuDecl -> String
declName (MuTypeSig b ) = b
declName (MuTypeDecl b ) = b
declName (MuPatBind n _ _) = n
declName (MuFunBind cases)  | (MuMatch n _ _ _ ) <- head cases = n
declName _                  = []

