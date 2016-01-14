module Language.Haskell.Names (
  isName,
  declName) where

import  Language.Haskell.Mu

isName :: String -> String -> Bool
isName n hsName = hsName == n

declName :: MuDeclaration -> String
declName (MuTypeSignature b ) = b
declName (MuTypeAlias b ) = b
declName (MuConstant n _ _) = n
declName (MuFunction n cases)  = n
declName _                  = []

