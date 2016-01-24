module Language.Haskell.Names (
  isName,
  declName) where

import  Language.Haskell.Mu

isName :: String -> String -> Bool
isName n hsName = hsName == n

declName :: Declaration -> String
declName (TypeSignature b ) = b
declName (TypeAlias b ) = b
declName (ConstantDeclaration n _ _) = n
declName (FunctionDeclaration n cases)  = n
declName _                  = []

