module Language.Haskell.Names (isName, nameOf) where

import  Language.Haskell.Syntax

isName :: String -> HsName -> Bool
isName name hsName = nameOf hsName == name

nameOf :: HsName -> String
nameOf (HsSymbol n) = n
nameOf (HsIdent  n) = n
