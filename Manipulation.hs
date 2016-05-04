module Manipulation where

import HsDecls
import HsTypes
import HsBinds
import RdrName
import FastString
import Outputable
import SrcLoc

unMaybe :: Maybe a -> a
unMaybe (Just b) = b

takeFirstArg :: HsType RdrName -> HsType RdrName
takeFirstArg (HsAppTy a b) = unLoc a

takeSecondArg :: HsType RdrName -> HsType RdrName
takeSecondArg (HsAppTy a b) = unLoc b

putOutInnerHsType :: HsType RdrName -> HsType RdrName
putOutInnerHsType (HsForAllTy a b c d e) = unLoc e

hsTypeToString :: HsType RdrName -> String
hsTypeToString = showSDocUnsafe . ppr

mkRdrName :: String -> RdrName
mkRdrName = mkVarUnqual . fsLit