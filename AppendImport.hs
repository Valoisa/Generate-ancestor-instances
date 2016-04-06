module AppendImport where

import DynFlags
import FastString
import HsSyn
import Lexer
import Outputable
import qualified Parser
import RdrName
import SrcLoc
import StaticFlags
import StringBuffer
import qualified GHC
import HsDecls
import Module
import BasicTypes

import Data.List

import System.Environment

addImpAppl :: HsModule RdrName -> HsModule RdrName
addImpAppl (HsModule name exps imps decls depmes hadmodhead) 
    = (HsModule name exps (importApplicative : imps) decls 
                                                    depmes hadmodhead)

importApplicative :: LImportDecl RdrName
importApplicative = noLoc $ ImportDecl { ideclSourceSrc = Nothing
                                       , ideclName      = noLoc 
                                $ mkModuleName "Control.Applicative"
                                       , ideclPkgQual   = Nothing
                                       , ideclSource    = False
                                       , ideclSafe      = False
                                       , ideclQualified = False
                                       , ideclImplicit  = False
                                       , ideclAs        = Nothing
                                       , ideclHiding    = Nothing } 
