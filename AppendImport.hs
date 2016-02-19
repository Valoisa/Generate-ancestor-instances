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

import Data.List

import System.Environment

getLImortDecls :: IO (Maybe (HsModule RdrName)) -> IO (Maybe [LImportDecl RdrName])
getLImortDecls md = do
	mp <- md
	case mp of
		Nothing  -> return Nothing
		Just mdl -> return $ Just (hsmodImports mdl)


getImportDecls :: IO (Maybe [LImportDecl RdrName]) -> IO (Maybe [ImportDecl RdrName])
getImportDecls md = do
	mp <- md
	case mp of
		Nothing -> return Nothing
		Just mdl -> return $ Just (map unLoc mdl)

{-getOneImportDecl :: HsDecl RdrName -> ImportDecl RdrName
getOneImportDecl a = case a of
	ImportDecl mdl -> a-}

getModulesNames :: [ImportDecl RdrName] -> [String]
getModulesNames = map (moduleNameString.unLoc.ideclName)

applicIsImported :: [String] -> Bool
applicIsImported xs = "Control.Applicative" `elem` xs
