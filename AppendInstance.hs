module AppendInstance where

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

import System.Environment

getHsModDecls :: IO (Maybe (HsModule RdrName)) -> IO (Maybe [LHsDecl RdrName])
getHsModDecls md = do
	mp <- md
	case mp of
		Nothing  -> return Nothing
		Just mdl -> return $ Just (hsmodDecls mdl)

getHsDecls :: IO (Maybe [LHsDecl RdrName]) -> IO (Maybe [HsDecl RdrName])
getHsDecls md = do
	mp <- md
	case mp of
		Nothing -> return Nothing
		Just mdl -> return $ Just (map unLoc mdl)

getOneInstDecl :: HsDecl RdrName -> InstDecl RdrName
getOneInstDecl a = case a of
	InstD mdl -> mdl

getInstDecls :: IO (Maybe [HsDecl RdrName]) -> IO (Maybe [InstDecl RdrName])
getInstDecls md = do
	mp <- md
	case mp of 
		Nothing -> return Nothing
		Just mdl -> return $ Just (map getOneInstDecl mdl)
