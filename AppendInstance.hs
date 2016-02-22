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
import Bag
import Name

import Data.List

import HsDecls
import HsTypes
import HsBinds

import System.Environment

-- "Pulls out" the declarations from the module 
-- (that are inside of Located)
getHsModDecls :: IO (Maybe (HsModule RdrName)) 
                                    -> IO (Maybe [LHsDecl RdrName])
getHsModDecls md = do
    mp <- md
    case mp of
        Nothing  -> return Nothing
        Just mdl -> return $ Just (hsmodDecls mdl)

-- "Pulls out" the declarations from Located
getHsDecls :: IO (Maybe [LHsDecl RdrName]) 
                            -> IO (Maybe [HsDecl RdrName])
getHsDecls md = do
    mp <- md
    case mp of
        Nothing  -> return Nothing
        Just mdl -> return $ Just (map unLoc mdl)

-- This is necessary for the next fuction (getInstDecls)
isInstDecl :: HsDecl RdrName -> Bool
isInstDecl (InstD _) = True
isInstDecl _         = False

getOneInstDecl :: HsDecl RdrName -> InstDecl RdrName
getOneInstDecl (InstD (a)) = a

-- "Pulls out" the Instance declarations
getInstDecls :: IO (Maybe [HsDecl RdrName]) 
                                    -> IO (Maybe [InstDecl RdrName])
getInstDecls md = do
    mp <- md
    case mp of 
        Nothing  -> return Nothing
        Just mdl -> return $ Just (map getOneInstDecl
                                            $ filter isInstDecl mdl)

-- "Pulls out" the class instance declarations
getClsInstDecl :: IO (Maybe [InstDecl RdrName]) 
                                    -> IO (Maybe [ClsInstDecl RdrName])
getClsInstDecl md = do
    mp <- md
    case mp of
        Nothing  -> return Nothing
        Just mdl -> return $ Just (map cid_inst 
                                            $ filter isClsInstDecl mdl)

-- Checks if it is a class instance declaration
isClsInstDecl :: InstDecl RdrName -> Bool
isClsInstDecl (ClsInstD _ ) = True
isClsInstDecl _             = False

-- "Pulls out" HsType (is contained in ClsInstDecl)
getHsType :: IO (Maybe [ClsInstDecl RdrName])
                                        -> IO (Maybe [String])
getHsType md = do
    mp <- md
    case mp of
        Nothing  -> return Nothing
        Just mdl -> return $ Just (map (deleteBrackets
                                    . showSDocUnsafe . pprParendHsType
                                    . unLoc . cid_poly_ty) mdl)

-- HsType is shown up inside of the brackets; 
-- it is necessary to delete them
deleteBrackets :: String -> String
deleteBrackets = init . tail

-- "Pulls" the HsBinds out of ClsInstDecl
getHsBinds :: IO (Maybe [ClsInstDecl RdrName])
                           -> IO (Maybe [[HsBindLR RdrName RdrName]])
getHsBinds md = do
    mp <- md
    case mp of
        Nothing  -> return Nothing
        Just mdl -> return $ Just (map ((map unLoc) . bagToList 
                                                . cid_binds) mdl)

-- "Pulls" only the FunBinds' ids out of the HsBindLR
isFunBind :: HsBindLR RdrName RdrName -> Bool
isFunBind (FunBind _ _ _ _ _ _) = True
isFunBind _                     = False

getFunBindsIds :: IO (Maybe [[HsBindLR RdrName RdrName]])
                    -> IO (Maybe [[String]])
getFunBindsIds md = do
    mp <- md
    case mp of
        Nothing  -> return Nothing
        Just mdl -> return $ Just (map ((map (rdrName2String . unLoc
                                        . fun_id))
                                        . filter isFunBind) mdl)

-- Shows the names of the functions that were described
-- inside of the Instance Declarations
rdrName2String :: RdrName -> String
rdrName2String nm = showSDocUnsafe $ pprOccName $ rdrNameOcc nm

getFunExprs :: IO (Maybe [[HsBindLR RdrName RdrName]])
                    -> IO (Maybe [[String]])
getFunExprs md = undefined {-do
    mp <- md
    case mp of 
        Nohing   -> return Nothing
        Just mdl -> return $ Just (map ((map (rdrName2String . unLoc
                                        . fun_matchess))
                                        . filter isFunBind) mdl)-}

getOneLHsExpr :: MatchGroup RdrName (LHsExpr RdrName) -> LHsExpr RdrName
getOneLHsExpr (MG _ _ _ _) = undefined

getFunDeclTypes :: IO (Maybe [[HsBindLR RdrName RdrName]])
                    -> IO (Maybe [[Bool]])
getFunDeclTypes md = do
    mp <- md
    case mp of
        Nothing  -> return Nothing
        Just mdl -> return $ Just (map ((map fun_infix)
                                            .filter isFunBind) mdl)

-- The following functions are the superpositions
-- of the previous funtions
getListOfHsTypes :: IO (Maybe (HsModule RdrName)) -> IO (Maybe [String])
getListOfHsTypes = getHsType . getClsInstDecl . getInstDecls
                             . getHsDecls . getHsModDecls

getListsOfFunNames :: IO (Maybe (HsModule RdrName)) 
                                    -> IO (Maybe [[String]])
getListsOfFunNames = getFunBindsIds . getHsBinds . getClsInstDecl 
                        . getInstDecls . getHsDecls . getHsModDecls
                        
getListsOfFunDeclTypes :: IO (Maybe (HsModule RdrName)) 
                                    -> IO (Maybe [[Bool]])
getListsOfFunDeclTypes = getFunDeclTypes . getHsBinds . getClsInstDecl 
                        . getInstDecls . getHsDecls . getHsModDecls
