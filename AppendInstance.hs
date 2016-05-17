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
import BasicTypes
import TcEvidence

import Data.List

import HsDecls
import HsTypes
import HsBinds

import MyParser
import Manipulation

import System.Environment

mkFunId :: String -> Located RdrName
mkFunId = noLoc . mkRdrName

applicative :: HsType RdrName
applicative = HsTyVar (mkRdrName "Applicative")

functor :: HsType RdrName
functor = HsTyVar (mkRdrName "Functor")

monad :: HsType RdrName
monad = HsTyVar (mkRdrName "Monad")

deflHsTyVarBndrs :: LHsTyVarBndrs RdrName
deflHsTyVarBndrs = HsQTvs { hsq_kvs = []
                          , hsq_tvs = [] }

mkInstHead :: (HsType RdrName) -> (HsType RdrName) -> (LHsType RdrName)
mkInstHead classname mydata = noLoc (HsForAllTy Explicit Nothing 
                    deflHsTyVarBndrs (noLoc [])
                    (noLoc (HsAppTy (noLoc classname) 
                    (noLoc mydata))))

mkInstance :: LHsType RdrName -> LHsBinds RdrName
                                            -> ClsInstDecl RdrName
mkInstance insthead funs = ClsInstDecl { 
                                        cid_poly_ty       = insthead
                                      , cid_binds         = funs
                                      , cid_sigs          = []
                                      , cid_tyfam_insts   = []
                                      , cid_datafam_insts = []
                                      , cid_overlap_mode  = Nothing
                                            }

mkGRHSs :: String -> GRHSs RdrName (LHsExpr RdrName)
mkGRHSs rhs = GRHSs { grhssGRHSs      = [noLoc 
                                $ GRHS [] (unMaybe $ parseToLHsExpr rhs)]
                    , grhssLocalBinds = EmptyLocalBinds }

mkLMatch ::  [LPat RdrName] -> GRHSs RdrName (LHsExpr RdrName)
                                    -> LMatch RdrName (LHsExpr RdrName)
mkLMatch mpats mgrhss = noLoc $ Match { m_fun_id_infix = Nothing
                                      , m_pats         = mpats
                                      , m_type         = Nothing
                                      , m_grhss        = mgrhss }

mkMyMatchGroup :: LMatch RdrName (LHsExpr RdrName)
                                -> MatchGroup RdrName (LHsExpr RdrName)
mkMyMatchGroup lmatch = MG { mg_alts    = [lmatch]
                           , mg_arg_tys = []
                           , mg_res_ty  = placeHolderType
                           , mg_origin  = FromSource }

{- mkFunBind :: String -> String -> [LPat RdrName]
                          -> Bool -> HsBindLR RdrName RdrName
mkFunBind idL idR lpats inf = FunBind { fun_id      = mkFunId idL
                                      , fun_infix   = inf
                                      , fun_matches = mkMyMatchGroup
                                                $ mkLMatch lpats 
                                                $ mkGRHSs idR
                                      , fun_co_fn   = WpHole
                                      , bind_fvs    = placeHolderType
                                      , fun_tick    = [] } -}

mkCIDBindsForInstMonad :: [LHsBindLR RdrName RdrName]
                                            -> LHsBinds RdrName
mkCIDBindsForInstMonad userbind = listToBag $ (++)
                   [ noLoc (mkFunBind (mkFunId "return") ([mkLMatch [] 
                                                $ mkGRHSs "pure"]))
                   , noLoc (mkFunBind (mkFunId "(>>)") ([mkLMatch [] 
                                                $ mkGRHSs "(*>)"])) ]
                    userbind

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

isFunBind :: HsBindLR RdrName RdrName -> Bool
isFunBind (FunBind _ _ _ _ _ _) = True
isFunBind _                     = False

whatClassIsInstance :: ClsInstDecl RdrName -> HsType RdrName
whatClassIsInstance = takeFirstArg . putOutInnerHsType 
                        . unLoc . cid_poly_ty

takeUserData :: ClsInstDecl RdrName -> HsType RdrName
takeUserData = takeSecondArg . putOutInnerHsType 
                        . unLoc . cid_poly_ty

takeUserBind :: ClsInstDecl RdrName -> [LHsBindLR RdrName RdrName]
takeUserBind = (map noLoc) . (filter (filterFun ">>=")) 
            . filterFunBinds

takeUserReturn :: ClsInstDecl RdrName -> [LHsBindLR RdrName RdrName]
takeUserReturn = (map noLoc) . (filter (filterFun "return")) 
            . filterFunBinds

takeUserGrGr :: ClsInstDecl RdrName -> [LHsBindLR RdrName RdrName]
takeUserGrGr = (map noLoc) . (filter (filterFun ">>")) 
            . filterFunBinds

filterFun :: String -> HsBindLR RdrName RdrName -> Bool
filterFun fun bind = (rdrName2String . unLoc . fun_id) bind == fun

filterFunBinds :: ClsInstDecl RdrName
                                    -> [HsBindLR RdrName RdrName]
filterFunBinds = (filter isFunBind) 
            . (map unLoc) . bagToList . cid_binds

isInstanceMonad :: ClsInstDecl RdrName -> Bool
isInstanceMonad instD = 
    ((hsTypeToString . whatClassIsInstance) instD) == "Monad"

isInstanceApplicative :: ClsInstDecl RdrName -> Bool
isInstanceApplicative instD = 
   ((hsTypeToString . whatClassIsInstance) instD) == "Applicative"

isInstanceFunctor :: ClsInstDecl RdrName -> Bool
isInstanceFunctor instD = 
    ((hsTypeToString . whatClassIsInstance) instD) == "Functor"

getInstsMonad :: IO (Maybe [ClsInstDecl RdrName])
                                    -> IO (Maybe [ClsInstDecl RdrName])
getInstsMonad md = do
    mp <- md
    case mp of
        Nothing  -> return Nothing
        Just mdl -> return $ Just (filter isInstanceMonad mdl)

getInstsApplicative :: IO (Maybe [ClsInstDecl RdrName])
                                    -> IO (Maybe [ClsInstDecl RdrName])
getInstsApplicative md = do
    mp <- md
    case mp of
        Nothing  -> return Nothing
        Just mdl -> return $ Just (filter isInstanceApplicative mdl)

getInstsFunctor :: IO (Maybe [ClsInstDecl RdrName])
                                    -> IO (Maybe [ClsInstDecl RdrName])
getInstsFunctor md = do
    mp <- md
    case mp of
        Nothing  -> return Nothing
        Just mdl -> return $ Just (filter isInstanceFunctor mdl)

-- "Pulls out" HsType (is contained in ClsInstDecl)
getHsType :: IO (Maybe [ClsInstDecl RdrName])
                                        -> IO (Maybe [HsType RdrName])
getHsType md = do
    mp <- md
    case mp of
        Nothing  -> return Nothing
        Just mdl -> return $ Just (map (unLoc . cid_poly_ty) mdl)


-- "Pulls" the HsBinds out of ClsInstDecl
getHsBinds :: IO (Maybe [ClsInstDecl RdrName])
                           -> IO (Maybe [[HsBindLR RdrName RdrName]])
getHsBinds md = do
    mp <- md
    case mp of
        Nothing  -> return Nothing
        Just mdl -> return $ Just (map ((map unLoc) . bagToList 
                                                . cid_binds) mdl)

--Adding instances
fixInstancesMonad :: IO (Maybe (HsModule RdrName))
                                -> IO (Maybe [ClsInstDecl RdrName])
fixInstancesMonad md = do
    mp <-  getInstsMonad
           $ getClsInstDecl $ getInstDecls 
           $ getHsDecls $ getHsModDecls md
    return $ case mp of 
        Nothing  -> Nothing
        Just mdl -> Just (map (\x -> 
                mkInstance (mkInstHead monad (takeUserData x)) 
                    (mkCIDBindsForInstMonad $ takeUserBind x)) mdl)

--addInstanceApplicative
