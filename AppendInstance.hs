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
import AppendImport

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

mkFunBindForAppl :: String -> HsBindLR RdrName RdrName
                          -> Bool -> HsBindLR RdrName RdrName
mkFunBindForAppl idL monbind inf = FunBind { 
                                        fun_id      = mkFunId idL
                                      , fun_infix   = inf
                                      , fun_matches = fun_matches monbind
                                      , fun_co_fn   = fun_co_fn monbind
                                      , bind_fvs    = bind_fvs monbind
                                      , fun_tick    = fun_tick monbind
                                       }

mkCIDBindsForInstMonad :: [LHsBindLR RdrName RdrName]
                                            -> LHsBinds RdrName
mkCIDBindsForInstMonad userbind = listToBag $ (++)
                   [ noLoc (mkFunBind (mkFunId "return") ([mkLMatch [] 
                                                $ mkGRHSs "pure"]))
                   , noLoc (mkFunBind (mkFunId "(>>)") ([mkLMatch [] 
                                                $ mkGRHSs "(*>)"])) ]
                    userbind

mkCIDBindsForInstAppl :: [HsBindLR RdrName RdrName]
                -> [HsBindLR RdrName RdrName] -> LHsBinds RdrName
mkCIDBindsForInstAppl [] [] = listToBag 
                    [ noLoc (mkFunBind (mkFunId "(<*>)") ([mkLMatch [] 
                                                $ mkGRHSs "ap"])) ]
mkCIDBindsForInstAppl [] mongr = listToBag
                    [ noLoc (mkFunBind (mkFunId "(<*>)") ([mkLMatch [] 
                                                $ mkGRHSs "ap"]))
                    , noLoc (mkFunBindForAppl "*>" (head mongr) True) ]
mkCIDBindsForInstAppl monret [] = listToBag
                    [ noLoc (mkFunBindForAppl "pure" (head monret) False)
                    , noLoc (mkFunBind (mkFunId "(<*>)") ([mkLMatch [] 
                                                $ mkGRHSs "ap"])) ]
mkCIDBindsForInstAppl monret mongr = listToBag
                    [ noLoc (mkFunBindForAppl "pure" (head monret) False)
                    , noLoc (mkFunBind (mkFunId "(<*>)") ([mkLMatch [] 
                                                $ mkGRHSs "ap"]))
                    , noLoc (mkFunBindForAppl "*>" (head mongr) True) ]

mkCIDBindsForInstFunc :: LHsBinds RdrName
mkCIDBindsForInstFunc = listToBag
                    [ noLoc (mkFunBind (mkFunId "fmap") ([mkLMatch [] 
                                                $ mkGRHSs "liftM"])) ]

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

takeUserReturn :: ClsInstDecl RdrName -> [HsBindLR RdrName RdrName]
takeUserReturn = (filter (filterFun "return")) . filterFunBinds

takeUserGrGr :: ClsInstDecl RdrName -> [HsBindLR RdrName RdrName]
takeUserGrGr = (filter (filterFun ">>")) . filterFunBinds

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

filterAloneInstMon :: Maybe [ClsInstDecl RdrName]
                                    -> Maybe [ClsInstDecl RdrName]
                                    -> Maybe [ClsInstDecl RdrName]
filterAloneInstMon mpap md = 
    case md of
        Nothing  -> Nothing
        Just mdl -> Just (filter 
                                (hasntApprInst (unMaybeList mpap)) mdl)

hasntApprInst :: [ClsInstDecl RdrName] -> ClsInstDecl RdrName -> Bool
hasntApprInst xs x = not $ elem (showSDocUnsafe $ ppr $ takeUserData x) 
                  (map (showSDocUnsafe . ppr . takeUserData) xs)

hasApprInst :: [ClsInstDecl RdrName] -> ClsInstDecl RdrName -> Bool
hasApprInst xs x = elem (showSDocUnsafe $ ppr $ takeUserData x) 
                  (map (showSDocUnsafe . ppr . takeUserData) xs)

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
fixInstancesMonad :: Maybe [ClsInstDecl RdrName]
                                -> Maybe [ClsInstDecl RdrName]
fixInstancesMonad mp = 
    case mp of 
        Nothing  -> Nothing
        Just mdl -> Just (map (\x -> 
                mkInstance (mkInstHead monad (takeUserData x)) 
                    (mkCIDBindsForInstMonad $ takeUserBind x)) mdl)

addInstanceApplicative :: Maybe [ClsInstDecl RdrName]
                                -> Maybe [ClsInstDecl RdrName]
addInstanceApplicative mp = 
    case mp of 
        Nothing  -> Nothing
        Just mdl -> Just (map (\x -> 
                mkInstance (mkInstHead applicative (takeUserData x)) 
                    (mkCIDBindsForInstAppl (takeUserReturn x)
                                                (takeUserGrGr x))) mdl)

addInstanceFunctor :: Maybe [ClsInstDecl RdrName]
                                -> Maybe [ClsInstDecl RdrName]
addInstanceFunctor mp = 
    case mp of 
        Nothing  -> Nothing
        Just mdl -> Just (map (\x -> 
                mkInstance (mkInstHead functor (takeUserData x)) 
                    mkCIDBindsForInstFunc) mdl)

cutInstanceMonad :: IO (Maybe (HsModule RdrName))
                                -> IO (Maybe (HsModule RdrName))
cutInstanceMonad md = do
    mp <- md
    appls <- getInstsApplicative
           $ getClsInstDecl $ getInstDecls 
           $ getHsDecls $ getHsModDecls md
    return $
        case mp of
            Nothing  -> Nothing
            Just mdl -> Just (HsModule {
            hsmodName = hsmodName mdl
          , hsmodExports = hsmodExports mdl
          , hsmodImports = hsmodImports mdl
          , hsmodDecls = filter (notInstMon (unMaybeList appls)) 
                                                    $ hsmodDecls mdl
          , hsmodDeprecMessage = hsmodDeprecMessage mdl
          , hsmodHaddockModHeader = hsmodHaddockModHeader mdl })
          where
            notInstMon :: [ClsInstDecl RdrName] -> LHsDecl RdrName -> Bool
            notInstMon xs ldecl = 
                let decl = unLoc ldecl in
                case decl of
                  InstD a -> case a of
                               ClsInstD b -> case isInstanceMonad b of
                                               True  -> case hasntApprInst xs b of
                                                          False -> True
                                                          True  -> False
                                               False -> True
                               _          -> True
                  _       -> True
                    
                

appendAllInstances :: IO (Maybe (HsModule RdrName))
                                -> IO (Maybe (HsModule RdrName))
appendAllInstances md = do
    mp <- cutInstanceMonad md
    appls <- getInstsApplicative
           $ getClsInstDecl $ getInstDecls 
           $ getHsDecls $ getHsModDecls md
    funcs <- getInstsFunctor
           $ getClsInstDecl $ getInstDecls 
           $ getHsDecls $ getHsModDecls md
    monads <- getInstsMonad
           $ getClsInstDecl $ getInstDecls 
           $ getHsDecls $ getHsModDecls md
    return $ case mp of 
        Nothing  -> Nothing
        Just mdl -> Just (HsModule {
            hsmodName = hsmodName mdl
          , hsmodExports = hsmodExports mdl
          , hsmodImports = hsmodImports mdl ++ [ importFunctor
                           , importApplicative
                           , importMonad ]
          , hsmodDecls = hsmodDecls mdl
               ++ (map noLoc $ map (InstD . ClsInstD)
                 ((unMaybeList $ addInstanceApplicative 
                               $ filterAloneInstMon appls monads)
               ++ (unMaybeList $ addInstanceFunctor 
                               $ filterAloneInstMon funcs monads)
               ++ (unMaybeList $ fixInstancesMonad 
                               $ filterAloneInstMon appls monads)))
          , hsmodDeprecMessage = hsmodDeprecMessage mdl
          , hsmodHaddockModHeader = hsmodHaddockModHeader mdl }) 
