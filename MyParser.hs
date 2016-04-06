module MyParser where

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


libdir :: FilePath
libdir = "/opt/ghc/7.10.3/lib/ghc-7.10.3"

parseHaskell :: FilePath -> IO (Maybe (HsModule RdrName))
parseHaskell file = do
    initStaticOpts
    dynFl <- GHC.runGhc (Just libdir) GHC.getSessionDynFlags
    sbuf <- hGetStringBuffer file
    let srcloc = mkRealSrcLoc (mkFastString file) 1 1
    return $ case unP Parser.parseModule (mkPState dynFl sbuf srcloc) of
        POk _ (L _ mdl) -> Just mdl
        PFailed _ _     -> Nothing

parseToLHsExpr :: String -> IO (Maybe (HsExpr RdrName))
parseToLHsExpr expr = do
    initStaticOpts
    dynFl <- GHC.runGhc (Just libdir) GHC.getSessionDynFlags 
    let srcloc = mkRealSrcLoc (mkFastString "log.hs") 1 1
    return $ case unP Parser.parseExpression (mkPState dynFl 
                                  (stringToStringBuffer expr) noSrcLoc) of
        POk _ (L _ hsexpr) -> Just hsexpr
        PFailed _ _        -> Nothing

showParsedString :: IO (Maybe (HsExpr RdrName)) -> IO String
showParsedString pstr = do
    str <- pstr
    case str of
        Just res -> return $ showSDocUnsafe $ ppr res
        Nothing  -> return $ "Nothing(((("
