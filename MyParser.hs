module Parser where

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

main :: IO ()
main = do
	dynFl <- GHC.runGhc (Just libdir) GHC.getSessionDynFlags
	[file] <- getArgs
	mp <- parseHaskell file
	case mp of
		Nothing  -> putStrLn "Parse failed"
		Just mdl -> print $ map ((showSDoc dynFl). ppr) $ hsmodDecls mdl

libdir :: FilePath
libdir = "/opt/ghc/7.10.3/lib/ghc-7.10.2.20150906"

parseHaskell :: FilePath -> IO (Maybe (HsModule RdrName))
parseHaskell file = do
	initStaticOpts
	dynFl <- GHC.runGhc (Just libdir) GHC.getSessionDynFlags
	sbuf <- hGetStringBuffer file
	let srcloc = mkRealSrcLoc (mkFastString file) 1 1
	return $ case unP Parser.parseModule (mkPState dynFl sbuf srcloc) of
		POk _ (L _ mdl) -> Just mdl
		PFailed _ _     -> Nothing
