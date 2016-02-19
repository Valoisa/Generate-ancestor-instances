-- Adds an import of Control.Applicative
import AppendImport
import MyParser

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

main = do 
    dynFl <- GHC.runGhc (Just libdir) GHC.getSessionDynFlags
    [file] <- getArgs
    mp <- getImportDecls $ getLImortDecls $ parseHaskell file
    case mp of
        Nothing  -> putStrLn "Parse failed"
        Just mdl -> createNewFile (map ((++) "import ")
                            $ appendImport 
                            $ getModulesNames mdl) "Test-7.10.hs"

createNewFile :: [String] -> FilePath -> IO()
createNewFile xs file = do
    writeFile file $ unlines xs
