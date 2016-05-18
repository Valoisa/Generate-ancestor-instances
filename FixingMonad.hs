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
import HsTypes

import AppendInstance
import MyParser

import System.Environment

main :: IO ()
main = do
    dynFl <- GHC.runGhc (Just libdir) GHC.getSessionDynFlags
    [file] <- getArgs
    mp <- appendAllInstances $ parseHaskell file
    case mp of
        Nothing  -> putStrLn "Parse failed"
        Just mdl -> writeFile "Instances7.10.hs" $ showSDocUnsafe 
                                                 $ ppr mdl
