import DynFlags
import HsSyn
import Outputable
import qualified GHC

import AppendImport
import MyParser

import System.Environment

main :: IO ()
main = do
    dynFl <- GHC.runGhc (Just libdir) GHC.getSessionDynFlags
    [file] <- getArgs
    mp <- parseHaskell file
    case mp of
        Nothing  -> putStrLn "Parse failed"
        Just mdl -> writeFile "Test_rewrite.hs" $ showSDoc dynFl                                                
                                                $ ppr $ addImpAppl mdl
