module Main where

import SimpNode
import SimpfsEncoder
import SimpfsDecoder

import Control.Monad
    ((<$!>), liftM)
import Data.ByteString.Lazy as BS
    (readFile, writeFile)
import System.Directory
    (makeRelativeToCurrentDirectory)
import System.Environment
    (getArgs)
import Data.Tree (Tree)

main = (\args ->

    case length args of
        0   -> printUsage

        1   -> printUsage

        2   -> case isSwitch (args !! 0) of
                True    -> printUsage

                False   -> case args !! 1 of
                            "-l"    -> printTree =<< simpfsMakeTree
                                  <$!> BS.readFile (args !! 0)

                            _       -> printUsage

        3   -> case isSwitch (args !! 0) of
                True    -> printUsage

                False   -> case args !! 1 of
                            "-a"    -> BS.writeFile (args !! 2)
                                   =<< simpfsEncode
                                   =<< makeRelativeToCurrentDirectory
                                       (args !! 0)

                            "-i"    -> print
                                   =<< (simpfsPeek (args !! 2))
                                  <$!> simpfsDecode
                                  <$!> BS.readFile (args !! 0)

                            _       -> printUsage
    ) =<< getArgs

isSwitch :: String -> Bool
isSwitch "" = False
isSwitch [x] = False
isSwitch (x:xs) = x == '-'

printUsage :: IO ()
printUsage = putStrLn $ "simpfs [ <archive> -l\n"
                     ++ "       | <folder> -a <archive>\n"
                     ++ "       | <archive> -i <path/to/node>\n"
                     ++ "       | -h ]"

printTree :: Maybe (Tree SimpNode) -> IO ()
printTree Nothing = putStrLn "No tree was generated."
printTree (Just tree) = putStrLn $ simpfsDrawTree tree


