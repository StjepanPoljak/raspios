module SimpfsEncoder (simpfsEncode) where

import SimpNode

import Data.Binary
    (encode)
import Data.Tree
    (Tree(Node), flatten)
import qualified Data.ByteString.Lazy as BS
    (empty, readFile, length, append, ByteString)
import System.Directory
    (listDirectory, doesDirectoryExist)
import Control.Monad
    (foldM, (<$!>))
import System.FilePath
    (joinPath, takeFileName)

simpfsEncode :: FilePath -> IO BS.ByteString
simpfsEncode fp = foldl (\xs x -> BS.append xs (encode x)) BS.empty
                . fixTree . assignAddr . fixFileNames . flatten
             <$!> makeTree fp

-- fill out ChildList for each SimpDir using input from assignAddr          --
fixTree :: [(ChildAddr, SimpNode)] -> [SimpNode]
fixTree [] = []
fixTree [x] = [snd x]
fixTree (x:xs) = case snd x of
    SimpDir nm cn cl    -> let newcl = consume xs (fromIntegral cn)
                           in (SimpDir nm cn newcl):(fixTree xs)
    SimpFile nm ds dt   -> (SimpFile nm ds dt):(fixTree xs)

    where consume :: [(ChildAddr, SimpNode)] -> ChildAddr -> ChildList
          consume _ 0 = []
          consume (y:ys) num = (fst y):consume ys (num - 1)

-- generate addresses for each node from flattened tree                     --
assignAddr :: [SimpNode] -> [(ChildAddr, SimpNode)]
assignAddr = assignAddrStep 0
    where assignAddrStep :: ChildAddr -> [SimpNode] -> [(ChildAddr, SimpNode)]
          assignAddrStep _ [] = []
          assignAddrStep addr (x:xs) =
            let off = (addr + (fromIntegral . getSimpNodeSize) x :: ChildAddr)
            in (addr, x):assignAddrStep off xs

-- take only "basename" (or filename) out of paths in flattened tree        --
fixFileNames :: [SimpNode] -> [SimpNode]
fixFileNames = map (\x -> case x of
            SimpDir nm cn cl    -> SimpDir (takeFileName nm) cn cl
            SimpFile nm ds dt   -> SimpFile (takeFileName nm) ds dt)

-- generate tree from directory                                             --
makeTreeFold :: Tree SimpNode -> FilePath -> IO (Tree SimpNode)
makeTreeFold (Node (SimpDir dir cn cl) subTree) curr =

    let newPath = joinPath [ dir, curr ]
    in (\isDir -> case isDir of

        False   -> return
                 . (\x -> Node (SimpDir dir (cn + 1) []) (Node x []:subTree))
                 . (\x -> SimpFile newPath (fromIntegral $ BS.length x) x)
               =<< BS.readFile newPath

        True    -> return
                 . (\x -> Node (SimpDir dir (cn + 1) []) (x:subTree))
               =<< makeTree newPath

    ) =<< doesDirectoryExist newPath

makeTree :: FilePath -> IO (Tree SimpNode)
makeTree fp = foldM makeTreeFold (Node (SimpDir fp 0 []) [])
          =<< listDirectory fp
