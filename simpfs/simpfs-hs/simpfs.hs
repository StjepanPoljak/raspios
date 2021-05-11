module Main where

import Data.Binary
import System.IO
import qualified Data.ByteString.Lazy as BS
import Data.Int
import Data.Tree
import Control.Monad
import System.Directory
import System.FilePath

type Name       = FilePath
type DataSize   = Int64
type Data       = BS.ByteString
type ChildNum   = Int32

data SimpNode = SimpFile Name DataSize Data
              | SimpDir Name ChildNum [Int]
              deriving (Show)

putString :: String -> Put
putString str = mapM_ (\x -> put (fromIntegral $ fromEnum x :: Word8)) (str ++ "\0")

instance Binary SimpNode where
    put (SimpFile nm ds dt) = do put (1 :: Word8)
                                 putString $ takeFileName nm
                                 put (fromIntegral ds :: Word32)
                                 foldM_ (\_ x -> put x) () (BS.unpack dt)
    put (SimpDir nm cn cl)  = do put (0 :: Word8)
                                 putString $ takeFileName nm
                                 put (fromIntegral cn :: Word32)
                                 mapM_ (\x -> put (fromIntegral x :: Word32)) cl
    get = do t <- get :: Get Word8
             case t of
                0 -> return (SimpDir "xx" 3 [])
                1 -> return (SimpFile "xxx" 3 BS.empty)

getName :: SimpNode -> Name
getName (SimpFile name _ _) = name
getName (SimpDir name _ _) = name

main = do
    tree <- (\x -> makeTree x) =<< makeRelativeToCurrentDirectory =<< getCurrentDirectory
    --let etree = encode (flatten tree) in do
        --print etree
--        print $ (BS.pack . map (fromIntegral . ord)) "hello world!"
    --print $ assignAddr . fixFileNames . flatten $ tree
    --print $ fixTree . assignAddr . fixFileNames . flatten $ tree
    BS.writeFile "TEST.bin" $ foldl (\xs x -> BS.append xs (encode x)) BS.empty (fixTree . assignAddr . fixFileNames . flatten $ tree)

getSimpNodeSize :: SimpNode -> Int
getSimpNodeSize (SimpFile nm ds dt) = 1 + (length nm) + 1 + 4 + (fromIntegral ds :: Int)
getSimpNodeSize (SimpDir nm cn cl) = 1 + (length nm) + 1 + 4 + 4 * (fromIntegral cn :: Int)

fixFileNames :: [SimpNode] -> [SimpNode]
fixFileNames = map (\x -> case x of
                        SimpDir nm cn cl -> SimpDir (takeFileName nm) cn cl
                        SimpFile nm ds dt -> SimpFile (takeFileName nm) ds dt)

assignAddr :: [SimpNode] -> [(Int, SimpNode)]
assignAddr = assignAddrStep 0
    where assignAddrStep :: Int -> [SimpNode] -> [(Int, SimpNode)]
          assignAddrStep _ [] = []
          assignAddrStep addr (x:xs) = (addr, x):(assignAddrStep (addr + getSimpNodeSize x) xs)

fixTree :: [(Int, SimpNode)] -> [SimpNode]
fixTree [] = []
fixTree [x] = [snd x]
fixTree (x:xs) = case snd x of
                SimpDir nm cn cl -> (SimpDir nm cn (consume xs (fromIntegral cn :: Int))):(fixTree xs)
                SimpFile nm ds dt -> (SimpFile nm ds dt):(fixTree xs)
        where consume :: [(Int, SimpNode)] -> Int -> [Int]
              consume _ 0 = []
              consume (y:ys) num = (fst y):consume ys (num - 1)

makeTreeFold :: Tree SimpNode -> FilePath -> IO (Tree SimpNode)
makeTreeFold (Node (SimpDir dir cn cl) subTree) curr =

    let newPath = joinPath [ dir, curr ]
    in (\isDir -> case isDir of

        False   -> return
                 . (\x -> Node (SimpDir dir (cn + 1) []) (Node x []:subTree))
                 . (\x -> SimpFile newPath (BS.length x) x)
               =<< BS.readFile newPath

        True    -> return
                 . (\x -> Node (SimpDir dir (cn + 1) []) (x:subTree))
               =<< makeTree newPath

    ) =<< doesDirectoryExist newPath

makeTree :: FilePath -> IO (Tree SimpNode)
makeTree fp = foldM makeTreeFold (Node (SimpDir fp 0 []) [])
          =<< listDirectory fp
