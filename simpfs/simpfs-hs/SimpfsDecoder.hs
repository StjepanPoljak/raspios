module SimpfsDecoder (simpfsDecode, simpfsMakeTree, simpfsDrawTree) where

import SimpNode

import qualified Data.IntMap.Strict as IMS

import Data.Tree
    (Tree(Node), drawTree)
import Data.Binary
    (decode)
import Data.Int
    (Int64)
import qualified Data.ByteString.Lazy as BS
    (ByteString, drop, null)

unflatten :: [(ChildAddr, SimpNode)] -> Maybe (Tree SimpNode)
unflatten [] = Nothing
unflatten lst =
        let dict = IMS.fromList . map (\(x, y) -> (fromIntegral x, y)) $ lst
        in Just $ unflattenIMS dict (Node (dict IMS.! 0) [])

unflattenIMS :: IMS.IntMap SimpNode -> Tree SimpNode -> Tree SimpNode
unflattenIMS dict (Node (SimpDir nm cn cl) stree) = Node (SimpDir nm cn cl) (setCL dict cl)
    where setCL :: IMS.IntMap SimpNode -> [ChildAddr] -> [Tree SimpNode]
          setCL _ [] = []
          setCL dict' (x:xs) =
                let new = dict' IMS.! (fromIntegral x) in
                    case new of
                        SimpFile _ _ _  -> (Node new []):setCL dict' xs
                        SimpDir _ _ _   -> unflattenIMS dict' (Node new []):setCL dict' xs

simpfsDrawTree :: Tree SimpNode -> String
simpfsDrawTree = drawTree . simpfsDrawableTree

simpfsDrawableTree :: Tree SimpNode -> Tree String
simpfsDrawableTree (Node (SimpDir nm cn cl) stree) = Node nm (map simpfsDrawableTree stree)
simpfsDrawableTree (Node (SimpFile nm ds dt) []) = Node nm []

simpfsMakeTree :: BS.ByteString -> Maybe (Tree SimpNode)
simpfsMakeTree = unflatten . simpfsDecode

simpfsDecode :: BS.ByteString -> [(ChildAddr, SimpNode)]
simpfsDecode = decodeStep 0
    where decodeStep :: ChildAddr -> BS.ByteString -> [(ChildAddr, SimpNode)]
          decodeStep addr bs
            | BS.null bs    = []
            | otherwise     = let snode     = decode bs
                                  snsize    = fromIntegral . getSimpNodeSize
                                            $ snode :: Int64
                                  nadr      = addr + fromIntegral snsize in
                              (addr, snode):decodeStep nadr (BS.drop snsize bs)


