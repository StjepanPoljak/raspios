module SimpfsDecoder
    ( simpfsDecode
    , simpfsMakeTree
    , simpfsDrawTree
    , simpfsPeek) where

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
import System.FilePath (splitPath, dropTrailingPathSeparator)

unflatten :: [(ChildAddr, SimpNode)] -> Maybe (Tree SimpNode)
unflatten [] = Nothing
unflatten lst =
        let dict = IMS.fromList . map (\(x, y) -> (fromIntegral x, y)) $ lst
        in Just $ unflattenIMS dict (Node (dict IMS.! 0) [])

unflattenIMS :: IMS.IntMap SimpNode -> Tree SimpNode -> Tree SimpNode
unflattenIMS dict (Node (SimpDir nm cn cl) stree) = Node (SimpDir nm cn cl)
                                                         (setCL dict cl)
    where setCL :: IMS.IntMap SimpNode -> [ChildAddr] -> [Tree SimpNode]
          setCL _ [] = []
          setCL dict' (x:xs) = let new = dict' IMS.! (fromIntegral x) in

                case new of
                    SimpFile _ _ _  -> (Node new [])
                                     : setCL dict' xs

                    SimpDir _ _ _   -> unflattenIMS dict' (Node new [])
                                     : setCL dict' xs

simpfsDrawTree :: Tree SimpNode -> String
simpfsDrawTree = drawTree . simpfsDrawableTree

simpfsDrawableTree :: Tree SimpNode -> Tree String
simpfsDrawableTree (Node (SimpDir nm cn cl) stree) =
                    Node nm (map simpfsDrawableTree stree)
simpfsDrawableTree (Node (SimpFile nm ds dt) []) = Node nm []

simpfsMakeTree :: BS.ByteString -> Maybe (Tree SimpNode)
simpfsMakeTree = unflatten . simpfsDecode

getFName :: SimpNode -> FilePath
getFName (SimpDir nm _ _) = nm
getFName (SimpFile nm _ _) = nm

simpfsPeek :: FilePath -> [(ChildAddr, SimpNode)] -> Maybe SimpNode
simpfsPeek _ [] = Nothing
simpfsPeek fp lst =

    let dict = IMS.fromList . map (\(x, y) -> (fromIntegral x, y)) $ lst
        fpl = map (dropTrailingPathSeparator) . splitPath $ fp
        dict1 = dict IMS.! 0
        fpl1 = fpl !! 0 in

            if (getFName dict1) == fpl1
            then peekStep fp (drop 1 fpl) [fpl1] dict dict1
            else Nothing

    where peekStep :: FilePath -> [FilePath] -> [FilePath]
                               -> IMS.IntMap SimpNode -> SimpNode
                               -> Maybe SimpNode
          peekStep _ [] [] _ _ = Nothing
          peekStep fp' [] _ _ sn = Just sn
          peekStep fp' (x:xs) fpl dict' sn = (\newsn ->

                peekStep fp' xs (x:fpl) dict' newsn

            ) =<< findChild sn x dict' -- Nothing -- case sn of

          findChild :: SimpNode -> FilePath -> IMS.IntMap SimpNode
                                            -> Maybe SimpNode
          findChild (SimpDir _ _ []) _ _ = Nothing
          findChild (SimpFile _ _ _) _ _ = Nothing
          findChild (SimpDir nm cn (c:cs)) cfp dict'' =

                let curr = dict'' IMS.! (fromIntegral c) in

                    if (getFName curr) == cfp
                    then Just curr
                    else findChild (SimpDir nm cn cs) cfp dict''

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

