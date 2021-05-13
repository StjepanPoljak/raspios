module SimpNode
    ( SimpNode(..)
    , getSimpNodeSize
    , Name
    , DataSize
    , Data
    , ChildNum
    , ChildAddr
    , ChildList
    ) where

import Data.Binary
    (Binary, Put, Get, put, get, Word8, Word32)
import Data.Char
    (chr)
import qualified Data.ByteString.Lazy as BS
    (ByteString, pack, unpack)
import Control.Monad
    (foldM_)

type Name       = FilePath
type DataSize   = Word32
type Data       = BS.ByteString
type ChildNum   = Word32
type ChildAddr  = Word32
type ChildList  = [ChildAddr]

data SimpNode = SimpFile Name DataSize Data
              | SimpDir Name ChildNum ChildList
              deriving (Show)

getSimpNodeSize :: SimpNode -> Int
getSimpNodeSize (SimpFile nm ds _) = 1                      -- Type         --
                                   + length nm + 1          -- Name         --
                                   + 4                      -- DataSize     --
                                   + fromIntegral ds        -- Data         --
getSimpNodeSize (SimpDir nm cn _)  = 1                      -- Type         --
                                   + length nm + 1          -- Name         --
                                   + 4                      -- ChildNum     --
                                   + 4 * fromIntegral cn    -- ChildList    --

instance Binary SimpNode where
    put (SimpFile nm ds dt) = do put (1 :: Word8)
                                 putString nm
                                 put (fromIntegral ds :: Word32)
                                 foldM_ (\_ x -> put x) () (BS.unpack dt)

    put (SimpDir nm cn cl)  = do put (0 :: Word8)
                                 putString nm
                                 put (fromIntegral cn :: Word32)
                                 mapM_ put cl

    get = do t <- get :: Get Word8
             case t of
                0 -> do nm <- consumeString ""
                        cn <- get :: Get ChildNum
                        cl <- consume cn [] :: Get [ChildAddr]
                        return (SimpDir nm cn cl)
                1 -> do nm <- consumeString ""
                        ds <- get :: Get DataSize
                        dt <- consume ds [] :: Get [Word8]
                        return (SimpFile nm ds (BS.pack dt))
                _ -> error "Invalid file type"

        where consumeString :: String -> Get String
              consumeString str = do
                        t <- get :: Get Word8
                        case t of
                            0   -> return str
                            _   -> consumeString (str ++ [(chr . fromEnum) t])

              consume :: (Integral n, Binary a) => n -> [a] -> Get [a]
              consume 0 lst = return lst
              consume num lst = (\x -> consume (num - 1) (x:lst)) =<< get

putString :: String -> Put
putString str = mapM_ (put . (fromIntegral :: Int -> Word8) . fromEnum)
                (str ++ "\0")

