module Frozone.Util.Random where

import System.Random
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base16 as B16

randomB16Ident :: Int -> IO String
randomB16Ident entropy =
    do g <- newStdGen
       return $ BSC.unpack $ B16.encode $ BSC.pack (take entropy $ randoms g)
