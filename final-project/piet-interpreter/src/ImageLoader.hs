module ImageLoader where

import System.IO (openFile, IOMode(ReadMode, WriteMode, ReadWriteMode))
import Codec.Picture


path :: String
path = "../images/hw.gif"

readPixels = do
    image <- readImage path
    return convertRGBA8 <$> image


