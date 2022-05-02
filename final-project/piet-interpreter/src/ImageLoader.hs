module ImageLoader where

import System.IO (openFile, IOMode(ReadMode, WriteMode, ReadWriteMode))
import Codec.Picture (readImage, pixelAt, decodeImage, convertRGB8)
import Codec.Picture.Types
import qualified Data.Vector as Vec
import Data.Vector ((!))
import PietTypes

path :: String
path = "/home/philip2000/Documents/CMSC488B/final-project/piet-interpreter/images/hw1.gif"

special :: [PixelRGB8]
special = [PixelRGB8 255 255 255, PixelRGB8 0 0 0];

colorTable :: Vector (PixelRGB8)
colorTable = Vec.fromList [PixelRGB8 255 192 192, PixelRGB8 255 255 192, PixelRGB8 192 255 192, 
    PixelRGB8 192 255 255, PixelRGB8 192 192 255, PixelRGB8 255 192 255, 
    PixelRGB8 255 0 0, PixelRGB8 255 255 0, PixelRGB8 0 255 0, PixelRGB8 0 255 255, 
    PixelRGB8 0 0 255, PixelRGB8 255 0 255, PixelRGB8 192 0 0, PixelRGB8 192 192 0, 
    PixelRGB8 0 192 0, PixelRGB8 0 192 192, PixelRGB8 0 0 192, PixelRGB8 192 0 192]

readAndConvertImg :: FilePath -> IO (Either String (Image PixelRGB8))
readAndConvertImg path = do
    res <- readImage path
    case res of
        Right img -> return (Right $ convertRGB8 img)
        Left err -> return $ Left err

imgToPixelRGB8s :: Image PixelRGB8 -> Vector (Vector PixelRGB8)
imgToPixelRGB8s img@(Image w h _) = Vec.fromList [
    Vec.fromList [pixelAt img x y | x <- [0..w-1]] | y <- [0..h-1]
     ]

-- Determines the ratio of a codel to pixel to make interpreting more efficient
determineCodelSize :: Vector (Vector PixelRGB8) -> Int
determineCodelSize img = undefined

-- Indexes the color table vector using ki + j where k = 6
decodeInstr :: PixelRGB8 -> PixelRGB8 -> PietInstr
decodeInstr p1 p2 | elem p1 special || elem p2 special = Nop
decodeInstr p1 p2 =
    let idxP1 = Vec.elemIndex p1 colorTable in
    let idxP2 = Vec.elemIndex p2 colorTable in

    case (idxP1, idxP2) of
        (Just i1, Just i2) ->
                let (r1, c1) = divMod i1 6 in
                let (r2, c2) = divMod i2 6 in
                let (lightDiff, hueDiff) = ((r1 - r2) `mod` 3, (c1 - c2) `mod` 6) in
                cmdTable ! (6 * lightDiff + hueDiff)
        _ -> Nop