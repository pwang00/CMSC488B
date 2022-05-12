module ImageLoader where

import System.IO (openFile, IOMode(ReadMode, WriteMode, ReadWriteMode))
import Codec.Picture (readImage, pixelAt, decodeImage, convertRGB8)
import Codec.Picture.Types
import qualified Data.Vector as Vec
import Data.Vector ((!))
import PietTypes

special :: [PixelRGB8]
special = [PixelRGB8 255 255 255, PixelRGB8 0 0 0];

colorTable :: Vector (PixelRGB8)
colorTable = Vec.fromList [PixelRGB8 255 192 192, PixelRGB8 255 255 192, PixelRGB8 192 255 192, 
    PixelRGB8 192 255 255, PixelRGB8 192 192 255, PixelRGB8 255 192 255, 
    PixelRGB8 255 0 0, PixelRGB8 255 255 0, PixelRGB8 0 255 0, PixelRGB8 0 255 255, 
    PixelRGB8 0 0 255, PixelRGB8 255 0 255, PixelRGB8 192 0 0, PixelRGB8 192 192 0, 
    PixelRGB8 0 192 0, PixelRGB8 0 192 192, PixelRGB8 0 0 192, PixelRGB8 192 0 192]

imgToPixelRGB8s :: Image PixelRGB8 -> ImageGrid
imgToPixelRGB8s img@(Image w h _) = Vec.fromList [
    Vec.fromList [pixelAt img x y | x <- [0..w-1]] | y <- [0..h-1]
     ]

configureProgram :: ImageGrid -> CodelSize -> PietProgram
configureProgram img cs | Vec.length img < 1 = error "Image is empty"
configureProgram img cs | Vec.length (img ! 0) < 1 = error "Image is empty"
configureProgram img cs = Prog {
    _grid = img, 
    _height = Vec.length img,
    _width = Vec.length (img ! 0),
    _cs = cs,
    _pstate = initialState
} 

imageToProgram :: FilePath -> CodelSize -> IO (Either String PietProgram)
imageToProgram path cs = do
    res <- readImage path
    case res of
        Left err -> return $ Left err
        Right img -> let conv = imgToPixelRGB8s $ convertRGB8 img in
            return $ Right (configureProgram conv cs)
            
-- Determines the ratio of a codel to pixel to make interpreting more efficient
determineCodelSize :: ImageGrid -> Int
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
                let (lightDiff, hueDiff) = ((r2 - r1) `mod` 3, (c2 - c1) `mod` 6) in
                cmdTable ! (6 * lightDiff + hueDiff)
        _ -> Nop