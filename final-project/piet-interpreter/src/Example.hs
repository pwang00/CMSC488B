module Example where

import Interpreter
import PietTypes
import ImageLoader
import Codec.Picture
import Data.Vector ((!), (!?))
path :: String
path = "/home/philip2000/Documents/CMSC488B/final-project/piet-interpreter/images/hw1-small.gif"

example :: IO (Either String PietProgram)
example = do
    res <- imageToProgram path 1
    case res of
        (Left err) -> return $ Left err
        (Right img) -> do
            finalState <- interp img (Res initialState Continue)
            return $ Right img


    