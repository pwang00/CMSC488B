module Example where

import Interpreter
import PietTypes
import ImageLoader
import Codec.Picture

path :: String
path = "/home/philip2000/Documents/CMSC488B/final-project/piet-interpreter/images/hw1.gif"

example :: IO (Either String PietProgram)
example = do
    res <- imageToProgram path 11
    case res of
        (Left err) -> return $ Left err
        (Right img) ->
            interp img initialState


    