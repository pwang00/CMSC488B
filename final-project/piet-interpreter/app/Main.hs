module Main where

import PietTypes
import Options.Applicative
import Data.Semigroup ((<>))

data Config = Config {
  hello :: String,
  showfinalstate :: Bool
}

config :: Parser Config
config = Config
    <$> strOption
      ( long "path" 
      <> metavar "PATH" 
      <> short 'p'
      <> help "Supply a path to Piet image (.png, .jpg, .gif)")
    <*> switch
        ( long "showfinalstate"
        <> short 's'
        <> help "Whether or not to display the final program state upon termination")

main :: IO ()
main = runProgram =<< execParser opts
  where
    opts = info (sample <**> helper)
    ( fullDesc
      <> progDesc "Takes in a Piet program and interprets it, outputting a final state"
      <> header "Hiet: a Piet interpreter written in Haskell")

runProgram :: Config -> IO ()
runProgram (Sample path debug) = do
  img <- imageToProgram path
  case img of 
    (Left err) -> putStrLn "An error was encountered: " ++ show err
    (Right img) -> do
      finalState <- interp img (Res initialState Continue)

      if debug then 
        putStrLn "=====================Final State====================="
        putStrLn $ show finalState
        else return ()