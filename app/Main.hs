module Main where

import System.Environment (getArgs)
import Data.Maybe (isNothing, fromJust)
import System.Exit (exitFailure)

import Lib.Model.TS (PTS(..))
import Lib.Parser.TS (parse, validate)

main :: IO ()
main = do

    -- TODO: Improve argument parsing
    args    <- getArgs
    fileName    <- case args of
        [n]  -> return n
        _    -> do
            putStrLn "Usage: minicheck 'filename'"
            exitFailure

    content <- readFile fileName
    let ts = parse content
    let v  = validate ts -- TODO: Maybe print a better validation message
    if isNothing ts then do
        putStrLn "Failed to parse input file!"
        exitFailure
    else do
        if isNothing v then do
            putStrLn "Invalid specification"
            exitFailure
        else do 
            print (P (fromJust v))

    -- The transition system is ready at this point
    
