module Main where

import System.Environment (getArgs)
import Data.Maybe (isNothing, fromJust)
import System.Exit (exitFailure)

import qualified Lib.Parser.Options as Options
import Lib.Parser.TS (parse, validate)

-- Validate options and exit in case of invalid arguments
validateOpts :: Options.Options -> IO ()
validateOpts options = do

    -- Check for illegal arguments
    if (not . null) (Options.oIllegal options) then do
        putStr "Invalid arguments: "
        print (Options.oIllegal options)
        putStrLn "Usage: minicheck --ts=<file> [--help]"
        exitFailure
    else do { return () }

    -- Print the usage info if the '--help' flag is used
    if Options.oHelp options then do
        putStrLn "Usage: minicheck --ts=<file> [--help]"
        exitFailure
    else do { return () }

    -- Assert that we have an input TS file
    if null (Options.oTsFile options) then do
        putStrLn "Missing input specification. "
        putStrLn "Usage: minicheck --ts=<file> [--help]"
        exitFailure
    else do { return () }

main :: IO ()
main = do

    args <- getArgs
    let options = Options.parse args
    validateOpts options
    
    content <- readFile (Options.oTsFile options)
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
            print (fromJust v)

    -- The transition system is ready at this point

