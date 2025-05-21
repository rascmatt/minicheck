module Lib.Parser.Options (Options, oArgs, oTsFile, oProgFile, oHelp, oIllegal, parse) where

import Lib.Parser.Base

data Options = Options {
    oArgs     :: [String],
    oTsFile   :: String,
    oProgFile :: String,
    oHelp     :: Bool,
    oIllegal  :: [String] -- Unparsable arguments
} deriving (Show)

mOptional :: Parse String -> Parse String
mOptional p = pure "" `mplus` p

mFlag :: String -> Parse Bool
mFlag key = do
    _ <- skipWs (string "--")
    _ <- string key
    f <- mOptional (do {
        _ <- skipWs (string "=");
        (neList . sat) (not . isWs)
    })
    case f of
        "true"  -> return True
        ""      -> return True
        "false" -> return False
        _       -> mzero

mOption :: String -> Parse String
mOption key = do
    _ <- skipWs (string "--")
    _ <- string key
    _ <- skipWs (string "=")
    (neList . sat) (not . isWs)

mArg :: Parse String
mArg = do
    c <- skipWs (sat (/= '-'))
    s <- (list . sat) (not . isWs)
    return (c:s)

option :: String -> String -> Maybe String
option key = topLevel (mOption key)

arg :: String -> Maybe String
arg = topLevel mArg

flag :: String -> String -> Maybe Bool
flag key = topLevel (mFlag key)

parse :: [String] -> Options
parse args = Options ar tsFile pFile hFlag illegal
    where
        parsed  = [ (a, arg a, option "ts" a, flag "help" a, option "p" a) | a <- args ]
        -- Collect everything we can't parse at all
        illegal = [ a | (a, Nothing, Nothing, Nothing, Nothing) <- parsed ]
        -- If we can't parse it to a flag or an option, its a normal argument
        ar      = [ x | (_, Just x, Nothing, Nothing, Nothing) <- parsed ]
        ts      = [ tt | (_, _, Just tt, _, _) <- parsed ]
        -- Use the last occurrence of the '--ts' flag
        tsFile  = if null ts then [] else last ts
        -- Use the last occurrence of the '--help' flag
        help    = [ x | (_, Nothing, Nothing, Just x, _) <- parsed ]
        hFlag   = not (null help) && last help
        p       = [ pp | (_, _, _, _, Just pp) <- parsed ]
        -- Use the last occurrence of the '--p' flag
        pFile  = if null p then [] else last p
