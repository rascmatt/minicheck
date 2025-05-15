module Main where

import Lib.Parser.TS (parse)

main :: IO ()
main = do
    content <- readFile "data/ts-1.txt"
    let ts = parse content
    if ts == Nothing then
        putStrLn "Failed to parse input file!"
    else do {    
        putStrLn "Parsed successfully:"; 
        print ts 
    }
    -- TODO: Validate the parsed transition system
