module Main where

import Options.Applicative
import System.IO (Handle, hPutStrLn, stdout, stderr)
import System.Exit (ExitCode(..), exitWith, exitSuccess)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (second)
import Control.Exception (try, IOException)
import Control.Monad.Validate
import Data.List (isSuffixOf)

import qualified Parser.TS as TSParser
import qualified Parser.CTL as CTLParser
import qualified Extension.Minimm.Parser as MiniParser
import Model.TS (TS, toDot)
import Model.CTL (CTL)
import Extension.Minimm.Transform (transform)
import Verify.Check

data CommandLine
    = VerifyModel
        { onlySyntax   :: Bool
        , debug        :: Bool
        , dot          :: Bool
        , modelPath    :: FilePath
        , formulaPaths :: [FilePath]
        }
    | PrintExtensions
    deriving (Eq, Show)

commandLine :: Parser CommandLine
commandLine =
    PrintExtensions
        <$ switch
            (  long "extensions"
            <> help "Print supported extensions and exit"
            )
    <|> VerifyModel
        <$> switch
            (  long "only-syntax"
            <> help "Check syntax correctness and exit"
            )
        <*> switch
            (  long "debug"
            <>  help "Print the parsed transition system and formulas"
            )
        <*> switch
            (  long "dot"
            <>  help "Print the transition system as DOT Graph"
            )
        <*> argument str (metavar "MODEL" <> action "file")
        <*> some (argument str (metavar "FORMULAS" <> action "file"))

type ErrorMessage = String
type ValidationError = (FilePath, ErrorMessage)

safeReadFile :: FilePath -> ValidateT [ValidationError] IO String
safeReadFile filepath = do
    result <- liftIO $ (try $ readFile filepath :: IO (Either IOException String))
    case result of
        Left err      -> refute [(filepath, drop 2 $ dropWhile (/= ':') $ show err)]
        Right content -> return content

readTS :: FilePath -> ValidateT [ValidationError] IO TS
readTS filepath = do
    content <- safeReadFile filepath
    case TSParser.parse content of
        Just ts | TSParser.validate ts -> return ts
                | otherwise            -> refute [(filepath, "invalid model")]
        Nothing -> refute [(filepath, "syntax error in transition system")]

readMiniProgram :: FilePath -> ValidateT [ValidationError] IO TS
readMiniProgram filepath = do
    content <- safeReadFile filepath
    case transform <$> MiniParser.parse content of
        Just ts | TSParser.validate ts -> return ts
                | otherwise            -> error (filepath ++ ": invalid MINI transformation")
        Nothing -> refute [(filepath, "syntax error in MINI source code")]

readModel :: FilePath -> ValidateT [ValidationError] IO TS
readModel filepath
    | any (`isSuffixOf` filepath) [".mini", ".mm", ".minimm"]
        = readMiniProgram filepath
    | otherwise
        = readTS filepath

readCTL :: FilePath -> ValidateT [ValidationError] IO (FilePath, CTL)
readCTL filepath = do
    case CTLParser.parse filepath of
        Just ctl -> return (filepath, ctl)
        Nothing  -> do
            content <- safeReadFile filepath
            case CTLParser.parse content of
                Just ctl -> return (filepath, ctl)
                Nothing  -> refute [(filepath, "syntax error")]

printPadded :: Handle -> [(String, String)] -> IO ()
printPadded handle rows =
    let
        padding = maximum $ map (length . fst) rows
    in
        forM_ rows $ \(k, v) -> do
            hPutStrLn handle $ pad padding k ++ " => " ++ v
    where
        pad :: Int -> String -> String
        pad n s = s ++ replicate (n - length s) ' '

main :: IO ()
main = execParser opts >>= main'
    where
    opts = info (commandLine <**> helper)
        (  fullDesc
        <> progDesc "Validate model (raw transition system or MINI program) against any number of CTL formulas."
        )

main' :: CommandLine -> IO ()
main' PrintExtensions = do
    putStrLn "MINI Language Support"
main' (VerifyModel onlyCheckSyntax debugMode dotFormat modelFilepath formulaFilepaths) = do
    result <- runValidateT $ liftA2 (,) (readModel modelFilepath) (forM formulaFilepaths readCTL)
    (ts, formulas) <- case result of
        Left  err -> printPadded stderr err >> exitWith (ExitFailure 2)
        Right val -> return val

    when debugMode $ do
        putStrLn "-----------------------"
        putStrLn "Transition System:"
        putStrLn (if dotFormat then toDot ts else show ts)
        putStrLn "-----------------------"
        putStrLn "Formulas:"
        forM_ formulas $ \(origin, f) -> do
            putStrLn $ origin ++ ": " ++ show f
        putStrLn "-----------------------"

    when onlyCheckSyntax
        exitSuccess

    let verification = map (second (\ctl -> if verify ts ctl then "OK" else "FAIL")) formulas
    printPadded stdout verification

    unless (all ((== "OK") . snd) verification) $
        exitWith (ExitFailure 1)
