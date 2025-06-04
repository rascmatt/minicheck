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

import qualified Lib.Parser.TS as TSParser
import qualified Lib.Parser.CTL as CTLParser
import qualified Lib.Extension.Minimm.Parser as MiniParser
import Lib.Model.TS (TS)
import Lib.Model.CTL (CTL)
import Lib.Extension.Minimm.Transform (transform)
import Lib.Verify.Check

data CommandLine
    = VerifyModel
        { onlySyntax   :: Bool
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
main' (VerifyModel onlyShowSyntax modelFilepath formulaFilepaths) = do
    result <- runValidateT $ liftA2 (,) (readModel modelFilepath) (forM formulaFilepaths readCTL)
    (ts, formulas) <- case result of
        Left  err -> printPadded stderr err >> exitWith (ExitFailure 2)
        Right val -> return val

    when onlyShowSyntax
        exitSuccess

    let verification = map (second (\ctl -> if verify ts ctl then "OK" else "FAIL")) formulas
    printPadded stdout verification

    unless (all ((== "OK") . snd) verification) $
        exitWith (ExitFailure 1)
