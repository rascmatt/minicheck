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
import Model.CTL (CTL(..), PathFormula(..))
import Extension.Minimm.Transform (transform)
import Verify.Check
import Model.TS (TS(props), Proposition (prop), toDot)
import Model.Pattern (Pattern, matchesPattern)

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
        <*> many (argument str (metavar "FORMULAS" <> action "file"))

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

validateCTL :: TS -> (FilePath, CTL) -> ValidateT [ValidationError] IO (FilePath, CTL)
validateCTL ts (fp, ctl) = do
    let cp = ctlProps ctl
    let tp = map prop (props ts)
    let diff = filter (\pattern -> not $ any (matchesPattern pattern) tp) cp
    if (not . null) diff then
        if length diff == 1 then
            refute [(fp, "Atomic proposition \"" ++ show (head diff) ++ "\" does not occur in the transition system.")]
        else
            refute [(fp, "Atomic propositions " ++ show diff ++ " do not occur in the transition system.")]
    else
        return (fp, ctl)

ctlProps :: CTL -> [Pattern]
ctlProps (AtomicProposition p) = [p]
ctlProps (BinaryOperation _ a b) = ctlProps a ++ ctlProps b
ctlProps (Negation a) = ctlProps a
ctlProps (Exists a) = ctlPropsPath a
ctlProps (ForAll a) = ctlPropsPath a
ctlProps _ = []

ctlPropsPath :: PathFormula -> [Pattern]
ctlPropsPath (Next n) = ctlProps n
ctlPropsPath (Until p u) = ctlProps p ++ ctlProps u
ctlPropsPath (Eventually e) = ctlProps e
ctlPropsPath (Globally g) = ctlProps g

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
    putStrLn "Proposition Patterns"
    putStrLn "Transition System DOT Visualization"
main' (VerifyModel onlyCheckSyntax debugMode dotFormat modelFilepath formulaFilepaths) = do
    result <- runValidateT $ liftA2 (,) (readModel modelFilepath) (forM formulaFilepaths readCTL)

    (ts, formulas) <- case result of
        -- Report any IO or syntax errors and exit
        Left  err -> printPadded stderr err >> exitWith (ExitFailure 2)
        Right (ts, cs) -> do
            -- Semantic validation of the formulas against the transition system
            val <- runValidateT $ forM cs (validateCTL ts)
            case val of
                Left  err  -> printPadded stderr err >> exitWith (ExitFailure 2)
                Right ctls -> return (ts, ctls)

    when debugMode $ do
        putStrLn "-----------------------"
        putStrLn "Transition System:"
        putStrLn (if dotFormat then toDot ts else show ts)
        putStrLn "-----------------------"
        unless (null formulas) $ do
            putStrLn "Formulas:"
            forM_ formulas $ \(origin, f) -> do
                putStrLn $ origin ++ ": " ++ show f
            putStrLn "-----------------------"

    when onlyCheckSyntax $ do
        putStrLn "Syntax check successful"
        exitSuccess

    when (null formulas) $ do
        hPutStrLn stderr "No input formula specified" >> exitWith (ExitFailure 2)

    let verification = map (second (\ctl -> if verify ts ctl then "OK" else "FAIL")) formulas
    printPadded stdout verification

    unless (all ((== "OK") . snd) verification) $
        exitWith (ExitFailure 1)
