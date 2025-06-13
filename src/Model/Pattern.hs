module Model.Pattern (Pattern, makePattern, matchesPattern) where

import Parser.Base
import Control.Applicative (liftA2)
import Data.String (IsString(..))

-- | Patterns can include wildcards: "*" matches any string, while "#" matches any string
--   that does not contain a dot (".").
data Pattern = Pattern (String -> Bool) String

instance Show Pattern where
  show (Pattern _ s) = s

instance Eq Pattern where
  (Pattern _ s1) == (Pattern _ s2) = eqPattern s1 s2

eqPattern :: String -> String -> Bool
eqPattern ('*':s1) ('*':s2) = dropWhile (== '*') s1 == dropWhile (== '*') s2
eqPattern ('#':s1) ('#':s2) = dropWhile (== '#') s1 == dropWhile (== '#') s2
eqPattern (c1 :s1) (c2 :s2) = c1 == c2 && eqPattern s1 s2
eqPattern [] [] = True
eqPattern _ _   = False

instance IsString Pattern where
  fromString = makePattern

makePattern :: String -> Pattern
makePattern patternString = Pattern (\str -> patternParser patternString `parses` str) patternString

-- | Checks whether a string matches the specified pattern.
matchesPattern :: Pattern -> String -> Bool
matchesPattern (Pattern check _) = check

patternParser :: String -> Parse String
patternParser ('*':rest) = liftA2 (++) (list $ sat (const True)) (patternParser rest)
patternParser ('#':rest) = liftA2 (++) (list $ sat (/= '.')) (patternParser rest)
patternParser (c  :rest) = liftA2 (:) (char c) (patternParser rest)
patternParser []         = return []
