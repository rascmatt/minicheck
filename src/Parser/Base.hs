{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-|
Module      : Parser.Base
Description : A minimal monadic parser combinator library

This module provides the foundation for building parsers using monadic combinators.

-}
module Parser.Base where

-- Monad Parser Base

newtype Parse a = Parse (String -> [(a, String)])

instance Functor Parse where
  fmap :: (a -> b) -> Parse a -> Parse b
  fmap f (Parse x) = Parse (\cs -> [ (f a, cs') | (a, cs') <- x cs ])

instance Applicative Parse where

    -- succeed
    pure :: a -> Parse a
    pure a = Parse (\cs -> [(a, cs)])

    (<*>) :: Parse (a -> b) -> Parse a -> Parse b
    p1 <*> p2 = do {x1 <- p1; x2 <- p2; return (x1 x2)}

instance Monad Parse where
    -- >*>
    p >>= f = Parse (\cs -> concat [unbox (f a) cs' | (a, cs') <- unbox p cs])

unbox :: Parse a -> (String -> [(a, String)])
unbox (Parse p) = p

-- TODO
class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

instance MonadPlus Parse where
  -- none
  mzero :: Parse a
  mzero = Parse (const [])
  -- alt
  mplus :: Parse a -> Parse a -> Parse a
  mplus (Parse p1) (Parse p2) = Parse (\cs -> p1 cs ++ p2 cs)


-- Universal Parser Basis --

char :: Char -> Parse Char
char c = sat (== c)

item :: Parse Char
item = Parse (\cs -> case cs of
                ""      -> []
                (c:ccs) -> [(c, ccs)])

peek :: Parse Char
peek = Parse (\cs -> case cs of
                ""      -> []
                (c:ccs) -> [(c, c:ccs)]) -- Don't consume the character

string :: String -> Parse String
string ""       = return ""
string (c:cs)   = do {mc <- char c; ms <- string cs; return (mc:ms)}

strings :: [String] -> Parse String
strings []  = mzero
strings [s] = string s
strings (s:sr) = string s `mplus` strings sr

sat :: (Char -> Bool) -> Parse Char
sat p = do
    c <- item
    if p c then return c else mzero

(+++) :: Parse a -> Parse a -> Parse a
p +++ q = Parse (\cs -> case unbox (p `mplus` q) cs of
    [] -> []
    (x:_) -> [x])

many :: Parse a -> Parse [a]
many p = many1 p +++ return []

many1 :: Parse a -> Parse [a]
many1 p = do a <- p; as <- many p; return (a:as)

sepby :: Parse a -> Parse b -> Parse [a]
p `sepby` sep = (p `sepby1` sep) +++ return []

sepby1 :: Parse a -> Parse b -> Parse [a]
p `sepby1` sep = do
    a <- p
    as <- many (sep >> p)
    return (a:as)

space :: Parse String
space = many (sat isWs)

token :: Parse a -> Parse a
token p = do {a <- p; space; return a}

symbol :: String -> Parse String
symbol = token . string

-- Possibly empty list
list :: Parse a -> Parse [a]
list p = pure [] `mplus` do { c <- p; s <- list p; return (c:s) }

-- Non-empty list
neList :: Parse a -> Parse [a]
neList p = do { c <- p; s <- list p; return (c:s) }

-- Possibly empty list (skipWs between elements)
listSw :: Parse a -> Parse [a]
listSw p = pure [] `mplus` do { c <- skipWs p; s <- listSw p; return (c:s) }

-- Non-empty list (skipWs between elements)
neListSw :: Parse a -> Parse [a]
neListSw p = do { c <- skipWs p; s <- listSw p; return (c:s) }

skipWs :: Parse a -> Parse a
skipWs p = do
    n <- peek
    if isWs n then do { _ <- item; skipWs p} else p

isWs :: Char -> Bool
isWs c = c `elem` [' ', '\t', '\n', '\r']

parses :: Parse a -> String -> Bool
parses (Parse p) input = not $ null [ found | (found, []) <- p input]

topLevel :: Parse a -> String -> Maybe a
topLevel (Parse p) input = case results of
        []         -> Nothing
        (result:_) -> Just result
    where results = [ found | (found, []) <- p input]
