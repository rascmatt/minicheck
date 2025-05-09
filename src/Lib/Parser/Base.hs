{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lib.Parser.Base where

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
    Parse f <*> Parse x = Parse (\cs -> [ (f2 a, rem2) | (a, rem1) <- x cs, (f2, rem2) <- f rem1 ])

instance Monad Parse where
    -- >*>
    p >>= f = Parse (\cs -> concat [unbox (f a) cs' | (a, cs') <- unbox p cs])

unbox :: Parse a -> (String -> [(a, String)])
unbox (Parse p) = p

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

sat :: (Char -> Bool) -> Parse Char
sat p = do
    c <- item
    if p c then return c else mzero

apply :: Parse a -> String -> [(a, String)]
apply (Parse p) = p

-- Possibly empty list (skipWs between elements)
list :: Parse a -> Parse [a]
list p = pure [] `mplus` do { c <- skipWs p; s <- list p; return (c:s) }

-- Non-empty list (skipWs between elements)
neList :: Parse a -> Parse [a]
neList p = do { c <- skipWs p; s <- list p; return (c:s) }

skipWs :: Parse a -> Parse a
skipWs p = do
    n <- peek
    if isWs n then do { _ <- item; skipWs p} else p

isWs :: Char -> Bool
isWs c = c `elem` [' ', '\t', '\n', '\r']

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'