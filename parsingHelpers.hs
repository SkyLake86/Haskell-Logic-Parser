{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use lambda-case" #-}
module ParsingHelpers where

import Control.Applicative ( Alternative(..) )
import Data.Char
    ( isSpace, isDigit, isAlpha, isAlphaNum, isLower, isUpper )




newtype Parser a = P (String -> [(a, String)])
parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp



instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of
                         []        -> []
                         [(v,out)] -> [(g v, out)])
instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v,inp)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
                          []        -> []
                          [(g,out)] -> parse (fmap g px) out)
instance Monad Parser where
  p >>= f = P (\inp -> case parse p inp of
                        []        -> []
                        [(v,out)] -> parse (f v) out)
  return x = P (\input -> [(x, input)])
instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                        []        -> parse q inp
                        [(v,out)] -> [(v,out)])


item :: Parser Char
item = P (\inp -> case inp of 
                     [] -> []
                     (x:xs) -> [(x,xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

letter :: Parser Char
letter = sat isAlpha

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

alphanum :: Parser Char
alphanum = sat isAlphaNum

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P $
    \inp -> case parse p inp of
        [] -> parse q inp
        [(v, out)] -> [(v, out)]


many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v : vs)

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v
