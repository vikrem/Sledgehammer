module Parser where
import Control.Monad
import Data.Char

newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser f) str = f str

instance Monad Parser where
    return v = Parser $ \str -> [(v, str)]
    p >>= q = Parser $ \str -> case parse p str of
                [] -> []
                [(val, rest)] -> parse (q val) rest

instance MonadPlus Parser where
    mzero = Parser (\_ -> [])
    mplus a b = Parser (\inp -> case parse a inp of
                    [] -> parse b inp
                    [(v, str)] -> [(v, str)])

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = mplus

failure = mzero

pullchar :: Parser Char
pullchar = Parser (\inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x, xs)])

matchpred :: (Char -> Bool) -> Parser Char
matchpred pred = do
                c <- pullchar
                if pred c then return c else failure
char :: Char -> Parser Char
char x = matchpred (== x)

digit :: Parser Char
digit = matchpred isDigit

alpha = matchpred isAlpha

oneOf :: String -> Parser Char
oneOf [] = failure 
oneOf (x:xs) = char x <|> oneOf xs

noneOf :: String -> Parser Char
noneOf [] = pullchar
noneOf s@(x:xs) = Parser (\inp -> case parse (oneOf s) inp of
                        [] -> parse pullchar inp
                        _ -> parse failure inp)

string :: String -> Parser String
string [] = return []
string (y:ys) = do
                x <- char y
                xs <- string ys
                return (x:xs)

epsilon :: Parser [a]
epsilon = return []

many :: Parser a -> Parser [a]
many p = many1 p <|> epsilon

many1 :: Parser a -> Parser [a]
many1 p = do
        x <- p
        xs <- many p
        return (x:xs)

getUntil :: Char -> Parser String
getUntil c = getWhile (/= c)

getWhile :: (Char -> Bool) -> Parser String
getWhile p = many $ matchpred p

getWhile1 :: (Char -> Bool) -> Parser String
getWhile1 p = many1 $ matchpred p

spaces :: Parser String
spaces = getWhile $ isSpace

newline :: Parser Char
newline = char '\n'

newlines :: Parser String
newlines = many $ char '\n'

token :: Parser String
token = do
        spaces
        x <- getWhile1 isAlpha
        spaces
        return x
