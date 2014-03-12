module Parser where
import Control.Monad
import Data.Char

data Parser a = Parser (String -> Consumed a)
data Consumed a = Consumed (Reply a)
                | Empty (Reply a)
                deriving (Eq, Show)
data Reply a = Ok a String Message | Error Message
                deriving (Eq, Show)
data Message = Message String [String]
                deriving (Eq, Show)

parse :: Parser a -> String -> Consumed a
parse (Parser f) str = f str

instance Monad Parser where
    return v = Parser $ \str -> Empty (Ok v str $ Message "" [])
    (Parser p) >>= q = Parser $ \str -> case p str of
                r@(Consumed (Ok a b m)) -> parse (q a) b
                r@(Empty (Ok a b m)) -> parse (q a) b
                (Consumed (Error m)) -> (Consumed (Error m)) 
                (Empty (Error m)) -> (Empty (Error m)) 

--    fail s = Parser $ \str -> Left $ "Parser error : " ++ s ++ " (found near '" ++ take 30 str ++ "')"

instance MonadPlus Parser where
    mzero = Parser $ \inp -> (Empty (Error $ Message "Parse error" []))
    mplus a b = Parser (\inp -> case parse a inp of
                    r@(Consumed _) -> r
                    r@(Empty (Ok a newrest m1)) -> case parse b inp of
                        Empty (Error m2) -> mergeOk a newrest m1 m2
                        Empty (Ok _ _ m2) -> mergeOk a newrest m1 m2
                        consumed -> consumed
                    r@(Empty (Error m1)) -> case parse b inp of
                        Empty (Error m2) -> mergeError m1 m2
                        Empty (Ok a _ m2) -> mergeOk a inp m1 m2
                        consumed -> consumed)
                where
                mergeOk x inp m1 m2 = Empty (Ok x inp (merge m1 m2))
                mergeError m1 m2 = Empty (Error (merge m1 m2))
                merge (Message inp exp1) (Message _ exp2) = Message inp $ exp1 ++ exp2


(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = mplus

(<?>) :: Parser a -> String -> Parser a
p <?> exp = Parser $ \inp -> case parse p inp of
                Empty (Error m) -> Empty (Error (expect m exp))
                (Empty (Ok x rest m)) -> (Empty (Ok x rest (expect m exp)))
                other -> other

expect :: Message -> String -> Message
expect (Message p oldexp) exp = Message p $ [exp] ++ oldexp

pullchar :: Parser Char
pullchar = Parser (\inp -> case inp of
                    [] -> Empty $ Error $ Message "Unexpected end of file" []
                    (x:xs) -> Consumed (Ok x xs $ Message "" []))

matchpred :: (Char -> Bool) -> Parser Char
matchpred pred = Parser $ \inp ->
                        case inp of
                        (x:xs) | pred x 
                            -> Consumed (Ok x xs $ Message "" [])
                        (x:xs) -> (Empty (Error $ Message [x] []))
                        [] -> (Empty (Error $ Message "End of input" [] ))

char :: Char -> Parser Char
char x = matchpred (== x) <?> show x

digit :: Parser Char
digit = matchpred isDigit <?> "digit"
alpha = matchpred isAlpha <?> "alphabetic"

oneOf :: String -> Parser Char
oneOf [] = error "Malformed usage of oneOf"
oneOf needles = matchpred (flip elem needles) <?> ("Looking for one of " ++ needles)

noneOf :: String -> Parser Char
noneOf [] = error "Bad usage of noneOf"
noneOf needles = matchpred (not . flip elem needles) <?> ("Looking for none of " ++ needles)


string :: String -> Parser String
string [] = Parser $ \inp -> Empty (Ok "" inp $ Message "" [])
string p@(y:ys) = (do
                x <- char y
                xs <- string ys
                return (x:xs))
                <?> p

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
newline = char '\n' <?> "newline"

newlines :: Parser String
newlines = many $ char '\n'

token :: Parser String
token = do
        spaces
        x <- getWhile1 isAlpha
        spaces
        return x
        <?> "token"
