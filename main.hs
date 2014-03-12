module Main where

import qualified Data.Map as Map
import Data.Maybe
import Data.Char
import Parser
import Debug.Trace

type RoomName = String
data RoomAction = RoomAction
    { trigger :: String
    , result :: String
    , move :: Maybe RoomName
    } deriving (Eq, Show)

data RoomFunc = RoomFunc
    { name :: RoomName
    , msg :: String
    , actions :: Triggers
    } deriving (Eq, Show)


type Adventure = Map.Map RoomName RoomFunc
type Triggers = Map.Map String RoomAction
main = do
    putStr "Please enter a story to load: "
    filename <- getLine
    contents <- readFile filename
    beginAdventure $ parseAdventure contents

parseRoom :: Parser RoomFunc
parseRoom = do
            many (noneOf "@")
            string "@"
            n <- token <?> "room name"
            spaces
            msg <- getUntil '%'
            string "%" <?> "room description"
            actions <- many parseRoomAction
            let triggers = Map.fromList (map (\a -> ( (trigger a), a )) actions)
            return (RoomFunc n msg triggers)

parseRoomAction :: Parser RoomAction
parseRoomAction = do
                    t <- token <?> "room action"
                    spaces
                    string "-"
                    spaces
                    r <- getUntil '%'
                    string "%" <?> "end of room action"
                    many $ oneOf " \t"
                    nextroom <- many $ noneOf " \t\n"
                    many $ oneOf " \t"
                    newlines
                    return (RoomAction t r (stringToMaybe nextroom))
                where
                    stringToMaybe "" = Nothing
                    stringToMaybe v = Just v

parseAdventure :: String -> Adventure
parseAdventure s = case (parse (many1 parseRoom) s) of
                Empty (Ok rooms _ _) -> Map.fromList $ map (\rm -> ((name rm), rm)) rooms
                fail -> error $ show fail

beginAdventure :: Adventure -> IO ()
beginAdventure adv = do
                putStrLn "<==\\ YOUR QUEST BEGINS /==>"
                runRoom adv rm
                where
                rm = getRoom adv "START"

runRoom :: Adventure -> RoomFunc -> IO ()
runRoom adv rm = do
                putStrLn $ msg rm
                repl adv rm

repl :: Adventure -> RoomFunc -> IO ()
repl adv rn = do
            putStr "What would you like to do? > "
            line <- getLine
            let wds = words line
            let tests = mapMaybe ((flip Map.lookup) $ (actions rn)) wds
            case tests of
                [] -> do { putStrLn "I don't understand what you mean." ; repl adv rn ; }
                [(roomact)] -> do
                    putStrLn $ result roomact
                    case move roomact of
                        Nothing -> repl adv rn
                        Just newrm -> runRoom adv $ getRoom adv newrm

getRoom :: Adventure -> RoomName -> RoomFunc
getRoom adv rn = case Map.lookup rn adv of
                Just rm -> rm
                Nothing -> error "Couldn't find room!"
