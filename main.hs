module Main where

import qualified Data.Map as Map
import Parser

type RoomName = String
data RoomAction = RoomAction
    { trigger :: String
    , result :: String
    } deriving (Eq, Show)

data RoomFunc = RoomFunc
    { name :: RoomName
    , msg :: String
    , actions :: [RoomAction]
    } deriving (Eq, Show)


type Adventure = Map.Map RoomName RoomFunc

main = do
    putStr "Please enter a story to load: "
    filename <- getLine
    contents <- readFile filename
    beginAdventure $ parseAdventure contents

parseRoom :: Parser RoomFunc
parseRoom = do
            many (noneOf "@")
            string "@"
            n <- token
            spaces
            msg <- getUntil '%'
            string "%"
            actions <- many parseRoomAction
            return (RoomFunc n msg actions)

parseRoomAction :: Parser RoomAction
parseRoomAction = do
                    t <- token
                    spaces
                    string "-"
                    spaces
                    r <- getUntil '\n'
                    newlines
                    return (RoomAction t r)

parseAdventure :: String -> Adventure
parseAdventure s = Map.fromList $ map (\rm -> ((name rm), rm)) rooms
            where
            rooms = fst $ head $ (parse (many1 parseRoom) s) :: [RoomFunc]

beginAdventure :: Adventure -> IO ()
beginAdventure adv = do
                putStrLn "<== YOUR QUEST BEGINS ==>"
                putStr "You are now in the "
                putStr $ name rm
                putStrLn " room."
                putStrLn $ msg rm
                where
                rm = getRoom adv "START"

getRoom :: Adventure -> RoomName -> RoomFunc
getRoom adv rn = case Map.lookup rn adv of
                Just rm -> rm
                Nothing -> error "Couldn't find room!"
